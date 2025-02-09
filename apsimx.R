
# Start, set up trials_df -----
# TODO: This file should probably be divided into a series of functions.
#   Then, there will be a run function to call them and track progress.

library(apsimx)
library(tidyverse)
library(daymetr)
library(data.table)
library(soilDB)
library(spData)
library(here)
library(parallel)  # For parallel computing
Sys.setlocale("LC_ALL", "English_United States")

# Set up files and df: ----
start_time <- Sys.time() # track running time

# FIXME: this is a really terrible way of setting the directory.
# There should be an .env file or something similar.
codes_dir <- here() # Folder with the codes
setwd(paste0(codes_dir, "/apsimx_output")) #folder where the output goes

# TODO: since setwd: readLines('selected_crop.txt') ?
crop <- readLines("C:/Users/cmg3/Documents/GitHub/SCT/selected_crop.txt")

# TODO: why even do setwd if you are going to do this?
trials_df <- read_csv(paste0(codes_dir, "/apsimx_output/output/input.csv"))
trials_df <- mutate(trials_df, ID = row_number()) %>%
  rename(X = Longitude, Y = Latitude)

locs_df <- select(trials_df, X, Y) %>%
  distinct() %>%
  mutate(ID_Loc = row_number())

trials_df <- left_join(trials_df, locs_df)

#set up progress log
writeLines("Progress Log:", "progress.log") #Create file
prog_log <- file("progress.log", "a")    #Open connection to append

# Step 1: Clean and parse dates from input data: ----
# Require year as part of the input
writeLines("Parse dates", prog_log)

prev_year <- as.numeric(substr(Sys.time(), 1, 4)) - 1
# TODO: Generally Supressing warnings is bad unless there is a good reason, why?
trials_df <- suppressWarnings(
  mutate(trials_df, Year = as.numeric(str_extract(Planting, "\\b\\d{4}\\b")))
)
trials_df <- suppressWarnings(
  mutate(trials_df, PlantingDate = as_date(Planting))
)
trials_df <- mutate(
  trials_df,
  Year = ifelse(is.na(PlantingDate), Year, format(PlantingDate, "%Y")),
  # If year is missing, use last year with full data.
  Year = ifelse(is.na(Year), prev_year, Year),
  # If PlantingDate is misisng, use beginning and end of year as bounds.
  sim_start = if_else(
    is.na(PlantingDate),
    as_date(paste0(as.character(Year), "-01-01")),
    as_date(PlantingDate %m-% months(1))
  ),
  sim_end = if_else(
    is.na(PlantingDate),
    as_date(paste0(as.character(Year), "-12-31")),
    as_date(PlantingDate %m+% months(10))
  )
)

# Step 2: Clean and parse the Genetics column: ----
# Get what maturities of cultivar we'll use
writeLines("Set maturities", prog_log)

if (crop == "Soy") {
  trials_df <- trials_df %>%
    mutate(gen1 = floor(Genetics), gen2 = Genetics - gen1) %>%
    mutate(gen1 = case_when(
      gen1 >= 10 ~ "10",
      gen1 <= -2 ~ "000",
      gen1 == -1 ~ "00",
      gen1 == 0 ~ "0",
      gen1 >= 1 & Genetics <= 9 ~ as.character(gen1)
    )) %>%
    mutate(gen2 = case_when(
      gen1 >= 8 ~ "Generic_MG",
      gen2 >= 0 & gen2 < 0.33 ~ "early",
      gen2 >= 0.33 & gen2 < 0.66 ~ "mid",
      gen2 >= 0.66 ~ "late"
    )) %>%
    mutate(Mat = paste0(gen2, gen1)) %>%
    select(-gen1, -gen2)
}

if (crop == "Maize") {
  trials_df <- trials_df %>% mutate(
    lett = str_to_upper(str_extract(Genetics, "^[A-Za-z]")),
    num = as.numeric(str_extract(Genetics, "\\d+"))
  )
  # TODO: A good number of these variables should be defined as consts at top
  corn_mats <- c(80, 90, 95, 100, 103, 105, 108, 110, 112, 115, 120, 130)
  trials_df <- trials_df %>% mutate(ifelse(is.na(lett), "B", lett))
  trials_df <- trials_df %>%
    rowwise() %>%
    mutate(num = corn_mats[which.min(abs(corn_mats - num))[1]]) %>%
    mutate(Mat = paste0(lett, "_", as.character(num)))
  trials_df <- select(trials_df, -lett, -num)
  # TODO: could all of the above be performed in a single pipe?
}

# Get weather, make met files -----
# TODO: This seems to be a new section, but no progress logs here

# For each location, collect weather data for years from minimum
# (first requested year, ten years before now) to most recent full year
locyear_df <- trials_df %>%
  select(X, Y, ID_Loc, sim_start) %>%
  mutate(first_year = year(sim_start)) %>%
  select(-sim_start) %>%
  unique() %>%
  group_by(ID_Loc, X, Y) %>%
  summarize(first_year = min(first_year)) %>%
  mutate(first_year = min(first_year, prev_year - 10), last_year = prev_year)

# TODO: Is there any reason to use the ;? why not just a new line?
unlink("met", recursive = TRUE) ; dir.create("met")

# Setup for parallel processing
no_cores <- detectCores() - 2  # Reserve 2 cores for the system
cl <- makeCluster(no_cores)
# NOTE: I changed the variable order for brevity.
clusterExport(cl, envir = environment(), varlist = c(
  "locyear_df",
  "get_daymet2_apsim_met",
  "napad_apsim_met",
  "impute_apsim_met",
  "write_apsim_met"
))


# Ensure the directory exists for weather data
# TODO: Should this check for existance before calling create?
dir.create("met", recursive = TRUE, showWarnings = FALSE)

# NOTE: Not sure what this is doing, can it be simplified?
parLapply(cl, seq_len(nrow(locyear_df)), function(idx) {
  locyear_tmp <- locyear_df[idx, ]
  try({
    met_tmp <- get_daymet2_apsim_met(
      lonlat = c(locyear_tmp$X, locyear_tmp$Y),
      years = c(
        as.integer(locyear_tmp$first_year),
        as.integer(locyear_tmp$last_year)
      ),
      silent = TRUE
    )
    na_met_tmp <- tryCatch(
      napad_apsim_met(met_tmp),
      error = function(e) met_tmp
    )
    imp_met_tmp <- tryCatch(
      impute_apsim_met(na_met_tmp),
      warning = function(w) na_met_tmp
    )
    attr(imp_met_tmp, "site") <- attr(met_tmp, "site")
    attr(imp_met_tmp, "latitude") <- attr(met_tmp, "latitude")
    attr(imp_met_tmp, "longitude") <- attr(met_tmp, "longitude")
    write_apsim_met(
      imp_met_tmp,
      wrt.dir = "met",
      paste0("loc_", locyear_tmp$ID_Loc, ".met")
    )
  })
})


# Get soil, make soil file -----
# TODO: This seems like another section, should it be a separate progress log?
# TODO: Why is = being used for assignment?
soil_profile_list = list()
# TODO: Why is ; being used instead of newline?
unlink("soils", recursive = TRUE) ; dir.create("soils")
locs_df$got_soil <- NA

# NOTE: is this the best way to break up this line?
ids_needs_soil <- locs_df[
  locs_df$got_soil == FALSE | is.na(locs_df$got_soil),
]$ID_Loc

for (id in ids_needs_soil) {
  locs_tmp <- locs_df[locs_df$ID_Loc == id, ]
  tryCatch({
    soil_profile_tmp <- tryCatch(
      get_ssurgo_soil_profile(lonlat = c(locs_tmp$X, locs_tmp$Y), fix = TRUE),
      error = function(e) {
        soil_profile_tmp <- list(
          get_isric_soil_profile(lonlat = c(locs_tmp$X, locs_tmp$Y), fix = TRUE)
        )
      }
    )

    # NOTE: What does this block do?
    horizon <- soil_profile_tmp[[1]]$soil
    soilwat <- soilwat_parms() #creating SWCON in SoilWater parameters
    PO <- 1 - horizon$BD / 2.65
    soilwat$SWCON <- (PO - horizon$DUL) / PO
    soilwat$SWCON <- ifelse(soilwat$SWCON < 0, 0, soilwat$SWCON)
    soilwat$Thickness <- horizon$Thickness
    soil_profile_tmp[[1]]$soilwat <- soilwat

    # NOTE: What does this block do?
    initwat <- initialwater_parms() # Set initial water to reasonable values
    initwat$InitialValues <- horizon$DUL
    initwat$Thickness <- horizon$Thickness
    soil_profile_tmp[[1]]$initialwater <- initwat

    oc_min <- 0.001 #set minimum carbon content in soils # NOTE: global var?
    given_oc <- soil_profile_tmp[[1]][["soil"]]$Carbon
    soil_profile_tmp[[1]][["soil"]]$Carbon <- ifelse(
      given_oc < oc_min, oc_min, given_oc
    )

    write_rds(soil_profile_tmp, file = paste0("soils/soil_profile_", id))
    soil_profile_list[[as.character(id)]] <- soil_profile_tmp[[1]]
    locs_df[locs_df$ID_Loc == id, "got_soil"] <- TRUE
    print(paste0(
      "loc: ", id, "   ",
      round(which(ids_needs_soil == id) / length(ids_needs_soil), 4)
    ))
  }, error = function(e) {
    locs_df[locs_df$ID_Loc == id, "got_soil"] <<- FALSE
    print(paste0(
      "loc: ", id, "   ",
      round(which(ids_needs_soil == id) / length(ids_needs_soil), 4),
      "  FAIL"
    ))
  })
}
write_rds(soil_profile_list, "soils/soil_profile_list.rds")


# Create APSIM files -----
# TODO: This seems like another step that could be included in the progress log?
unlink("apsim", recursive = TRUE)
dir.create("apsim")
file.copy(
  from = paste0(codes_dir, "/template_models/", crop, "_Template.apsimx"),
  to = paste0(crop, "_.apsimx"), overwrite = TRUE
)

# Prepare for parallel processing
# NOTE: Should give each one it's own line, but no one is reading them anyway
clusterExport(cl, c(
  "trials_df", "codes_dir", "crop", "edit_apsimx",
  "edit_apsimx_replace_soil_profile", "paste0", "dir.create",
  "file.copy", "tryCatch", "print"
))

# Parallel APSIM files creation
# TODO: Replace `1:nrow(trials_df)` with `seq_len(nrow(trials_df))`
# TODO: This really feels like the place for a for loop
apsimxfilecreate <- parLapply(cl, 1:nrow(trials_df), function(trial_n) {
  trial_tmp <- trials_df[trial_n, ]
  if (!dir.exists(paste0("apsim/trial_", trial_n))) {
    dir.create(paste0("apsim/trial_", trial_n))
  }
  source_dir <- paste0("apsim/trial_", trial_n)
  write_dir <-  paste0("apsim/trial_", trial_n)
  filename <- paste0(crop, "_", trial_n, ".apsimx")

  edit_apsimx(
    file = paste0(crop, "_.apsimx"),
    wrt.dir = write_dir,
    edit.tag = trial_n,
    node = "Clock",
    parm = "Start",
    value = paste0(trial_tmp$sim_start, "T00:00:00"),
    verbose = FALSE
  )
  edit_apsimx(
    file = filename,
    src.dir = source_dir,
    wrt.dir = write_dir,
    overwrite = TRUE,
    node = "Clock",
    parm = "End",
    value = paste0(trial_tmp$sim_end, "T00:00:00"),
    verbose = FALSE
  )
  edit_apsimx(
    file = filename,
    src.dir = source_dir,
    wrt.dir = write_dir,
    overwrite = TRUE,
    node = "Weather",
    value = paste0(getwd(), "/met/loc_", trial_tmp$ID_Loc, ".met"),
    verbose = FALSE
  )

  if (is.na(trial_tmp$PlantingDate)) {
    edit_apsimx(
      filename,
      src.dir = source_dir,
      wrt.dir = write_dir,
      overwrite = TRUE,
      node = "Manager",
      manager.child = "Sowing",
      parm = "SowDate",
      value = "NA",
      verbose = FALSE
    )
    edit_apsimx(
      filename,
      src.dir = source_dir,
      wrt.dir = write_dir,
      overwrite = TRUE,
      node = "Crop",
      parm = "SowDate",
      value = "NA",
      verbose = FALSE
    )
  } else {
    edit_apsimx(
      filename,
      src.dir = source_dir,
      wrt.dir = write_dir,
      overwrite = TRUE,
      node = "Manager",
      manager.child = "Sowing",
      parm = "SowDate",
      value = as.character(format(trial_tmp$PlantingDate, "%d-%b")),
      verbose = FALSE
    )
    edit_apsimx(
      filename,
      src.dir = source_dir,
      wrt.dir = write_dir,
      overwrite = TRUE,
      node = "Crop",
      parm = "SowDate",
      value = as.character(format(trial_tmp$PlantingDate, "%d-%b")),
      verbose = FALSE
    )
  }
  edit_apsimx(
    filename,
    src.dir = source_dir,
    wrt.dir = write_dir,
    overwrite = TRUE,
    node = "Crop",
    parm = "CultivarName",
    value = trial_tmp$Mat,
    verbose = FALSE
  )
  tryCatch({
    edit_apsimx_replace_soil_profile(
      file = filename,
      src.dir = source_dir,
      wrt.dir = write_dir,
      overwrite = TRUE,
      soil.profile = soil_profile_list[[as.character(trial_tmp$ID_Loc)]],
      verbose = FALSE
    )
  }, error = function(e) { })
  invisible()
})

# Run APSIM files -----
# TODO: This also seems like a good place for the log file?
#   Honestly, I'm not even sure what its doing at this point?

# Define the number of batches
# TODO: This should probably be defined in an easy-to-edit location
num_batches <- 10  # You can change this to run different percentages at a time

# Calculate the number of trials per batch
batch_size <- ceiling(nrow(trials_df) / num_batches)


clusterExport(cl, c(
  "trials_df", "codes_dir", "crop", "edit_apsimx",
  "edit_apsimx_replace_soil_profile", "paste0", "dir.create", "file.copy",
  "tryCatch", "print", "apsimx", "mutate", "write_csv", "soil_profile_list"
))

# Initialize a list to hold results from all batches
all_results <- list()

# Process each batch
for (batch in 1:num_batches) {
  # Determine the rows for the current batch
  # NOTE: This doesn't look the greatest, but not sure another way to shorten.
  batch_rows <-
    ((batch - 1) * batch_size + 1):min(nrow(trials_df), batch * batch_size)
  batch_trials <- trials_df[batch_rows, ]

  # Split trials for parallel execution
  # TODO: Replace seq(...) with seq_len(...)
  trial_list <- split(batch_trials, seq(nrow(batch_trials)))

  # Run APSIM simulations in parallel for the current batch
  # Run APSIM simulations in parallel
  results <- parLapply(cl, trial_list, function(trial) {
    trial_n <- trial$ID  # Assuming 'ID' is the identifier
    source_dir <- paste0("apsim/trial_", trial_n)
    filename <- paste0(crop, "_", trial_n, ".apsimx")
    output <- data.frame()  # Initialize an empty data frame for the results

    # Wrap APSIM simulation and result handling in tryCatch to handle any errors
    tryCatch({
      output_tmp <- apsimx(filename, src.dir = source_dir)
      output_tmp <- mutate(output_tmp, "ID" = trial_n)
      # Append the output of this trial to the overall results
      output <- rbind(output, output_tmp)
      # Save individual trial results
      write_csv(output_tmp,
                file = paste0(source_dir, "/", crop, "_", trial_n, "_out.csv"))
      return(output)  # Return the output for this trial
    }, error = function(e) {
      cat(paste0("Simulation for trial ", trial_n,
                 " failed with error: ", e$message, "\n"))
      return(NULL)  # Return NULL if there was an error
    })
  })

  # Combine the results from this batch and add to the all_results list
  batch_results <- do.call(rbind, results)
  all_results[[batch]] <- batch_results

  # Print out the progress
  cat(sprintf("Completed batch %d out of %d (%.2f%%)\n",
              batch, num_batches, 100 * batch / num_batches))
}


# Stop the cluster
stopCluster(cl)


# Summarize Results -----

# Merge Outputs
outfiles <- list.files("apsim/", pattern = "_out", recursive = TRUE)
daily_output <- data.table::rbindlist(
  lapply(outfiles, function(x) {
    read_csv(paste0("apsim/", x), show_col_types = FALSE)
  }),
  use.names = TRUE
)
daily_output <- select(
  daily_output, -CheckpointID, -SimulationID, -SimulationName, -Zone, -Year
) %>% arrange(ID)

# Get simulated sowing and harvest dates
simsows <- select(daily_output, ID, SimSowDate) %>% filter(!is.na(SimSowDate))
simmats <- select(daily_output, ID, SimMatDate) %>% filter(!is.na(SimMatDate))
simharvs <- select(daily_output, ID, SimHarvestDate) %>%
  filter(!is.na(SimHarvestDate))
simdates <- left_join(simsows, simmats) %>% left_join(simharvs)
daily_output <- select(daily_output, -SimSowDate, -SimMatDate, -SimHarvestDate)

# Trim season (daily_output) to 1 month before planting and after death/harvest
simdates <- simdates %>%
  mutate(
    StartDate = date(SimSowDate) %m-% weeks(2),
    EndDate = date(SimHarvestDate) %m+% weeks(2)
  ) %>%
  select(ID, StartDate, SimSowDate, SimMatDate, SimHarvestDate, EndDate)

daily_output <- group_by(daily_output, ID) %>%
  left_join(select(simdates, ID, StartDate, EndDate)) %>%
  filter(Date >= StartDate & Date <= EndDate) %>%
  select(-StartDate, -EndDate)

# Create trials_x from trial-specific information
# NOTE: I haven't always been the most consistent with whether to sep pipe or fn
#   Generally decide based on which is the part that is taking up a lot of space
yields <- group_by(daily_output, ID) %>% summarize(
  Yield_Sim = max(Yieldkgha),
  MaxStage = max(Stage)
)
res <- group_by(daily_output, ID) %>%
  filter(!is.na(Result)) %>%
  select(ID, Result)

trials_x <- rename(trials_df, Latitude = Y, Longitude = X)
trials_x <- trials_x %>%
  select(-sim_start, -sim_end) %>%
  left_join(yields) %>%
  left_join(simdates) %>%
  left_join(res)
# TODO: Could all of these be part of the above pipe?:
trials_x <- mutate(trials_x, DTM_Sim = as.numeric(SimMatDate - SimSowDate)) %>%
  relocate(DTM_Sim, .after = SimSowDate)
trials_x <- rename(
  trials_x,
  MatDate_Sim = SimMatDate,
  PlantingDate_Sim = SimSowDate,
  HarvestDate_Sim = SimHarvestDate
)
trials_x <- select(trials_x, -PlantingDate)
trials_x <- relocate(trials_x, ID)

# Periods
if (crop %in% c("Soy", "Maize")) {
  max_stage <- 11
} else {
  max_stage <- max(daily_output$Stage)
}

daily_output <- daily_output %>%
  left_join(select(trials_x, ID, HarvestDate_Sim, PlantingDate_Sim)) %>%
  mutate(Period = case_when(
    Stage == 1 & (as_date(Date) < PlantingDate_Sim) ~ 1,
    Stage == 1 & (as_date(Date) >= HarvestDate_Sim) ~ max_stage,
    .default = floor(Stage)
  )) %>%
  select(-PlantingDate_Sim, -HarvestDate_Sim) %>%
  mutate(
    Period = factor(Period, ordered = TRUE, levels = as.character(1:max_stage))
  )

# Add cumulative precipitation and thermal time
daily_output <- daily_output %>%
  group_by(ID) %>%
  mutate(AccRain = cumsum(Rain), AccTT = cumsum(ThermalTime))

# daily_output <- daily_output %>%
#   left_join(select(trials_x, ID, MatDate_Sim, Planting)) %>%
#   mutate(Stage = case_match(
#     Period,
#     "1" ~ "Pre-planting", #germinating
#     "2" ~ "VE", #emerging
#     "3" ~ "V(n)", #vegetative
#     "4" ~ "R1", #early flowering
#     "5" ~ "R3", #early pod development
#     "6" ~ "R5 early", #early grain filling
#     "7" ~ "R5 mid", #mid grain filing
#     "8" ~ "R5 late", #late grain filling
#     "9" ~ "R6", #maturing
#     "10" ~ "R7", #ripening
#     "11" ~ "R8 & Post-harvest", #harvestripe + germinating
#   )) %>%
#   select(-MatDate_Sim) %>%
#   mutate(Period = factor(Period, ordered = T, levels = as.character(1:11)))

charact_x <- daily_output %>%
  group_by(Period, ID) %>%
  select(-Yieldkgha, -Stage) %>%
  summarize(
    across(where(is.numeric) & !c(DOY, AccRain, AccTT, AccEmTT), function(x) {
      mean(x, na.omit = TRUE)
    }),
    AccRain = sum(Rain), AccTT = sum(ThermalTime), AccEmTT = max(AccEmTT),
    Period_Start_Date = min(Date), Period_End_Date = max(Date)
  ) %>%
  mutate(
    Duration = as.numeric(
      as.period(Period_End_Date - Period_Start_Date, "days")
    ) / 86400,
    Period_Start_DOY = yday(Period_Start_Date),
    Period_End_DOY = yday(Period_End_Date)
  ) %>%
  relocate(ID, Period, Rain) %>%
  relocate(AccRain, .after = Rain) %>%
  relocate(AccTT, AccEmTT, .after = ThermalTime) %>%
  relocate(Period_Start_DOY, Duration, Period_End_DOY, .after = last_col()) %>%
  arrange(ID)

daily_charact_x <- daily_output

# TODO: Why use ; instead of  newline?
unlink("output",recursive = TRUE) ; dir.create("output")
write_csv(trials_x, "output/trials_x.csv")
write_csv(charact_x, "output/charact_x.csv")
write_csv(daily_charact_x, "output/daily_charact_x.csv")

final_x <- pivot_wider(
  charact_x,
  names_from = Period,
  values_from = Rain:Period_End_DOY
) %>% right_join(trials_x, .)

write_csv(final_x, "output/final_x.csv")

#calculate time duration for running the code:
end_time <- Sys.time()
duration <- end_time - start_time
print(duration)

close(prog_log)
