library(apsimx)
library(tidyverse)
library(daymetr)
library(data.table)
library(soilDB)
library(spData)
library(here)
library(parallel)
library(dplyr) # NOTE: I added this and below.
library(readr)
library(stringr)
library(lubridate)

# TODO: Make these .env or args in some way.
# Output folder (from base directory)
output_folder <- "apsimx_output"
# Path to input file (from base directory)
input_path <- file.path(output_folder, "output/input.csv")
corn_mats <- c(80, 90, 95, 100, 103, 105, 108, 110, 112, 115, 120, 130)

# ---- Set up files and load datasets ----
load_files <- function(base_dir = here()) {
  # Validate Directories
  output_dir <- file.path(base_dir, output_folder)
  if (!dir.exists(output_dir)) {
    dir.create(output_dir)
  }

  # Read in crop selection
  crop_fp <- file.path(base_dir, "selected_crop.txt")
  if (!file.exists(crop_fp)) {
    # TODO: Can I try to interpret the crop by matching the Genetics column?
    stop("Error: Select 'Maize' or 'Soy' by adding one to ./selected_crop.txt")
  }
  crop <- readLines(crop_fp)

  # Read in trials data
  trials_fp <- file.path(base_dir, input_path)
  if (!file.exists(trials_fp)) {
    stop("Error: Input file not found!")
  }
  trials_df <- read_csv(trials_fp) |>
    mutate(ID = row_number()) |>
    rename(X = .data$Longitude, Y = .data$Latitude)

  locs_df <- trials_df |>
    select(.data$X, .data$Y) |>
    distinct() |>
    mutate(ID_Loc = row_number())

  trials_df <- left_join(trials_df, locs_df, by = c("X", "Y"))

  # NOTE: is it bad to rely on writeLines to create file?
  # Set up progress log
  log_fp <- file.path(output_dir, "progress.log")
  writeLines("Progress Logs:", log_fp)
  prog_log <- file(log_fp, "a")

  # Return important variables
  return(list(
    crop = crop,
    base_dir = base_dir,
    output_dir = output_dir,
    prog_log = prog_log,
    locs_df = locs_df,
    trials_df = trials_df
  ))
}

# ---- Clean input data ----
parse_dates <- function(df) {
  df |>
    mutate(
      Year = as.numeric(str_extract(Planting, "\\b\\d{4}\\b")),
      PlantingDate = as_date(Planting),
      Year = ifelse(is.na(PlantingDate), Year, format(PlantindDate, "%Y")),
      Year = ifelse(is.na(Year), prev_year, Year),
      sim_start = if_else(
        is.na(PlantingDate),
        as_date(paste0(Year, "-01-01")),
        as_date(PlantingDate %m-% months(1))
      ),
      sim_end = if_else(
        is.na(PlantingDate),
        as_date(paste0(Year, "-12-31")),
        as_date(PlantingDate %m-% months(10))
      )
    )
}

process_soy_maturity <- function(df) {
  df |>
    mutate(
      gen1 = floor(Genetics),
      gen2 = Genetics - gen1,
      gen1 = case_when(
        gen1 >= 10 ~ "10",
        gen1 <= -2 ~ "000",
        gen1 == -1 ~ "00",
        gen1 == 0 ~ "0",
        gen1 >= 1 & Genetics <= 9 ~ as.character(gen1)
      ),
      gen2 = case_when(
        gen1 >= 8 ~ "Generic_MG",
        gen2 >= 0 & gen2 < 0.33 ~ "early",
        gen2 >= 0.33 & gen2 < 0.66 ~ "mid",
        gen2 >= 0.66 ~ "late"
      ),
      Mat = paste0(gen2, gen1) # TODO: Mat may not be the best name for maturity
    ) |>
    select(-gen1, -gen2)
}

process_maize_maturity <- function(df) {
  df |>
    mutate(
      lett = str_to_upper(str_extract(Genetics, "^[A-Za-z]")),
      num = as.numeric(str_extract(Genetics, "\\d+")),
      lett = ifelse(is.na(lett), "B", lett)
    ) |>
    rowwise() |>
    mutate(num = corn_mats[which.min(abs(corn_mats - num))[1]]) |>
    ungroup() |>
    mutate(Mat = paste0(lett, "_", as.character(num))) |>
    select(-lett, -num)
}

# ---- Collect and Write Meteorological, Soil and APSIM Files ----
# TODO: Rename this function (get -> make) or something
get_locyear <- function(df) {
  df |>
    select(X, Y, ID_Loc, sim_start) |>
    mutate(first_year = year(sim_start)) |>
    select(-sim_start) |>
    unique() |>
    group_by(ID_Loc, X, Y) |>
    summarize(first_year = min(first_year)) |>
    mutate(first_year = min(first_year, prev_year - 10), last_year = prev_year)
}

# TODO: Should I provide default values for some of these (dirs, cluster, crop,)
parallel_write_met_data <- function(locyear_df, output_dir, cluster) {
  # Clear the output folder
  unlink(output_dir, recursive = TRUE)
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  # Export shared vars for cluster
  # NOTE: should environment() be there at all?
  clusterExport(cluster, envir = environment(), varlist = c(
    "locyear_df", "getdaymet2_apsim_met", "napad_apsim_met",
    "impute_apsim_met", "write_apsim_met"
  ))

  parLapply(cluster, seq_len(nrow(locyear_df)), function(idx) {
    row <- locyear_df[idx, ]
    try({
      met_tmp <- get_daymet2_apsim_met(
        lonlat = c(row$X, row$Y),
        years = c(as.integer(row$first_year), as.integer(row$last_year)),
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
      # NOTE: Why Not imp_met_tmp$site <- met_tmp$site?
      attr(imp_met_tmp, "site") <- attr(met_tmp, "site")
      attr(imp_met_tmp, "latitude") <- attr(met_tmp, "latitude")
      attr(imp_met_tmp, "longitude") <- attr(met_tmp, "longitude")
      write_apsim_met(
        imp_met_tmp, output_dir, paste0("loc_", row$ID_Loc, ".met")
      )
    }, error = function(e) {
      message(paste("Error processing location", row$ID_Loc, ":", e))
    })
  })
}

# TODO: Seems like a good canidate for parallelization, already have cluster
write_soil_data <- function(locs_df, output_dir) {
  # Clear the output folder
  unlink(output_dir, recursive = TRUE)
  dir.create(output_dir)

  soil_profile_list <- list()
  locs_df$got_soil <- NA
  ids_needs_soil <- locs_df[
    locs_df$got_soil == FALSE | is.na(locs_df$got_soil)
  ]$ID_Loc

  for (id in ids_needs_soil) {
    row <- locs_df[locs_df$ID_Loc == id, ]
    tryCatch({
      soil_profile <- tryCatch(
        get_ssurgo_soil_profile(lonlat = c(row$X, row$Y), fix = TRUE)[[1]],
        error = function(e) {
          # NOTE: I added the super assignment, hope it's needed.
          soil_profile <<- get_isric_soil_profile(lonlat = c(row$X, row$Y),
                                                  fix = TRUE)
        }
      )
      # Extract the apsim SoilWat params from the soil profile
      horizon <- soil_profile$soil
      soilwat <- soilwat_parms()
      PO <- 1 - horizon$BD / 2.65
      soilwat$SWCON <- max(0, (PO - horizon$DUL) / PO)
      soilwat$Thickness <- horizon$Thickness
      soil_profile$soilwat <- soilwat

      # Get Initial Water conditions
      initwat <- initialwater_parms() # Set initial water to reasonable values
      initwat$InitialValues <- horizon$DUL
      initwat$Thickness <- horizon$Thickness
      soil_profile$initialwater <- initwat

      # Get Soil Carbon
      oc_min <- 0.001 # Minimum Soil Carbon Content
      soil_profile[["soil"]]$Carbon <- max(oc_min, horizon$Carbon)

      # TODO: Add .rds to the output fp?
      output_fp <- file.path(output_dir, paste0("soil_profile_", id))
      write_rds(soil_profile, file = output_fp)
      soil_profile_list[[as.character(id)]] <- soil_profile
      locs_df[locs_df$ID_Loc == id, "got_soil"] <- TRUE
      perc_ids <- which(ids_needs_soil == id) / length(ids_needs_soil)
      print(paste0("loc: ", id, "   ", round(perc_ids, 4)))
    }, error = function(e) {
      locs_df[locs_df$ID_Loc == id, "got_soil"] <<- FALSE
      print(paste0("loc: ", id, "   ", round(perc_ids, 4), "  FAIL"))
    })
  }
  write_rds(soil_profile_list, file.path(output_dir, "soil_profile_list.rds"))
  return(soil_profile_list)
}

parallel_write_apsim_files <- function(
  trial_df, soil_profiles, crop, output_dir, met_dir, cluster
) {
  # Clear the output folder
  unlink(output_dir, recursive = TRUE)
  dir.create(output_dir)
  # Export shared vars for cluster
  clusterExport(cluster, c(
    "df", "output_dir", "met_dir", "crop", "soil_profile_list",
    "edit_apsimx", "edit_apsimx_replace_soil_profile",
    "paste0", "dir.create", "file.copy", "file.path", "tryCatch", "print"
  ))

  parLapply(cluster, seq_len(nrow(trial_df)), function(trial_n) {
    row <- trial_df[trial_n, ]
    trial_dir <- file.path(output_dir, paste0("trial_", trial_n))
    if (!dir.exists(trial_dir)) {
      dir.create(trial_dir)
    }
    filename <- paste0(crop, "_", trial_n, ".apsimx")

    # Clock Start, Clock End, and Weather (met file)
    edit_apsimx(
      file = paste0(crop, "_.apsimx"),
      wrt.dir = trial_dir,
      edit.tag = trial_n,
      node = "Clock",
      parm = "Start",
      value = paste0(row$sim_start, "T00:00:00"),
      verbose = FALSE
    )
    edit_apsimx(
      file = filename,
      src.dir = trial_dir,
      wrt.dir = trial_dir,
      overwrite = TRUE,
      node = "Clock",
      parm = "End",
      value = paste0(row$sim_end, "T00:00:00"),
      verbose = FALSE
    )
    edit_apsimx(
      file = filename,
      src.dir = trial_dir,
      wrt.dir = trial_dir,
      overwrite = TRUE,
      node = "Weather",
      value = file.path(met_dir, paste0("loc_", row$ID_Loc, ".met")),
      verbose = FALSE
    )

    sow_date <- ifelse(
      is.na(row$PlantingDate),
      as.character(format(row$PlantingDate, "%d-%b")),
      "NA"
    )
    # TODO: These are pretty similar, can I combine them?:
    edit_apsimx(
      file = filename,
      src.dir = trial_dir,
      wrt.dir = trial_dir,
      overwrite = TRUE,
      node = "Manager",
      manager.child = "Sowing",
      parm = "SowDate",
      value = sow_date,
      verbose = FALSE
    )
    edit_apsimx(
      file = filename,
      src.dir = trial_dir,
      wrt.dir = trial_dir,
      overwrite = TRUE,
      node = "Crop",
      parm = "SowDate",
      value = sow_date,
      verbose = FALSE
    )
    edit_apsimx(
      file = filename,
      src.dir = trial_dir,
      wrt.dir = trial_dir,
      overwrite = TRUE,
      node = "Crop",
      parm = "CultivarName",
      value = row$Mat,
      verbose = FALSE
    )
    tryCatch({
      edit_apsimx_replace_soil_profile(
        file = filename,
        src.dir = trial_dir,
        wrt.dir = trial_dir,
        overwrite = TRUE,
        soil.profile = soil_profiles[[as.character(row$ID_Loc)]],
        verbose = FALSE
      )
    }, error = function(e) { })
    invisible()
  })
}

# ---- Driver Function ----
run_apsimx <- function() {
  Sys.setlocale("LC_ALL", "English_United States")
  start_time <- Sys.time()
  files <- load_files()
  df <- files$trials_df
  locs_df <- files$locs_df
  locyear_df <- get_locyear(df)
  # Create a CPU cluster (cl) for parallelization
  no_cores <- detectCores() - 2
  cl <- makeCluster(no_cores)

  #### Clean Input Data ####
  writeLines("Parse Dates", files$prog_log)
  df <- parse_dates(df)

  writeLines("Set Maturities", files$prog_log)
  if (files$crop == "Soy") {
    df <- process_soy_maturity(df)
  } else if (files$crop == "Maize") {
    df <- process_maize_maturity(df)
  }

  #### Write Meteorological, Soil, and APSIM Files ####
  writeLines("Collect Meteorological Data")
  met_dir <- file.path(files$output_dir, "met")
  parallel_write_met_data(cl, locyear_df, met_dir)

  writeLines("Collect Soil Data")
  soil_dir <- file.path(files$output_dir, "soils")
  soil_profiles <- write_soil_data(locs_df, soil_dir)

  writeLines("Create APSIM Files")
  apsim_dir <- file.path(files$output_dir, "apsim")
  crop_template_fp <- file.path(
    files$base_dir, "template_models", paste0(files$crop, "_Template.apsimx")
  )
  file.copy(
    from = crop_template_fp,
    to = file.path(files$output_dir, paste0(files$crop, "_.apsimx")),
    overwrite = TRUE
  )
  parallel_write_apsim_files(
    trial_df = df,
    soil_profiles = soil_profiles,
    crop = files$crop,
    output_dir = apsim_dir,
    met_dir = met_dir,
    cluster = cl
  )

  #### Run APSIM ####

  #### Summarize Results ####

  #### Cleanup Step ####
  stopCluster(cl)
  end_time  <- Sys.time()
  print(paste("Runtime:", end_time - start_time))
  close(files$prog_log)
}







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
