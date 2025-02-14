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

# TODO: Rename df args
dotenv::load_dot_env(".env")

# TODO: Make these .env or args in some way.
# Output folder (from base directory)
output_folder <- "apsimx_output"
# Path to input file (from base directory)
input_path <- file.path(output_folder, "output/input.csv")
apsim_batches <- 10 # You can change this to run different percentages at a time

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
  corn_mats <- c(80, 90, 95, 100, 103, 105, 108, 110, 112, 115, 120, 130)
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

make_locyear <- function(df) {
  df |>
    select(X, Y, ID_Loc, sim_start) |>
    mutate(first_year = year(sim_start)) |>
    select(-sim_start) |>
    unique() |>
    group_by(ID_Loc, X, Y) |>
    summarize(first_year = min(first_year)) |>
    mutate(first_year = min(first_year, prev_year - 10), last_year = prev_year)
}

# ---- Collect and Write Meteorological, Soil and APSIM Files ----
parallel_write_met_data <- function(locyear_df, output_dir, cl) {
  # Clear the output folder
  unlink(output_dir, recursive = TRUE)
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  # Export shared vars for cluster
  # NOTE: should environment() be there at all?
  clusterExport(cl, envir = environment(), varlist = c(
    "locyear_df", "get_daymet2_apsim_met", "napad_apsim_met",
    "impute_apsim_met", "write_apsim_met"
  ))

  parLapply(cl, seq_len(nrow(locyear_df)), function(idx) {
    row <- locyear_df[idx, ]
    filename <- paste0("loc_", row$ID_Loc, ".met")
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
      write_apsim_met(imp_met_tmp, output_dir, filename)
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

parallel_apsim_files <- function(df, soils, crop, output_dir, met_dir, cl) {
  # Clear the output folder
  unlink(output_dir, recursive = TRUE)
  dir.create(output_dir)
  # Export shared vars for cluster
  clusterExport(cl, c(
    "df", "soils", "crop", "output_dir", "met_dir",
    "edit_apsimx", "edit_apsimx_replace_soil_profile"
  ))

  parLapply(cl, seq_len(nrow(df)), function(trial_n) {
    row <- df[trial_n, ]
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

    sow_date <- ifelse(is.na(row$PlantingDate), "NA",
                       as.character(format(row$PlantingDate, "%d-%b")))
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
        soil.profile = soils[[as.character(row$ID_Loc)]],
        verbose = FALSE
      )
    }, error = function(e) { })
    invisible()
  })
}

# ---- Run Apsim ----
parallel_run_sim <- function(df, apsim_dir, crop, cluster, log_file) {
  all_results <- list()
  # Export shared vars for cluster
  clusterExport(cluster, c(
    "df", "apsim_dir", "crop", "apsimx", "mutate", "write_csv", "num_batches"
  ))

  # NOTE: parallelization is applied to each batch separately
  bsize <- ceiling(nrow(df) / apsim_batches)
  for (b_ix in 1:apsim_batches) {
    batch <- df[((b_ix - 1) * bsize + 1):min(nrow(df), b_ix * bsize), ]
    trial_rows <- split(batch, seq_len(nrow(batch))) # NOTE: Why seq_len(nrow)?

    results <- parLapply(cluster, trial_rows, function(trial) {
      trial_n  <- trial$ID # Assumes "ID" is the identifier
      source_dir <- file.path(apsim_dir, paste0("trial_", trial_n))
      filename <- paste0(crop, "_", trial_n, ".apsimx")
      out_fp <- file.path(source_dir, paste0(crop, "_", trial_n, "_out.csv"))

      # Run Simulation and catch errors:
      tryCatch({
        output <- apsimx(filename, src.dir = source_dir) |>
          mutate("ID" = trial_n)
        write_csv(output, file = out_fp)
        return(output)
      }, error = function(e) {
        cat(paste("Sim for trial", trial_n, "failed. Error:", e$message, "\n"))
        return(NULL)
      })
    })
    all_results[[batch]] <- do.call(rbind, results)
    progress_msg <- sprintf(
      "Batch %d/%d (%.2f%%) Complete.",
      b_ix, apsim_batches, 100 * (b_ix / apsim_batches)
    )
    cat(paste0(progress_msg, "\n"))
    writeLines(log_file, log_file)
  }
  return(all_results)
}

# ---- Summarize Results ----
process_results <- function(apsim_dir, crop) {
  # NOTE: added full.names = TRUE
  # TODO: Do I need to load all the files or could I just use the sim output?
  # Find and load all _out files from sim
  outfiles <- list.files(apsim_dir, "_out", recursive = TRUE, full.names = TRUE)
  # NOTE: Add fill = TRUE if I am not 100% certain all _out files have same cols
  sim_out <- rbindlist(lapply(outfiles, fread), use.names = TRUE) |>
    select(-CheckpointID, -SimulationID, -SimulationName, -Zone, -Year) |>
    arrange(ID)

  # Get the Start and End Date from daily_output
  simdates <- sim_out |>
    select(ID, SimSowDate, SimMatDate, SimHarvestDate) |>
    filter(rowSums(!is.na(.) > 1)) |>
    mutate(
      StartDate = as.Date(SimSowDate) - weeks(2),
      EndDate = as.Date(SimHarvestDate) + weeks(2)
    )

  # Filter daily_output by StartDate <= Date <= EndDate
  sim_out <- sim_out |>
    left_join(select(simdates, ID, StartDate, EndDate), by = "ID") |>
    filter(Date >= StartDate & Date <= EndDate) |>
    select(-StartDate, -EndDate, -SimSowDate, -SimMatDate, -SimHarvestDate)

  # Comment?
  yields <- group_by(sim_out, ID) |>
    summarize(Yield_Sim = max(Yieldkgha), MaxStage = max(Stage))

  # Comment?
  res <- group_by(sim_out, ID) |> filter(!is.na(Result)) |> select(ID, Result)

  # Add Comment Here
  trials_x <- trials_df |>
    rename(Latitude = Y, Longitude = X) |>
    select(-sim_start, -sim_end, -PlantingDate) |>
    left_join(yields, by = "ID") |>
    left_join(simdates, by = "ID") |>
    left_join(res, by = "ID") |>
    mutate(DTM_Sim = as.numeric(SimMatDate - SimSowDate)) |>
    rename(
      MatDate_Sim = SimMatDate,
      PlantingDate_Sim = SimSowDate,
      HarvestDate_Sim = SimHarvestDate
    ) |>
    relocate(ID) |>
    relocate(DTM_Sim, .after = SimSowDate)

  maxstage <- ifelse(crop %in% c("Soy", "Maize"), 11, max(sim_out$Stage))

  # Add Comment Here
  sim_out <- sim_out |>
    left_join(select(trials_x, ID, HarvestDate_Sim, PlantingDate_Sim)) |>
    mutate(
      Period = case_when(
        Stage == 1 & (as_date(Date) < PlantingDate_Sim) ~ 1,
        Stage == 1 & (as_date(Date) >= HarvestDate_Sim) ~ maxstage,
        .default = floor(Stage)
      ),
      Period = factor(Period, ordered = TRUE, levels = as.character(1:maxstage))
    ) |>
    select(-PlantingDate_Sim, -HarvestDate_Sim) |>
    group_by(ID) |>
    mutate(AccRain = cumsum(Rain), AccTT = cumsum(ThermalTime)) |>
    ungroup() # NOTE: Should I add this ungroup()?

  charact_x <- sim_out |>
    group_by(Period, ID) |> # NOTE: Would this do anything if I didn't ungroup?
    select(-Yieldkgha, -Stage) |>
    summarize(
      across(
        where(is.numeric) & !c(DOY, AccRain, AccTT,  AccEmTT),
        function(x) mean(x, na.omit = TRUE)
      ),
      AccRain = sum(Rain),
      AccTT = sum(ThermalTime),
      AccEmTT = max(AccEmTT),
      Period_Start_Date = min(Date),
      Period_End_Date = max(Date)
    ) |>
    mutate(
      Duration = as.numeric(
        as.period(Period_End_Date - Period_Start_Date, "days")
      ) / 86400,
      Period_Start_DOY = yday(Period_Start_Date),
      Period_End_DOY = yday(Period_End_Date)
    ) |>
    relocate(ID, Period, Rain) |>
    relocate(AccRain, .after = Rain) |>
    relocate(AccTT, AccEmTT, .after = ThermalTime) |>
    relocate(Period_Start_DOY, Duration, Period_End_DOY, .after = last_col()) |>
    arrange(ID)

  daily_charact_x <- sim_out

  final_x <- pivot_wider(
    charact_x,
    names_from = Period,
    values_from = Rain:Period_End_DOY
  ) |> right_join(trials_x, .)

  return(list(
    trials_x = trials_x,
    charact_x = charact_x,
    daily_charact_x = daily_charact_x,
    final_x = final_x
  ))
}

save_results <- function(output_dir, output_dfs) {
  # Clear the output folder
  unlink(output_dir, recursive = TRUE)
  dir.create(output_dir)

  for (name in names(output_dfs)) {
    write_csv(output_dfs[[name]], file.path(output_dir, paste0(name, ".csv")))
  }

}

# ---- Driver Function ----
run_apsimx <- function() {
  Sys.setlocale("LC_ALL", "English_United States")
  start_time <- Sys.time()
  files <- load_files()
  df <- files$trials_df
  locs_df <- files$locs_df
  locyear_df <- make_locyear(df)
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
  # Copy crop model from template_models to output directory
  file.copy(from = file.path(files$base_dir, "template_models",
                             paste0(files$crop, "_Template.apsimx")),
            to = file.path(files$output_dir, paste0(files$crop, "_.apsimx")),
            overwrite = TRUE)
  parallel_apsim_files(df, soil_profiles, files$crop, apsim_dir, met_dir, cl)

  #### Run APSIM ####
  writeLines(paste("Running APSIM:", apsim_batches, "Batches"), files$prog_log)
  batches_out <- parallel_run_sim(df, apsim_dir, files$crop, cl, files$prog_log)

  #### Summarize Results ####
  writeLines("Summarizing Results", files$prog_log)
  results <- process_results(apsim_dir, files$crop)
  save_results(file.path(files$output_dir, "output"), results)

  #### Cleanup Step ####
  stopCluster(cl)
  end_time  <- Sys.time()
  print(paste("Runtime:", end_time - start_time))
  writeLines(paste("Finished! Total Runtime:", end_time - start_time))
  close(files$prog_log)
}

run_apsimx()
