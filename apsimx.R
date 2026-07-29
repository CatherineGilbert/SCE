# Start, set up trials_df -----
library(apsimx)
library(tidyverse)
library(daymetr)
library(chirps)
library(nasapower)
library(data.table)
library(soilDB)
library(spData)
library(xml2)
library(here)
library(tools)
library(parallel) 
library(climateR)
library(terra)
Sys.setlocale("LC_ALL", "English_United States")
start_time <- Sys.time() # track running time
print("Starting ...")

#debug
if (FALSE){
 output_dir <- "C:/Users/cmg3/Documents/GitHub/SCE/output_files"
 setwd(output_dir) 
 codes_dir <- "C:/Users/cmg3/Documents/GitHub/SCE"
 mat_handling <- "Soy" 
 weather_acquis <- "NASAPOWER"
 soil_acquis <- "ISRIC"
 templ_model_path <- "C:/Users/cmg3/Documents/GitHub/SCE/template_models/Soy_Template.apsimx"
 templ_model <- file_path_sans_ext(basename(templ_model_path))
 trials_df <- read_csv("C:/Users/cmg3/Documents/GitHub/SCE/example_input_files/abc_test.csv") 
 no_trim <- F
 buffer_val <- 0
}

codes_dir <- here() #where the folder with the codes is
output_dir <- paste0(codes_dir,"/output_files") #folder where the output goes
setwd(output_dir) 

parms <- readRDS("parameters.rds") #pull trial parameters set in app, then set here
list2env(parms, envir = environment())

templ_model_path <- list.files(paste0(codes_dir,"/input"), pattern = ".apsimx", full.names = TRUE)[1]
templ_model <- file_path_sans_ext(basename(templ_model_path))

trials_df <- list.files(paste0(codes_dir,"/input"), pattern = ".csv", full.names = TRUE)[1] %>%
  read_csv(., progress = F, show_col_types = F) 

print("Handle Input Dates ...")
trials_df <- mutate(trials_df, ID = row_number()) %>% rename(X = Longitude, Y = Latitude)
locs_df <- select(trials_df, X, Y) %>% distinct() %>% mutate(ID_Loc = row_number())
trials_df <- left_join(trials_df, locs_df, by = join_by(X,Y))

#date handling
trials_df <- suppressWarnings(mutate(trials_df, Year = as.numeric(str_extract(Planting, "\\b\\d{4}\\b"))))
trials_df <- suppressWarnings(mutate(trials_df, PlantingDate = as_date(as.character(trials_df$Planting), format = "%Y-%m-%d")))
trials_df <- mutate(trials_df, 
  Year = ifelse(is.na(PlantingDate), Year, format(PlantingDate,"%Y")), 
  Year = ifelse(is.na(Year), year(today()) - 1, Year), #if no year, use last year with full data
  # if no planting date, use beginning and end of year as boundaries
  sim_start = if_else(is.na(PlantingDate), as_date(paste0(as.character(Year),"-01-01")), as_date(PlantingDate %m-% months(1))), 
  sim_end = if_else(is.na(PlantingDate), as_date(paste0(as.character(as.numeric(Year)+1),"-12-31")), as_date(PlantingDate %m+% months(24))))

print("Handle Crop Maturities ...")
# Get what maturities of cultivar we'll use
if (mat_handling == "Soy"){
  trials_df <- trials_df %>% mutate(gen1 = floor(Genetics), gen2 = Genetics - gen1) %>%
    mutate(gen1 = case_when( 
      gen1 >= 10 ~ "10",
      gen1 <= -2 ~ "000",
      gen1 == -1 ~ "00",
      gen1 == 0 ~ "0",
      gen1 >= 1 & Genetics <= 9 ~ as.character(gen1)
    )) %>% mutate(gen2 = case_when( 
      gen1 >= 8 ~ "Generic_MG",
      gen2 >= 0 & gen2 < 0.33 ~ "early",
      gen2 >= 0.33 & gen2 < 0.66 ~ "mid",
      gen2 >= 0.66 ~ "late"
    )) %>% mutate(Mat = paste0(gen2,gen1)) %>% 
    select(-gen1, -gen2)
}

if (mat_handling == "Maize"){
  trials_df <- trials_df %>% mutate(lett = str_to_upper(str_extract(Genetics,"^[A-Za-z]")), 
                                    num = as.numeric(str_extract(Genetics,"\\d+")))
  trials_df <- trials_df %>% mutate(lett = ifelse(is.na(lett), "B", lett))
  corn_mats <- c(80,90,95,100,103,105,108,110,112,115,120,130)
  trials_df <- trials_df %>% rowwise() %>%
    mutate(num = corn_mats[which.min(abs(corn_mats - num))[1]]) %>%
    mutate(Mat = paste0(lett,"_",as.character(num)))
  trials_df <- select(trials_df, -lett, -num)
}

if (mat_handling == "Direct"){
  trials_df <- mutate(trials_df, Mat = Genetics)
}

check_time1 <- Sys.time() 

# Get weather, make met files -----

print("Get Weather Data ...")
# For each location, collect weather data for years from minimum (first requested year, ten years before now) to most recent full year

locyear_df <- trials_df %>% select(X,Y, ID_Loc, sim_start, sim_end) %>% distinct() %>% 
  # set bounds for past and future date collection ranges. these are separated. 
  mutate(historical_met_start = if_else(sim_start < today(), sim_start, NA), historical_met_end = if_else(sim_end < today(), sim_end, NA), 
         future_met_start = if_else(sim_start > today(), sim_start, NA), future_met_end = if_else(sim_end > today(), sim_end, NA)) %>%
  # close bounds in case the ranges overlap the present day
  mutate(historical_met_end = if_else(!is.na(historical_met_start) & is.na(historical_met_end), today() - days(1), historical_met_end),
         future_met_start = if_else(!is.na(future_met_end) & is.na(future_met_start), today(), future_met_start)) %>%
  # summarize these for the ranges per loc_ID we'll collect
  select(-sim_start, -sim_end) %>% unique() %>% group_by(ID_Loc,X,Y) %>%
  summarize(historical_met_start = min(historical_met_start, na.rm = T), 
            historical_met_end = max(historical_met_end, na.rm = T), 
            future_met_start = min(future_met_start, na.rm = T), 
            future_met_end = max(future_met_end, na.rm = T)) %>%
  mutate(across(where(is.Date), ~replace_when(.x, is.infinite(.x) ~ NA))) %>% 
  # and make sure we have at least ten years of history that the "typical season" stuff for the TT/precip plots can be created
  mutate(historical_met_start = min(historical_met_start, today() %m-% years(10), na.rm = T), 
         historical_met_end = max(historical_met_end, today() - days(1), na.rm = T)) %>%
  mutate(across(where(is.Date), ~replace_when(.x, is.infinite(.x) ~ NA))) %>% 
  mutate(collection_span = case_when(
    !is.na(historical_met_start) & is.na(future_met_start)  ~ "historical",
    !is.na(historical_met_start) & !is.na(future_met_start)  ~ "both",
    #this one shouldn't actually be possible, given that ten years historical data is generated for each loc_id, but i'm leaving it here
    is.na(historical_met_start) & !is.na(future_met_start)  ~ "future" 
  ))


#clim_model_catalog <- climateR::catalog
#catalog_filtered <- climater_filter(model = "CCSM4", scenario = "rcp85", ensemble = "r2i1p1")

#future weather data acquisition function
get_maca_apsim_met <- function(locyear_tmp, locyear_sv_tmp, startDate, endDate, model, scenario) {
  out <- getMACA(AOI = locyear_sv_tmp, varname = c("rsds","tasmax","tasmin","pr"), startDate = startDate,
                 endDate = endDate, model = model, timeRes = "day",  scenario = scenario, verbose = FALSE)
  names(out) <- sub("^(pr|rsds|tasmin|tasmax).*", "\\1", names(out)) #flexible rename since the names change and also aren't returned in a set order
  out <- dplyr::rename(out, rain = pr, maxt = tasmax, mint = tasmin, radn = rsds)
  new_df <- dplyr::mutate(out, year = year(date), #convert date to year and yday
                   day = lubridate::yday(date), 
                   maxt = round(maxt - 273.15, 2), #convert kelvin to celsius
                   mint = round(mint - 273.15, 2), 
                   radn = round(radn * 0.0036, 2),
                   rain = round(rain, 2)) #convert watt-hours to megajoules / m2
  new_df <- as.data.frame(dplyr::select(new_df, c("year", "day", "radn", "maxt", "mint", "rain")))
  fmet <- as_apsim_met(
    data.frame(new_df),
    filename = paste0("loc_", locyear_tmp$ID_Loc,".met"),
    site = locyear_tmp$ID_Loc,
    latitude = locyear_tmp$Y,
    longitude = locyear_tmp$X,
    colnames = c("year", "day", "radn", "maxt", "mint", "rain"),
    units = c("()", "()", "(MJ/m2/day)", "(oC)", "(oC)", "(mm)"),
    check = TRUE
  )
  return(fmet)
}

# merge_ranges <- function(df) {
#   df %>%
#     arrange(ID, start_date, end_date) %>%
#     group_by(ID) %>%
#     group_modify(~{
#       x <- .x
#       starts <- x$start_date
#       ends   <- x$end_date
#       out_start <- starts[1]
#       out_end   <- ends[1]
#       for(i in 2:nrow(x)){
#         if(starts[i] <= out_end[length(out_end)]){
#           # overlap
#           out_end[length(out_end)] <- max(out_end[length(out_end)], ends[i])
#         } else {
#           # new interval
#           out_start <- c(out_start, starts[i])
#           out_end   <- c(out_end, ends[i])
#           
#         }
#       }
#       tibble(start_date = out_start,end_date = out_end)
#     }) %>%
#     ungroup()
# }

# Setup for parallel processing
no_cores <- detectCores() - 2  # Reserve 2 cores for the system
print(paste("Cores available:",no_cores))
cl <- makeCluster(no_cores)
clusterExport(cl, varlist = c("locyear_df", "weather_acquis", "vect", 
                              "get_daymet2_apsim_met", "get_power_apsim_met","get_chirps_apsim_met",
                              "as_apsim_met", "napad_apsim_met", "impute_apsim_met", "write_apsim_met",
                              "getMACA","get_maca_apsim_met","year","today"
                              ), envir = environment())

# Ensure the directory exists for weather data
unlink("met",recursive = T) ; dir.create("met")

met_results <- parLapply(cl, seq_len(nrow(locyear_df)), function(id) {
  locyear_tmp <- locyear_df[id, ]
  locyear_sv_tmp <- terra::vect(locyear_df[id, ], geom = c("X", "Y"), crs = "EPSG:4326")
  hmet_tmp <- NA
  fmet_tmp <- NA
  
  tryCatch({
    if (locyear_tmp$collection_span %in% c("historical","both")){
         #no it doesn't work as a switch statement, and no I don't know why. 
        if (weather_acquis == "DAYMET"){hmet_tmp <- get_daymet2_apsim_met(lonlat = c(locyear_tmp$X, locyear_tmp$Y), 
                                            years = c(year(locyear_tmp$historical_met_start),    # prevent DAYMET from failing on the current year
                                                  min(year(locyear_tmp$historical_met_end), year(today()) - 1)))
        }
        if (weather_acquis == "NASAPOWER"){hmet_tmp <- get_power_apsim_met(lonlat = c(locyear_tmp$X, locyear_tmp$Y),
                            dates = as.character(c(max(locyear_tmp$historical_met_start, lubridate::as_date("1984-01-01")), 
                                      locyear_tmp$historical_met_end)))
        }
        if (weather_acquis == "CHIRPS"){hmet_tmp <- get_chirps_apsim_met(lonlat = c(locyear_tmp$X, locyear_tmp$Y),
                                # constrain CHIRPS dates to prevent errors
                                dates = as.character(c(max(locyear_tmp$historical_met_start, lubridate::as_date("1981-01-01")),
                                          min(locyear_tmp$historical_met_end, lubridate::floor_date(today(), unit = "years")))))
        } 
    }
  
    if (locyear_tmp$collection_span %in% c("future","both")) {
        fmet_tmp <- get_maca_apsim_met(locyear_tmp, locyear_sv_tmp, 
                                       startDate = locyear_tmp$future_met_start, 
                                       endDate = locyear_tmp$future_met_end,
                                       model = "CCSM4", scenario = "rcp45")
    }
    
    switch(locyear_tmp$collection_span,
           "historical" = met_tmp <- hmet_tmp,
           "future" = met_tmp <- fmet_tmp,
           "both" = met_tmp <- dplyr::bind_rows(hmet_tmp, fmet_tmp)
    )
      
    na_met_tmp <- tryCatch(napad_apsim_met(met_tmp), error = function(e) met_tmp)
    imp_met_tmp <- impute_apsim_met(na_met_tmp)
    attr(imp_met_tmp, "site") <- attr(met_tmp, "site")
    attr(imp_met_tmp, "latitude") <- attr(met_tmp, "latitude")
    attr(imp_met_tmp, "longitude") <- attr(met_tmp, "longitude")
    write_apsim_met(imp_met_tmp, wrt.dir = "met", paste0("loc_", locyear_tmp$ID_Loc, ".met"))
    
    NULL
  },
  error = function(e){
    print(paste0("Weather collection failed for location ", id, ": ", e$message))
    list(Loc_ID = id)
  })  

  })

failed_mets <- unlist(Filter(Negate(is.null), met_results))
trials_df <- mutate(trials_df, WeatherAcquis = weather_acquis, WeatherCollected = if_else(ID_Loc %in% failed_mets, FALSE, TRUE))

check_time2 <- Sys.time() 

#stop if no weather
if (length(list.files(paste0(output_dir,"/met/"), pattern = "\\.met$", recursive = FALSE)) == 0) {stop("No .met files collected successfully.")}

# Get soil, make soil file -----
print("Get Soil Data ...")

unlink("soils",recursive = T) ; dir.create("soils")

soil_results <- list()

for (id in locs_df$ID_Loc){
  locs_tmp <- locs_df[locs_df$ID_Loc == id,]
  tryCatch({
    if (soil_acquis == "SSURGO") {soil_profile_tmp <- get_ssurgo_soil_profile(lonlat = c(locs_tmp$X,locs_tmp$Y), fix = T, check = FALSE)[[1]]}
    if (soil_acquis == "ISRIC") {soil_profile_tmp <- get_isric_soil_profile(lonlat = c(locs_tmp$X,locs_tmp$Y), fix = T, check = FALSE)}
    if (soil_acquis == "World Modeler") {soil_profile_tmp <- get_worldmodeler_soil_profile(lonlat = c(locs_tmp$X,locs_tmp$Y))[["SoilName_1"]]}
    if (soil_acquis == "SLGA") {soil_profile_tmp <- get_slga_soil_profile(lonlat = c(locs_tmp$X,locs_tmp$Y), fix = T, check = FALSE)}
    
    #check_apsimx_soil_profile(soil_profile_tmp)   #for debugging
    
    horizon <- soil_profile_tmp$soil 
    
    #create and fill soil water parameters table
    soilwat_tmp <- soilwat_parms() 
    if (!is.null(soil_profile_tmp$soilwat) & !all(is.na(soil_profile_tmp$soilwat))) {
      soilwat_tmp[names(soil_profile_tmp$soilwat)] <- soil_profile_tmp$soilwat
    }
    PO <- 1-horizon$BD/2.65 #generic soil bulk density constant
    soilwat_tmp$SWCON <- (PO-horizon$DUL)/PO
    soilwat_tmp$SWCON <- ifelse(soilwat_tmp$SWCON < 0, 0.001, soilwat_tmp$SWCON)
    soilwat_tmp$Thickness <- horizon$Thickness 
    soil_profile_tmp$soilwat <- soilwat_tmp
    
    #create and fill initial water parameters table
    initwat_tmp <- initialwater_parms() 
    if (!is.null(soil_profile_tmp$initialwater) & !all(is.na(soil_profile_tmp$initialwater))) {
      initwat_tmp[names(soil_profile_tmp$initialwater)] <- soil_profile_tmp$initialwater
    }
    initwat_tmp$InitialValues <- horizon$DUL
    initwat_tmp$Thickness <- horizon$Thickness
    soil_profile_tmp$initialwater <- initwat_tmp
    
    #create and fill soil organic matter table
    soilorganicmatter_tmp <- soilorganicmatter_parms()
    if (!is.null(soil_profile_tmp$soilorganicmatter) & 
        !all(is.na(soil_profile_tmp$soilorganicmatter))) {
      soilorganicmatter_tmp[names(soil_profile_tmp$soilorganicmatter)] <- soil_profile_tmp$soilwat
    }
    soil_profile_tmp$soilorganicmatter <- soilorganicmatter_tmp
    
    #constrain minimum root weight
    given_rwt <- soil_profile_tmp[["soilorganicmatter"]]$RootWt
    soil_profile_tmp[["soilorganicmatter"]]$RootWt <- ifelse(given_rwt < 0.001 | is.na(given_rwt), 0.001, given_rwt) 
    
    #constrain minimum soil organic carbon content
    given_oc <- soil_profile_tmp[["soil"]]$Carbon
    soil_profile_tmp[["soil"]]$Carbon <- ifelse(given_oc < 0.001  | is.na(given_oc), 0.001, given_oc) 
    
    write_rds(soil_profile_tmp, paste0(output_dir,"/soils/soil_profile_",id,".rds"))
    
    NULL
    
  }, error = function(e){
    print(paste0("Soil collection failed for loc ",id,": ", e$message))
    soil_results <<- append(soil_results, list(Loc_ID = id))
  })
}

failed_soils <- unlist(Filter(Negate(is.null), soil_results))
trials_df <- mutate(trials_df, SoilAcquis = soil_acquis, SoilCollected = if_else(ID_Loc %in% failed_soils, FALSE, TRUE))

#stop if no soils
if (length(list.files(paste0(output_dir,"/soils/"), pattern = "\\.rds$", recursive = FALSE)) == 0) {stop("No soil profiles collected successfully.")}

check_time3 <- Sys.time() 

# Create APSIM files -----
print("Create APSIM Files ...")

unlink("apsim", recursive = TRUE)
dir.create("apsim")
#if the template model isn't already in the inputs folder:
if (paste0(templ_model_path) != paste0(codes_dir,"/input/", templ_model, ".apsimx")) {
  file.copy(from = paste0(templ_model_path),
          to = paste0(codes_dir,"/input/", templ_model, ".apsimx"), overwrite = TRUE)
}

# Prepare for parallel processing

clusterExport(cl, c("trials_df", "codes_dir", "mat_handling", "templ_model", "edit_apsimx", "output_dir", 
                    "edit_apsimx_replace_soil_profile", "paste0", "dir.create", "file.copy", "tryCatch", "print"))

# Parallel APSIM files creation
apsimxfilecreate <- parLapply(cl, 1:nrow(trials_df), function(trial_n) {
  trial_tmp <- trials_df[trial_n,]
  if(!dir.exists(paste0("apsim/trial_",trial_n))) {dir.create(paste0("apsim/trial_",trial_n))}
  source_dir <- paste0("apsim/trial_",trial_n)
  write_dir <-  paste0("apsim/trial_",trial_n)
  filename <- paste0(templ_model, "_", trial_n,".apsimx")
  edit_apsimx(file = paste0(templ_model,".apsimx"), 
              src.dir = paste0(codes_dir,"/input/"), 
              wrt.dir = write_dir, edit.tag = paste0("_",trial_n),
              node = "Clock", parm = "Start", 
              value = paste0(trial_tmp$sim_start,"T00:00:00"), verbose = F)
  edit_apsimx(file = filename,  src.dir = source_dir, wrt.dir = write_dir, overwrite = T,
              node = "Clock", parm = "End", value = paste0(trial_tmp$sim_end,"T00:00:00"), verbose = F)
  edit_apsimx(file = filename, src.dir = source_dir, wrt.dir = write_dir, overwrite = T,
              node = "Weather", value = paste0(getwd(),"/met/loc_",trial_tmp$ID_Loc,".met"), verbose = F)
  if (is.na(trial_tmp$PlantingDate)) {
    edit_apsimx(filename, src.dir = source_dir,  wrt.dir = write_dir, overwrite = T,
                node = "Manager", manager.child = "Sowing",
                parm = "SowDate", value = "NA", verbose = F)
    edit_apsimx(filename, src.dir = source_dir, wrt.dir = write_dir, overwrite = T, node = "Crop", parm = "SowDate", 
                value = "NA", verbose = F)
  } else {
    edit_apsimx(filename, src.dir = source_dir,  wrt.dir = write_dir, overwrite = T,
                node = "Manager", manager.child = "Sowing",
                parm = "SowDate", value = as.character(format(trial_tmp$PlantingDate, "%d-%b")), verbose = F)
    edit_apsimx(filename, src.dir = source_dir, wrt.dir = write_dir, overwrite = T, node = "Crop", parm = "SowDate", 
                value = as.character(format(trial_tmp$PlantingDate, "%d-%b")), verbose = F)
  }
  edit_apsimx(filename, src.dir = source_dir,  wrt.dir = write_dir, overwrite = T,
              node = "Crop", parm = "CultivarName", value = trial_tmp$Mat, verbose = F)
  tryCatch({
    soil_profile_tmp <- readRDS(paste0(output_dir,"/soils/soil_profile_",as.character(trial_tmp$ID_Loc),".rds"))
    edit_apsimx_replace_soil_profile(file = filename, src.dir = source_dir, wrt.dir = write_dir, overwrite = T,
                                     soil.profile = soil_profile_tmp, 
                                     verbose = F)
  }, error = function(e){print("Failed to attach soil profile.")})
  #invisible()
})

check_time4 <- Sys.time() 

#stop if no sims were created
if (length(list.files(paste0(output_dir,"/apsim/"), pattern = ".apsimx", recursive = TRUE)) == 0) {stop("No simulations created.")}

# Run APSIM files -----
print("Run APSIM Files ...")

# Define the number of batches
if (nrow(trials_df) <= 10) {
  num_batches <- 1 # If there are few trials, only run one batch. 
} else {num_batches <- 10} # You can change this to run different percentages at a time

# Calculate the number of trials per batch
batch_size <- ceiling(nrow(trials_df) / num_batches)

# save trial error messages
errlog <- NULL

clusterExport(cl, c("trials_df", "codes_dir", "templ_model", "edit_apsimx", 
                    "edit_apsimx_replace_soil_profile", "paste0", "dir.create",
                    "file.copy", "tryCatch", "print", "apsimx", "mutate", 
                    "write_csv", "errlog"))


# Initialize a list to hold results from all batches
all_results <- list()


# Process each batch
for (batch in 1:num_batches) {
  # Determine the rows for the current batch
  batch_rows <- ((batch - 1) * batch_size + 1):min(nrow(trials_df), batch * batch_size)
  batch_trials <- trials_df[batch_rows, ]
  
  # Split trials for parallel execution
  trial_list <- split(batch_trials, seq(nrow(batch_trials)))
  
  # Run APSIM simulations in parallel
  results <- parLapply(cl, trial_list, function(trial) {
    trial_n <- trial$ID  # Assuming 'ID' is the identifier
    source_dir <- paste0("apsim/trial_", trial_n)
    filename <- paste0(templ_model, "_", trial_n, ".apsimx")
    #output <- data.frame()  # Initialize an empty data frame for the results
    
    # Wrap APSIM simulation and result handling in tryCatch to handle any errors
    tryCatch({
      output_tmp <- apsimx(filename, src.dir = source_dir, silent = TRUE)
      output_tmp <- mutate(output_tmp, "ID" = trial_n) 
      # Save individual trial results
      write_csv(output_tmp, file = paste0(source_dir, "/", templ_model, "_", trial_n, "_out.csv"))
      return()  
    }, error = function(e){
      errlog <- paste0(errlog, "Simulation for trial ", trial_n, " failed with error: ", e$message)
      return(errlog)  # Return NULL if there was an error
    })
  })
  
  # Print errors for failed trials
  errlog <<- do.call(rbind, results)
  print(paste(errlog))
  
  # Print out the progress
  cat(sprintf("Completed batch %d out of %d (%.2f%%)\n", batch, num_batches, 100 * batch / num_batches))
}

check_time5 <- Sys.time() 

# Summarize Results -----
print("Summarize Results ...")
clusterExport(cl, c("read_csv"))

# Merge Outputs
outfiles <- list.files("apsim/", pattern = "_out", recursive = T)
  #stop if no sims ran successfully
if (length(outfiles) == 0) {stop("No simulations ran successfully.")}
daily_sim_outputs <- parLapply(cl, outfiles, function(x){read_csv(paste0("apsim/",x),show_col_types = FALSE)}) %>% 
  data.table::rbindlist(.,use.names = T)
daily_sim_outputs <- select(daily_sim_outputs, -any_of(c("CheckpointID", "SimulationID", "SimulationName", "Zone", "Year"))) %>% arrange(ID)

# Stop the cluster
stopCluster(cl)

# For Debugging:
if (FALSE){
  outfiles <- list.files("apsim/", pattern = "_out", recursive = T)
  daily_sim_outputs <- lapply(outfiles, function(x){read_csv(paste0("apsim/",x),show_col_types = FALSE)}) %>% 
    data.table::rbindlist(.,use.names = T)
  daily_sim_outputs <- select(daily_sim_outputs, -any_of(c("CheckpointID", "SimulationID", "SimulationName", "Zone", "Year"))) %>% arrange(ID)
}

# Get simulated sowing and harvest dates
simsows <- select(daily_sim_outputs, ID, SimSowDate) %>% filter(!is.na(SimSowDate)) 
simmats <- select(daily_sim_outputs, ID, SimMatDate) %>% filter(!is.na(SimMatDate)) 
simharvs <- select(daily_sim_outputs, ID, SimHarvestDate) %>% filter(!is.na(SimHarvestDate)) 
simdates <- left_join(simsows, simmats, by = join_by(ID)) %>% left_join(simharvs, by = join_by(ID))
daily_sim_outputs <- select(daily_sim_outputs, -SimSowDate, -SimMatDate, -SimHarvestDate)

# Get trial result
res <- group_by(daily_sim_outputs, ID) %>% filter(!is.na(Result)) %>% select(ID, Result)

# Trim season (daily_sim_outputs) to buffer duration before planting and after death / harvest
if(no_trim){ #if you don't want to trim outputs
  simstartend <- select(daily_sim_outputs, ID, Date) %>% group_by(ID) %>% summarize(StartDate = min(Date), EndDate = max(Date)) 
  simdates <- left_join(simstartend, simdates) %>% select(ID, StartDate, SimSowDate, SimMatDate, SimHarvestDate, EndDate)
} else { # trim outputs to buffer duration on either side of planting and harvest
  simdates <- simdates %>% mutate(StartDate = date(SimSowDate) %m-% days(buffer_val), EndDate = date(SimHarvestDate) %m+% days(buffer_val)) %>%
    select(ID, StartDate, SimSowDate, SimMatDate, SimHarvestDate, EndDate)
}
daily_sim_outputs <- group_by(daily_sim_outputs, ID) %>% left_join(select(simdates,ID, StartDate, EndDate), by = join_by(ID)) %>%
  filter(Date >= StartDate & Date <= EndDate) %>% select(-StartDate,-EndDate)
daily_sim_outputs <- mutate(daily_sim_outputs, Date = as_date(Date))

# Create trial_info from trial-specific information
maxstage <- group_by(daily_sim_outputs, ID) %>% summarize(MaxStage = max(Stage)) 
trial_info <- rename(trials_df, Latitude = Y, Longitude = X)
trial_info <- trial_info %>% select(-sim_start, -sim_end) %>% 
  left_join(maxstage, by = join_by(ID)) %>% 
  left_join(simdates, by = join_by(ID)) %>% 
  left_join(res, by = join_by(ID)) 
trial_info <- mutate(trial_info, DTM_Sim = as.numeric(SimMatDate - SimSowDate)) %>%
  relocate(DTM_Sim, .after = SimSowDate)
trial_info <- rename(trial_info, MatDate_Sim = SimMatDate, PlantingDate_Sim = SimSowDate, HarvestDate_Sim = SimHarvestDate) 
trial_info <- select(trial_info, -PlantingDate)
trial_info <- relocate(trial_info, ID)
trial_info <- select(trial_info, -any_of("...1"))

# Periods
if (mat_handling %in% c("Soy","Maize")) {
  max_stage <- 11
} else {
  max_stage <- max(daily_sim_outputs$Stage)
}

daily_sim_outputs <- daily_sim_outputs %>% left_join(select(trial_info, ID, HarvestDate_Sim, PlantingDate_Sim), by = join_by(ID)) %>% 
   mutate(Period = case_when(
   Stage == 1 & (Date < PlantingDate_Sim) ~ 1,
   Stage == 1 & (Date >= HarvestDate_Sim) ~ max_stage,
   .default = floor(Stage)
 )) %>% select(-PlantingDate_Sim, -HarvestDate_Sim) %>% 
   mutate(Period = factor(Period, ordered = T, levels = as.character(1:max_stage)))

period_key <- daily_sim_outputs %>% ungroup() %>%
  select(PhaseName, Period) %>% distinct() %>%
  filter(!is.na(PhaseName)) %>%
  group_by(Period) %>%
  summarise(
    Label                   = first(PhaseName),
    `APSIM Phases Included` = paste(PhaseName, collapse = " & "),
    `Original Periods`      = paste(Period,    collapse = ", "),
    .groups = "drop"
  ) %>% ungroup() %>%
  mutate(
    Period = as.character(Period)
  ) %>%
  select(Period, Label, `APSIM Phases Included`, `Original Periods`) %>%
  arrange(as.numeric(Period))

# daily_sim_outputs <- daily_sim_outputs %>% left_join(select(trial_info, ID, MatDate_Sim, Planting)) %>% 
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
#   )) %>% select(-MatDate_Sim) %>% 
#   mutate(Period = factor(Period, ordered = T, levels = as.character(1:11)))

RESERVE_VARS <- c("AccRain", "AccTT", "AccEmTT", "Duration", "Period_Start_Date", 
                  "Period_End_Date", "Period_Start_DOY", "Period_End_DOY", "Duration", "DOY", "Stage")

seasonal_data <- daily_sim_outputs %>%
  group_by(ID, Period) %>%
  summarise(
    AccRain           = sum(Rain, na.rm = TRUE),
    AccTT             = sum(ThermalTime, na.rm = TRUE),
    AccEmTT           = max(AccEmTT, na.rm = TRUE),
    Period_Start_Date = min(Date, na.rm = TRUE),
    Period_End_Date   = max(Date, na.rm = TRUE),
    Period_Start_DOY = yday(Period_Start_Date),
    Period_End_DOY   = yday(Period_End_Date),
    Duration         = n(),
    across(where(is.numeric) & !any_of(RESERVE_VARS),
           ~ mean(.x, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  relocate(ID, Period, Rain) %>% 
  relocate(AccRain, .after = Rain) %>% 
  relocate(AccTT, AccEmTT, .after = ThermalTime) %>%
  relocate(Period_Start_Date, Period_End_Date, Period_Start_DOY, Duration, Period_End_DOY, .after = last_col()) %>%
  arrange(ID, as.numeric(Period)) 

#empty data for missing periods 
idp <- tidyr::expand(tibble(seasonal_data), ID, Period) #full list of ID/Period combinations
idp <- anti_join(idp, seasonal_data,by = join_by(ID,Period)) #which ID/Period combinations are absent in seasonal_data

if (nrow(idp > 0)){
  col_names <- names(seasonal_data)[3:length(names(seasonal_data))]
  for (col in col_names) {
    idp[[col]] <- NA
  }
  idp <- mutate(idp, Duration = 0) #set duration of nonexistent periods to zero
  seasonal_data <- bind_rows(seasonal_data, idp) %>% arrange(ID, Period)
}

print("Writing Results ...")

unlink("results",recursive = T) ; dir.create("results")

write_csv(trial_info, "results/trial_info.csv")
write_csv(seasonal_data, "results/seasonal_data.csv")
write_csv(daily_sim_outputs, "results/daily_sim_outputs.csv")
write_csv(period_key, "results/period_key.csv")

final_x <- pivot_wider(seasonal_data, names_from = Period, values_from = Rain:Period_End_DOY) %>% right_join(trial_info,.,by = join_by(ID))
write_csv(final_x, "results/final_x.csv")

#calculate time duration for running the code:
end_time <- Sys.time()
duration <- end_time - start_time
print(duration)

# print("Time to begin analysis:"); print(check_time1 - start_time)
# print("Time to collect weather:"); print(check_time2 - check_time1)
# print("Time to collect soil:"); print(check_time3 - check_time2)
# print("Time to create sim files:"); print(check_time4 - check_time3)
# print("Time to run sim files:"); print(check_time5 - check_time4)
# print("Time to process results:"); print(end_time - check_time5)

