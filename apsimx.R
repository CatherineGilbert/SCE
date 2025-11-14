# Start, set up trials_df -----
library(apsimx)
library(tidyverse)
library(daymetr)
library(chirps)
library(nasapower)
library(data.table)
library(soilDB)
library(spData)
library(here)
library(tools)
library(parallel)  # For parallel computing
Sys.setlocale("LC_ALL", "English_United States")
start_time <- Sys.time() # track running time
print("Starting ...")

#debug
if (FALSE){
 output_dir <- "C:/Users/cmg3/Documents/GitHub/SCE/output_files"
 setwd(output_dir) 
 codes_dir <- "C:/Users/cmg3/Documents/GitHub/SCE"
 mat_handling <- "Soy" 
 weather_aquis <- "NASAPOWER"
 soil_aquis <- "ISRIC"
 templ_model_path <- "C:/Users/cmg3/Documents/GitHub/SCE/template_models/Soy_Template.apsimx"
 templ_model <- file_path_sans_ext(basename(templ_model_path))
 trials_df <- read_csv("C:/Users/cmg3/Documents/GitHub/SCE/example_input_files/abc_test.csv") 
}

codes_dir <- here() #where the folder with the codes is
output_dir <- paste0(codes_dir,"/output_files") #folder where the output goes
setwd(output_dir) 

parms <- read_csv("parameters.csv", progress = F, show_col_types = F) #pull trial parameters set in app, then set here
mat_handling <- pull(parms, mat_handling)
weather_aquis <- pull(parms, weather_aquis)
soil_aquis <- pull(parms, soil_aquis)

templ_model_path <- list.files(paste0(codes_dir,"/input"), pattern = ".apsimx", full.names = TRUE)[1]
templ_model <- file_path_sans_ext(basename(templ_model_path))

trials_df <- list.files(paste0(codes_dir,"/input"), pattern = ".csv", full.names = TRUE)[1] %>%
  read_csv(., progress = F, show_col_types = F) 

print("Handle Input Dates ...")
trials_df <- mutate(trials_df, ID = row_number()) %>% rename(X = Longitude, Y = Latitude)
locs_df <- select(trials_df, X, Y) %>% distinct() %>% mutate(ID_Loc = row_number())
trials_df <- left_join(trials_df, locs_df, by = join_by(X,Y))

#require year as part of the input
prev_year <- as.numeric(substr(Sys.time(),1,4)) - 1
yesterday <- as.character(today() - days(1))

trials_df <- suppressWarnings(mutate(trials_df, Year = as.numeric(str_extract(Planting, "\\b\\d{4}\\b"))))
trials_df <- suppressWarnings(mutate(trials_df, PlantingDate = as_date(as.character(trials_df$Planting), format = "%Y-%m-%d")))
trials_df <- mutate(trials_df, 
  Year = ifelse(is.na(PlantingDate), Year, format(PlantingDate,"%Y")), 
  Year = ifelse(is.na(Year), prev_year, Year), #if no year, use last year with full data
  # if no planting date, use beginning and end of year as boundaries
  sim_start = if_else(is.na(PlantingDate), as_date(paste0(as.character(Year),"-01-01")), as_date(PlantingDate %m-% months(1))), 
  sim_end = if_else(is.na(PlantingDate), as_date(paste0(as.character(Year),"-12-31")), as_date(PlantingDate %m+% months(10))))
trials_df <- trials_df %>% group_by(ID) %>% mutate(sim_end = min(sim_end, as_date(yesterday))) %>% ungroup()

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
locyear_df <- trials_df %>% select(X,Y,ID_Loc, sim_start) %>% 
  mutate(first_year = year(sim_start)) %>% 
  select(-sim_start) %>% unique() %>% group_by(ID_Loc,X,Y) %>%
  summarize(first_year = min(first_year)) %>%
  mutate(first_year = min(first_year, prev_year - 10)) 

# Setup for parallel processing
no_cores <- detectCores() - 2  # Reserve 2 cores for the system
print(paste("Cores available:",no_cores))
cl <- makeCluster(no_cores)
clusterExport(cl, varlist = c("locyear_df","yesterday","get_daymet2_apsim_met",
                              "get_power_apsim_met","get_chirps_apsim_met",
                              "napad_apsim_met", "impute_apsim_met", "write_apsim_met",
                              "prev_year","weather_aquis"), envir = environment())


# Ensure the directory exists for weather data

unlink("met",recursive = T) ; dir.create("met")

parLapply(cl, seq_len(nrow(locyear_df)), function(idx) {
  locyear_tmp <- locyear_df[idx, ]
  try({ #no it doesn't work as a case statement, and no I don't know why. 
    if (weather_aquis == "DAYMET"){met_tmp <- get_daymet2_apsim_met(lonlat = c(locyear_tmp$X, locyear_tmp$Y), 
                                     years = c(as.integer(locyear_tmp$first_year), as.integer(prev_year)))}
    if (weather_aquis == "NASAPOWER"){met_tmp <- get_power_apsim_met(lonlat = c(locyear_tmp$X, locyear_tmp$Y),
                        dates = c(paste0(locyear_tmp$first_year,"-01-01"), yesterday))}
    if (weather_aquis == "CHIRPS"){met_tmp <- get_chirps_apsim_met(lonlat = c(locyear_tmp$X, locyear_tmp$Y),
                            dates = c(paste0(locyear_tmp$first_year,"-01-01"), yesterday))}
    na_met_tmp <- tryCatch(napad_apsim_met(met_tmp), error = function(e) met_tmp)
    imp_met_tmp <- tryCatch(impute_apsim_met(na_met_tmp), warning = function(w) na_met_tmp)
    attr(imp_met_tmp, "site") <- attr(met_tmp, "site")
    attr(imp_met_tmp, "latitude") <- attr(met_tmp, "latitude")
    attr(imp_met_tmp, "longitude") <- attr(met_tmp, "longitude")
    write_apsim_met(imp_met_tmp, wrt.dir = "met", paste0("loc_", locyear_tmp$ID_Loc, ".met"))
  })
})

check_time2 <- Sys.time() 

# Get soil, make soil file -----
print("Get Soil Data ...")

soil_profile_list = list()
unlink("soils",recursive = T) ; dir.create("soils")
locs_df$got_soil <- NA

ids_needs_soil <- locs_df[locs_df$got_soil == F | is.na(locs_df$got_soil),]$ID_Loc
for (id in ids_needs_soil){
  locs_tmp <- locs_df[locs_df$ID_Loc == id,]
  tryCatch({
    if (soil_aquis == "SSURGO") {soil_profile_tmp <- get_ssurgo_soil_profile(lonlat = c(locs_tmp$X,locs_tmp$Y), fix = T)}
    if (soil_aquis == "ISRIC") {soil_profile_tmp <- get_worldmodeler_soil_profile(lonlat = c(locs_tmp$X,locs_tmp$Y))}
    
    horizon <- soil_profile_tmp[[1]]$soil 
    
    soilwat <- soilwat_parms() #creating SWCON in SoilWater parameters
    PO <- 1-horizon$BD/2.65
    soilwat$SWCON <- (PO-horizon$DUL)/PO
    soilwat$SWCON <- ifelse(soilwat$SWCON < 0, 0, soilwat$SWCON)
    soilwat$Thickness <- horizon$Thickness 
    soil_profile_tmp[[1]]$soilwat <- soilwat
    
    initwat <- initialwater_parms() #set initial water to reasonable values
    initwat$InitialValues <- horizon$DUL
    initwat$Thickness <- horizon$Thickness
    soil_profile_tmp[[1]]$initialwater <- initwat
    
    rwt_min <- 0.001 #set minimum root weight
    given_rwt <- soil_profile_tmp[[1]][["soilorganicmatter"]]$RootWt
    soil_profile_tmp[[1]][["soilorganicmatter"]]$RootWt <- ifelse(given_rwt < rwt_min, rwt_min, given_rwt) 
    
    oc_min <- 0.001 #set minimum carbon content in soils
    given_oc <- soil_profile_tmp[[1]][["soil"]]$Carbon
    soil_profile_tmp[[1]][["soil"]]$Carbon <- ifelse(given_oc < oc_min, oc_min, given_oc) 
    
    write_rds(soil_profile_tmp, file = paste0("soils/soil_profile_",id,".soils"))
    soil_profile_list[[as.character(id)]] <- soil_profile_tmp[[1]]
    locs_df[locs_df$ID_Loc == id,"got_soil"] <- T
    print(paste0("loc: ",id,"   ",round(which(ids_needs_soil == id)/length(ids_needs_soil),4)))
  }, error = function(e){
    locs_df[locs_df$ID_Loc == id,"got_soil"] <<- F
    print(paste0("loc: ",id,"   ",round(which(ids_needs_soil == id)/length(ids_needs_soil),4),"  FAIL"))
  })
}
write_rds(soil_profile_list, "soils/soil_profile_list.rds")

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

clusterExport(cl, c("trials_df", "codes_dir", "mat_handling", "templ_model", "edit_apsimx", "output_dir", "soil_profile_list",
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
    edit_apsimx_replace_soil_profile(file = filename, src.dir = source_dir, wrt.dir = write_dir, overwrite = T,
                                     soil.profile = soil_profile_list[[as.character(trial_tmp$ID_Loc)]], verbose = F)
  }, error = function(e){})
  invisible()
})

check_time4 <- Sys.time() 

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
  
  # Run APSIM simulations in parallel for the current batch
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
      # Append the output of this trial to the overall results
      # output <- rbind(output, output_tmp)
      # Save individual trial results
      write_csv(output_tmp, file = paste0(source_dir, "/", templ_model, "_", trial_n, "_out.csv"))
      #return(output)
      return()  
    }, error = function(e){
      errlog <- paste0(errlog, "Simulation for trial ", trial_n, " failed with error: ", e$message)
      return(errlog)  # Return NULL if there was an error
    })
  })
  
  # Combine the results from this batch and add to the all_results list
  #batch_results <- do.call(rbind, results)
  #all_results[[batch]] <- batch_results
  
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
daily_output <- parLapply(cl, outfiles, function(x){read_csv(paste0("apsim/",x),show_col_types = FALSE)}) %>% 
  data.table::rbindlist(.,use.names = T)
daily_output <- select(daily_output, -any_of(c("CheckpointID", "SimulationID", "SimulationName", "Zone", "Year"))) %>% arrange(ID)

# Stop the cluster
stopCluster(cl)

# For Debugging:
if (FALSE){
  outfiles <- list.files("apsim/", pattern = "_out", recursive = T)
  daily_output <- lapply(outfiles, function(x){read_csv(paste0("apsim/",x),show_col_types = FALSE)}) %>% 
    data.table::rbindlist(.,use.names = T)
  daily_output <- select(daily_output, -any_of(c("CheckpointID", "SimulationID", "SimulationName", "Zone", "Year"))) %>% arrange(ID)
}



# Get simulated sowing and harvest dates
simsows <- select(daily_output, ID, SimSowDate) %>% filter(!is.na(SimSowDate)) 
simmats <- select(daily_output, ID, SimMatDate) %>% filter(!is.na(SimMatDate)) 
simharvs <- select(daily_output, ID, SimHarvestDate) %>% filter(!is.na(SimHarvestDate)) 
simdates <- left_join(simsows, simmats, by = join_by(ID)) %>% left_join(simharvs, by = join_by(ID))
daily_output <- select(daily_output, -SimSowDate, -SimMatDate, -SimHarvestDate)

# Trim season (daily_output) to two weeks before planting and two weeks after death / harvest
simdates <- simdates %>% mutate(StartDate = date(SimSowDate) %m-% weeks(2), EndDate = date(SimHarvestDate) %m+% weeks(2)) %>%
  select(ID, StartDate, SimSowDate, SimMatDate, SimHarvestDate, EndDate)
daily_output <- group_by(daily_output, ID) %>% left_join(select(simdates,ID, StartDate, EndDate), by = join_by(ID)) %>%
  filter(Date >= StartDate & Date <= EndDate) %>% select(-StartDate,-EndDate)
daily_output <- mutate(daily_output, Date = as_date(Date))

# Create trial_info from trial-specific information
maxstage <- group_by(daily_output, ID) %>% summarize(MaxStage = max(Stage)) #summarize(Yield_Sim = max(Yieldkgha),  MaxStage = max(Stage))
res <- group_by(daily_output, ID) %>% filter(!is.na(Result)) %>% select(ID, Result)
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
  max_stage <- max(daily_output$Stage)
}

daily_output <- daily_output %>% left_join(select(trial_info, ID, HarvestDate_Sim, PlantingDate_Sim), by = join_by(ID)) %>% 
   mutate(Period = case_when(
   Stage == 1 & (Date < PlantingDate_Sim) ~ 1,
   Stage == 1 & (Date >= HarvestDate_Sim) ~ max_stage,
   .default = floor(Stage)
 )) %>% select(-PlantingDate_Sim, -HarvestDate_Sim) %>% 
   mutate(Period = factor(Period, ordered = T, levels = as.character(1:max_stage)))

# Add cumulative precipitation and thermal time
daily_output <- daily_output %>% group_by(ID) %>% mutate(AccRain = cumsum(Rain), AccTT = cumsum(ThermalTime))

# daily_output <- daily_output %>% left_join(select(trial_info, ID, MatDate_Sim, Planting)) %>% 
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

seasonal_data <- daily_output %>% 
  group_by(Period, ID) %>% select(-any_of(c("Yieldkgha", "Stage"))) %>% 
  summarize(across(where(is.numeric) & !c(DOY,AccRain,AccTT,AccEmTT), function(x){mean(x,na.omit=T)}), 
            AccRain = sum(Rain), AccTT = sum(ThermalTime), AccEmTT = max(AccEmTT),
            Period_Start_Date = min(Date), Period_End_Date = max(Date)) %>% 
  mutate(Duration = as.numeric(as.period(Period_End_Date - Period_Start_Date, "days"))/86400 + 1, 
         Period_Start_DOY = yday(Period_Start_Date), 
         Period_End_DOY = yday(Period_End_Date)) %>%
  relocate(ID, Period, Rain) %>% 
  relocate(AccRain, .after = Rain) %>% relocate(AccTT, AccEmTT, .after = ThermalTime) %>%
  relocate(Period_Start_DOY, Duration, Period_End_DOY, .after = last_col()) %>%
  arrange(ID) 

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

daily_sim_outputs <- daily_output

print("Writing Results ...")

unlink("results",recursive = T) ; dir.create("results")

write_csv(trial_info, "results/trial_info.csv")
write_csv(seasonal_data, "results/seasonal_data.csv")
write_csv(daily_sim_outputs, "results/daily_sim_outputs.csv")

final_x <- pivot_wider(seasonal_data, names_from = Period, values_from = Rain:Period_End_DOY) %>% right_join(trial_info,.,by = join_by(ID))
write_csv(final_x, "results/final_x.csv")


period_key <- daily_sim_outputs %>% ungroup() %>%
  select(StageName, Period) %>% distinct() %>%
  filter(!is.na(StageName)) %>%
  rename("APSIM StageName" = StageName)

period_key <- mutate(period_key, Notes = case_match(Period,
                                      min(Period) ~ "includes two weeks pre-planting",
                                      max(Period) ~ "includes two weeks post-harvest",
                                      .default = NA
                                      
)) %>% select(Period, `APSIM StageName`, Notes)
write_csv(period_key, "results/period_key.csv")


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

