library(tidyverse)
library(apsimx)

templ_model_path <- "C:/Users/cmg3/Documents/GitHub/SCE/template_models/Soy_Template.apsimx"
view_apsimx(src.dir = "C:/Users/cmg3/Documents/GitHub/SCE/template_models", file = "Soy_Template.apsimx", node = "Crop", )
args(inspect_apsimx)

trials_df <- read_csv("C:/Users/cmg3/Documents/GitHub/SCE/example_input_files/abc_test.csv") 
trials_df <- mutate(trials_df, ID = row_number()) %>% rename(X = Longitude, Y = Latitude)
locs_df <- select(trials_df, X, Y) %>% distinct() %>% mutate(ID_Loc = row_number())
trials_df <- left_join(trials_df, locs_df, by = join_by(X,Y))


# Get soil, make soil file -----
print("Get Soil Data ...")

soil_profile_list = list()
locs_df$got_soil <- NA
id = 2

ids_needs_soil <- locs_df[locs_df$got_soil == F | is.na(locs_df$got_soil),]$ID_Loc
for (id in ids_needs_soil){
  locs_tmp <- locs_df[locs_df$ID_Loc == id,]
  tryCatch({
    if (soil_aquis == "SSURGO") {soil_profile_tmp <- get_ssurgo_soil_profile(lonlat = c(locs_tmp$X,locs_tmp$Y), fix = T)}
    if (soil_aquis == "ISRIC") {soil_profile_tmp <- get_worldmodeler_soil_profile(lonlat = c(locs_tmp$X,locs_tmp$Y))}
    
    horizon <- soil_profile_tmp[[1]]$soil 
    
    #create SWCON in SoilWater parameters
    soilwat <- soilwat_parms() 
    PO <- 1-horizon$BD/2.65
    soilwat$SWCON <- (PO-horizon$DUL)/PO
    soilwat$SWCON <- ifelse(soilwat$SWCON < 0, 0.001, soilwat$SWCON)
    soilwat$Thickness <- horizon$Thickness 
    soil_profile_tmp[[1]]$soilwat <- soilwat
    
    #set initial water to reasonable values
    initwat <- initialwater_parms() 
    initwat$InitialValues <- horizon$DUL
    initwat$Thickness <- horizon$Thickness
    soil_profile_tmp[[1]]$initialwater <- initwat
    
    #provide soil organic matter table if none exists
    if (is.na(soil_profile_tmp[[1]][["soilorganicmatter"]])) {
      soil_profile_tmp[[1]][["soilorganicmatter"]] <- soilorganicmatter_parms()
    }
    
    #constrain minimum root weight
    given_rwt <- soil_profile_tmp[[1]][["soilorganicmatter"]]$RootWt
    soil_profile_tmp[[1]][["soilorganicmatter"]]$RootWt <- ifelse(given_rwt < 0.001, 0.001, given_rwt) 
    
    #constrain minimum soil organic carbon content
    given_oc <- soil_profile_tmp[[1]][["soil"]]$Carbon
    soil_profile_tmp[[1]][["soil"]]$Carbon <- ifelse(given_oc < 0.001, 0.001, given_oc) 
    
    write_rds(soil_profile_tmp, paste0(output_dir,"/soils/soil_profile_",id,".rds"))
  
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