seasoncorr_mx <- cor(t(out$scfinal_dt))
rownames(seasoncorr_mx) <- as.character(out$IDs)
colnames(seasoncorr_mx) <- as.character(out$IDs)

#load trails_x
trials_x <- read_csv("C:/Users/cmg3/Documents/GitHub/SCT/output_files/results/trial_info.csv")
trials_x <- filter(trials_x, ID %in% out$IDs) %>%
  mutate(PlantingDOY = yday(PlantingDate_Sim), 
                   PD_mday = format(PlantingDate_Sim, "%m/%d"))

#create site distances matrix
sitedist_mx <- distm(trials_x[, c("Longitude","Latitude")],fun = distHaversine)
rownames(sitedist_mx) <- pull(trials_x, ID)
colnames(sitedist_mx) <- pull(trials_x, ID)
#sitedist_mx <- scale(sitedist_mx, center = F)

#same comparisons as early, but using consistency over years and season length

yearly <- out$scfinal_dt %>% rownames_to_column("ID")
datetags <- select(trials_x, ID, ID_Loc, Site, Mat, Year, PlantingDOY, PD_mday, HarvestDate_Sim, PlantingDate_Sim, Latitude)
datetags <- mutate(datetags, seasonlength = HarvestDate_Sim - PlantingDate_Sim)

seasoncorr_dt <- as.data.frame(seasoncorr_mx) %>% rownames_to_column("ID") %>% 
  pivot_longer(pull(., "ID")) %>% rename(ID1 = ID, ID2 = name, season_corr = value) %>%
  mutate(ID1 = as.numeric(ID1), ID2 = as.numeric(ID2))
long_comp <- left_join(datetags, seasoncorr_dt, by = c("ID" = "ID1")) %>% left_join(datetags,., by = c("ID" = "ID2")) %>%
  rename(ID.x = ID) %>% filter(Year.x == Year.y)
sitedist_dt <- as.data.frame(sitedist_mx) %>% rownames_to_column("ID") %>% 
  pivot_longer(pull(., "ID")) %>% rename(ID1 = ID, ID2 = name, sitedist = value) %>%
  mutate(ID1 = as.numeric(ID1), ID2 = as.numeric(ID2))
long_comp <- left_join(long_comp, sitedist_dt, by = c("ID.x" = "ID1", "ID.y" = "ID2"))
long_comp <- filter(long_comp, ID_Loc.x >= ID_Loc.y) 
long_comp <- mutate(long_comp, "planting_offset" = (abs(PlantingDOY.x - PlantingDOY.y) + 1),
                    "season_diff" = abs(as.numeric(seasonlength.x - seasonlength.y)))

#consistency between years for [location + planting data] comparisons, based on dates
vardates <- long_comp %>% group_by(Site.x, Site.y, PD_mday.x, PD_mday.y) %>%
  summarize(var(season_corr), mean(season_corr),
            "sitedist" = mean(sitedist), 
            "planting_offset" = mean(planting_offset),
            "latitude.x" = mean(Latitude.x),
            "latitude.y" = mean(Latitude.y),
            "lat_offset" = mean(abs(latitude.y- latitude.x)),
            mean(season_diff))