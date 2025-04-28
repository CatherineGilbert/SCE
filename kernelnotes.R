#seasonal profiles datafame NxM, tral x seasonal covariates
#pairwise or cosine correlation of that dataframe's trials, getting cosine similarity and euclidean distance 
#cluster the two bit matrix, look at clusters to find patterns of relationships between points
#use anomaly detection to find trial pairs which don't fit into those patterns
library(tidyverse)
library(lsa)
library(abind)
library(esquisse)
library(geosphere)
library(RColorBrewer)
library(janitor)
library(corrr)
library(pheatmap)


#from the seasonal covariates, pivot wide and flatten data from all trials to that there is one long vector per location 
#run cosine similarity P vs Q and get the result for each intersection of sites 
#use correlation distance? 
#can create a heatmap from these similarities

library(proxy)
library(philentropy)


final_x <- read_csv("C:/Users/cmg3/Documents/dateshift_output/final_x.csv")
charact_x <- read_csv("C:/Users/cmg3/Documents/dateshift_output/charact_x.csv")
final_x <- filter(final_x, Result == "Harvested at Maturity.") 

seascov <- t(out$scfinal_dt)

cosine_dist(seascov)





#--------------------

#create seasonal similarity matrix 
ID_corr2 <- function(matsel, final_x, charact_x, 
                     exclude_badp = TRUE, exclude_startend = TRUE,
                     nzv_chk = 1e-6, empty_chk = 0.9, var_chk = 0.9) {
  
  pal_f <- colorRampPalette(brewer.pal(9,"RdYlBu")) #creates a continuous palette
  palette <- rev(pal_f(50)[1:50])
  
  #set how individual trials will be labeled
  nametag <- select(final_x, ID, Site, PlantingDate_Sim, Mat) %>% 
    mutate(tag = paste0(ID,": ", Site, " ", format(PlantingDate_Sim, "%j/%Y")),
           mtag = paste0(ID,": ", Mat, " ", Site, " ", format(PlantingDate_Sim, "%j/%Y"))) 
  
  #generic remove periods that don't have enough data to be used (remove 6 in this case)
  #filter to trials that ended successfully
  full_run_IDs <- select(final_x, ID, MaxStage) %>% 
    filter(!is.na(MaxStage)) %>%
    filter(MaxStage == max(MaxStage)) %>% pull(ID)
  #find, of successful trials, periods where the mean duration < 1
  period_durs <- select(charact_x, ID, Period, Duration) %>% filter(ID %in% full_run_IDs) %>% 
    group_by(Period) %>% summarise(Duration = mean(Duration)) 
  if(any(period_durs$Duration < 1)) {
    badp <- filter(period_durs, Duration < 1) %>% pull(Period) #define discarded periods as periods with mean duration < 1
  } else {
    badp <- NULL
  }
  
  #get names of variables to use for comparison
  varn <- charact_x %>% ungroup() %>% 
    select(where(is.numeric) & !c(ID, Period, Period_Start_DOY, Duration, Period_End_DOY)) %>%
    names()
  
  final_dt <- filter(final_x, Mat == matsel) %>% 
    select(ID, starts_with(varn)) %>% 
    
    #grab only numeric variables (no dates)
    select(where(is.numeric))
  
  #save a list of all of the parameters now 
  full_varlist <- names(select(final_dt, -ID))
  
  #remove trials where no data was collected
  final_dt <- remove_empty(final_dt, which = c("rows"), cutoff = empty_chk) #empty_chk = 0.9
  
  final_dt_locked <- final_dt
  
  #remove parameters that intersect with discarded periods
  if (exclude_badp) {
    badp_vars <- names(final_dt)[!names(final_dt) %in% names(select(final_dt, !ends_with(paste0("_",badp))))]
    final_dt <- select(final_dt, !ends_with(paste0("_",badp)))
  } else {
    badp_vars <- c("")
  }
  
  #remove parameters that intersect with periods before / after growing season
  if (exclude_startend) {
    startend_vars <- names(final_dt)[!names(final_dt) %in% names(select(final_dt, !ends_with(paste0("_",c(min(period_durs$Period), max(period_durs$Period))))))]
    final_dt <- select(final_dt, !ends_with(paste0("_",c(min(period_durs$Period), max(period_durs$Period)))))
  } else {
    startend_vars <- c("")
  }
  
  #remove parameters with near zero variance
  nzv_data <- sapply(final_dt, function(x){var(x, na.rm = TRUE)})
  nzv_vars <- names(nzv_data)[nzv_data < nzv_chk] #nzv_chk = 1e-6
  final_dt <- select(final_dt, !any_of(nzv_vars))
  
  #remove parameters which are autocorrelated, based on the full runs 
  final_full <- filter(final_dt, ID %in% full_run_IDs) %>% column_to_rownames("ID") #subset the data to successful runs
  var_cor <- cor(final_full, use = "complete.obs")
  correlated_vars <- caret::findCorrelation(var_cor, cutoff = var_chk, names = T)
  final_dt <- select(final_dt, !any_of(correlated_vars)) #remove autocorrelated variables
  
  #plot removed variables
  param_status <- data.frame(
    Parameter = full_varlist
  ) %>% mutate(Status = case_match(
    Parameter,
    startend_vars ~ "Discarded (Start/End Period)", 
    badp_vars ~ "Discarded (Shortened Period)",
    nzv_vars ~ "Discarded (Low Variance)",
    correlated_vars ~ "Discarded (Multicollinearity)",
    .default = "Kept"
  ))
  
  #scale the final parameters used for comparison
  scfinal_dt <- final_dt %>%
    column_to_rownames("ID") %>%
    scale() %>% as.data.frame() #scale variables
  
  #list of IDs
  id_list <- final_dt$ID
  
  out <- list(
    IDs = id_list, #trial IDs
    nametag = nametag, #used for labels. it's ID/Site/Planting DOY/Year
    used_params = param_status,
    final_dt = final_dt, #unscaled parameters used for seasonal correlations
    scfinal_dt = scfinal_dt #scaled parameters used for seasonal correlations
  )
}
out <- ID_corr2(matsel = "mid2", final_x = final_x, charact_x = charact_x)
seasoncorr_mx <- cor(t(out$scfinal_dt))
rownames(seasoncorr_mx) <- as.character(out$IDs)
colnames(seasoncorr_mx) <- as.character(out$IDs)

#load trails_x
trials_x <- read_csv("C:/Users/cmg3/Documents/dateshift_output/trials_x.csv")
trials_x <- filter(trials_x, ID %in% out$IDs) 
trials_x <- mutate(trials_x, PlantingDOY = yday(PlantingDate_Sim), 
                   PD_mday = format(PlantingDate_Sim, "%m/%d"))

#create site distances matrix
sitedist_mx <- distm(trials_x[, c("Longitude","Latitude")],fun = distHaversine)
rownames(sitedist_mx) <- pull(trials_x, ID)
colnames(sitedist_mx) <- pull(trials_x, ID)
sitedist_mx <- scale(sitedist_mx, center = F)

#create planting timing offset matrix
plantings <- select(trials_x, ID, PlantingDOY)
planting_mx <- outer(plantings$PlantingDOY, plantings$PlantingDOY, FUN = "-")
planting_mx <- scale(planting_mx, center = F) %>% abs()
rownames(planting_mx) <- pull(trials_x, ID)
colnames(planting_mx) <- pull(trials_x, ID)

#create array
array1 <- abind(seasoncorr_mx, sitedist_mx, planting_mx, along = 3)

# Get the row/column names
nms <- dimnames(array1)[[1]]

# Create all pairwise combinations
pairwise_df <- expand.grid(row = nms, col = nms, stringsAsFactors = FALSE)

# Flatten the two matrix layers into vectors
pairwise_df$correlation <- as.vector(array1[,,1])
pairwise_df$distance <- as.vector(array1[,,2])
pairwise_df$timing <- as.vector(array1[,,3])
pairwise_df <- pairwise_df[pairwise_df$row < pairwise_df$col, ]

set.seed(123)
#get cluster number through elbow method
wss <- sapply(1:10, function(k){
  kmeans(slice_sample(select(pairwise_df, correlation, distance, timing), n = 3000), centers = k, nstart = 10)$tot.withinss
})
plot(1:10, wss, type = "b", pch = 19, 
     xlab = "Number of Clusters (k)",
     ylab = "Total Within-Cluster Sum of Squares",
     main = "Elbow Method") #six-ish?? seems good

#do clustering
k_result <- kmeans(select(pairwise_df, correlation, distance, timing), centers = 4)
pairwise_df$cluster <- k_result$cluster

library(factoextra)

plot_data <- slice_sample(pairwise_df, n = 10000) %>% mutate(cluster = as.factor(cluster))

fviz_cluster(list(data = plot_data[c("correlation","distance","timing")], cluster = plot_data$cluster),
             geom = "point")

library(plotly)
plot_ly(data = plot_data, 
        x = ~correlation, 
        y = ~distance, 
        z = ~timing, 
        color = ~cluster,
        type = "scatter3d",
        mode = "markers",
        marker = list(size = 1))


plot_data <- filter(pairwise_df, distance > 1.645 | distance < -1.645)

ggplot(plot_data) +
  aes(x = distance, y = correlation, colour = cluster) +
  geom_density2d() +
  scale_color_gradient() +
  theme_minimal()

huh <- select(plot_data, correlation, distance, timing) %>% as.matrix()


library(ks)
kde(x = huh)


library(FactoMineR)
out$final_dt

ex_pca <- PCA(out$final_dt)

head(ex_pca$eig)

pca1 <- PCA(select(charact_x, Period:WaterStress))
pca1$var

pca1$var


#mahalanobois distance btween geographical distance and similarity matrices

#--------------------

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
long_comp <- mutate(long_comp, "planting_offset" = (abs(PlantingDOY.x - PlantingDOY.y) + 1) %/% 7, #planting offset is number of weeks apart
                    "season_diff" = abs(as.numeric(seasonlength.x - seasonlength.y)))

#consistency between years for [location + planting data] comparisons, based on dates
vardates <- long_comp %>% group_by(Site.x, Site.y, PD_mday.x, PD_mday.y) %>% 
  filter(PD_mday.x < 165 & PD_mday.x < 165) %>% 
  summarize(var(season_corr), mean(season_corr),
            "sitedist" = mean(sitedist), 
            "planting_offset" = mean(planting_offset),
            "latitude.x" = mean(Latitude.x),
            "latitude.y" = mean(Latitude.y),
            mean(season_diff))

#consistency between years for [location + planting data] comparisons, based on difference in planting delay
vardiff <- long_comp %>% 
  filter(PD_mday.x < 165 & PD_mday.x < 165) %>% #get rid of comparison pairs to late dates
  group_by(Site.x, Site.y, planting_offset) %>% 
  summarize(var(season_corr), mean(season_corr),
            "sitedist" = mean(sitedist), 
            mean(season_diff)) 


ggplot(vardates) +
  aes(
    x = `var(season_corr)`,
    y = `mean(season_corr)`,
    colour = planting_offset
  ) +
  geom_point() +
  scale_color_gradient() +
  theme_minimal()



sitename <- "urbana_il"
ilcomp <- filter(long_comp, Site.x == sitename | Site.y == sitename) %>%
  filter(`season_corr` > 0.5) %>%
  select(Site.x,Site.y,PD_mday.x,PD_mday.y, 
  season_corr,sitedist,planting_offset,season_diff)     
ilvardates <- filter(vardates, Site.x == sitename | Site.y == sitename) %>% ungroup() %>% filter(`sitedist` != 0) #smiliarity over date pairs
il2il <- filter(vardates, Site.x == sitename | Site.y == sitename) %>% ungroup() %>% filter(`sitedist` == 0) %>% filter(PD_mday.x == PD_mday.y)

ilvardates %>%
  ggplot() +
  aes(
    x = planting_offset,
    y = `mean(season_corr)`,
    colour = `var(season_corr)`
  ) +
  geom_jitter() +
  scale_color_viridis_c(option = "viridis", direction = 1) +
  theme_minimal()

ilvardates %>%
  ggplot() +
  aes(
    x = `mean(season_diff)`,
    y = `mean(season_corr)`,
    colour = `var(season_corr)`
  ) +
  geom_jitter() +
  scale_color_viridis_c(option = "viridis", direction = 1) +
  theme_minimal()




















var2 <- filter(var1, `sitedist` != 0) %>%
  filter((`sitedist` < 0.5 & `mean(season_corr)` < -0.25 & planting_offset < 20) | #close but different
          (`sitedist` > 1 & `mean(season_corr)` > 0.5)) #far but similar



ggplot(var1) +
  aes(x = `planting_offset`, y = `mean(season_corr)`, colour = sitedist) +
  geom_jitter(size = 0.1, width = 1.5)


diff_sets <- filter(var1, `var(season_corr)` < 0.1, `sitedist` != 0, `mean(season_corr)` < -0.4)
same_sets <- filter(var1, `var(season_corr)` < 0.1, `sitedist` != 0, `mean(season_corr)` > 0.55)

ggplot(diff_sets) +
  aes(x = `planting_offset`, y = `mean(season_corr)`, colour = sitedist) +
  geom_jitter(size = 0.1, width = 1.5)

ggplot(same_sets) +
  aes(x = `planting_offset`, y = `mean(season_corr)`, colour = `var(season_corr)`) +
  geom_jitter(size = 0.2, width = 1.5)


lm <- lm(data = long_comp, season_corr ~ sitedist + plantings) 
summary(lm)


long_comp %>% group_by(PlantingDOY.x, PlantingDOY.y)


samediff <- rbind(diff_sets, same_sets) %>% ungroup()

diff_tmp <- distinct(select(samediff, ends_with(".x")))
same_tmp <- distinct(select(samediff, ends_with(".y")))
names(diff_tmp) <- c("Site","PD")
names(same_tmp) <- c("Site","PD")
chosen_tmp <- rbind(diff_tmp, same_tmp)
chosen_tmp <- distinct(chosen_tmp)


df <- chosen_tmp
# Convert PD to Date (we'll append a dummy year first)
df$PD <- as.Date(paste0("2025/", df$PD), format = "%Y/%m/%d") # Dummy year to parse mm/dd

# Expand the dataframe
library(dplyr)
library(tidyr)

df_expanded <- df %>%
  tidyr::uncount(10, .id = "YearOffset") %>%
  mutate(Planting = PD - lubridate::years(YearOffset)) %>%
  select(Site, PD, Planting)

# If you want PD back in mm/dd format
df_expanded$PD <- format(df_expanded$PD, "%m/%d")
df_expanded$Planting <- format(df_expanded$Planting, "%Y-%m-%d")

chosen <- select(df_expanded, Site, Planting) %>% mutate(Genetics = "2.5") %>% 
  left_join(distinct(select(trials_x, Site, Latitude, Longitude)))


