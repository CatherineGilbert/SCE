library(pheatmap)
library(janitor)
library(RColorBrewer)
library(tidyverse)
library(esquisse)
library(lubridate)
library(apsimx)
library(ggplot2)
library(here)
library(viridisLite)
library(dendextend)

select <- dplyr::select

plotting <- FALSE

trials_x <- read_csv("./output/trials_x.csv")
charact_x <- read_csv("./output/charact_x.csv")
daily_charact_x <- read_csv("./output/daily_charact_x.csv")
final_x <- read_csv("./output/final_x.csv")

if (plotting){
  
  var <- "Rain"
  matval <- "mid1"
  site_tag <- "ames_ia"
  varchoice <- charact_x %>% ungroup() %>% select(where(is.numeric) & !c(ID, Period)) %>% names()

  j_dt <- filter(trials_x, Mat == matval) %>% select(ID,Genetics, Site, Mat) %>% left_join(charact_x)
  
for(var in varchoice){
  print(var)
  x <- ggplot(j_dt) +
    aes(x = Site, y = .data[[var]]) +
    geom_boxplot() +
    theme_minimal()
  plot(x)
}

  pal_f <- colorRampPalette(brewer.pal(9,"RdYlBu")) #creates a continuous palette
  palette <- rev(pal_f(50)[2:50])
  
for(var in varchoice){
  print(var)
  var_mat <- j_dt %>% select(ID, Site, Period, starts_with(var)) %>%
    pivot_wider(names_from = Period, values_from = var) %>% select(-ID) %>%
    group_by(Site) %>% summarize(across(where(is.numeric), function(x){mean(x,na.rm=T)})) %>%
    column_to_rownames("Site") 
  var_mat <- select(var_mat, as.character(1:11)) %>%
    remove_empty(which = "rows") %>%
    as.matrix()
  
  var_mat[is.nan(var_mat)] <- NA
  
  if (all(var_mat == var_mat[1,1])){  #check if matrix is constant
    pheatmap(var_mat, angle_col = 45,
             color = palette,
             breaks=c(var_mat[1,1]-2, var_mat[1,1]-1,var_mat[1,1]+1,var_mat[1,1]+2),
             fontsize = 10, 
             display_numbers = round(var_mat, 2), 
             number_color = "grey10", 
             number_format = "%.2f", 
             legend = F,
             cluster_cols = F,
             cluster_rows = T,
             main = paste0("Means of ",var," by Site (Maturity: ",matval,")"))
     } else {
  pheatmap(var_mat, angle_col = 45,
           color = palette,
           fontsize = 10, 
           display_numbers = round(var_mat, 2), 
           number_color = "grey10", 
           scale = "column",
           number_format = "%.2f", 
           legend = F,
           cluster_cols = F,
           cluster_rows = T,
           main = paste0("Means of ",var," by Site (Maturity: ",matval,")"))
  }
}
}

#get thermal time and precip for the last ten years of records
current_year <- as.numeric(substr(Sys.time(),1,4)) - 1
bigmet <- data.frame()
for(s in 1:max(trials_x$ID_Loc)){
  lil_met <- read_apsim_met(paste0("./met/loc_",s,".met"), verbose = F) %>% as_tibble() %>%
    filter(year >= current_year - 9, year <= current_year) %>% mutate(ID_Loc = s)
  bigmet <- rbind(bigmet, lil_met)
}
bigmet <- trials_x %>% select(Site, ID_Loc) %>% distinct() %>% left_join(bigmet) %>% group_by(Site, ID_Loc, year, day)
max_temp = 34 #thermal time max temp
base_temp = 0 #thermal time base temp
bigmet <- mutate(bigmet, tt = max((min(maxt,max_temp)+max(mint,base_temp))/2 - base_temp,0)) %>% ungroup() 

#start and end of simulation as doy, going over 365 if wrapping over the new year
startend <- select(trials_x, Site, Year, ID, Mat, PlantingDate_Sim, HarvestDate_Sim) %>%
  mutate(first_doy = yday(PlantingDate_Sim), 
         until_final =  as.numeric(HarvestDate_Sim - PlantingDate_Sim),
         final_doy = first_doy + until_final) #done this way because final_doy can go over 365
#mean start doy and end doy for each site
mean_startend <- group_by(startend, Site) %>% 
  summarize(first_doy = mean(first_doy, na.rm = T), final_doy = mean(final_doy, na.rm = T))
#season limited to average start and end of simulations
filtmet <- bigmet %>% left_join(mean_startend) %>% filter(day >= first_doy & day <= final_doy)

#accumulation of thermal time / precip for an average season at each site
#doy of sowing/harvest set on average dates based on trials that were input
dbtw_sites <- filtmet %>% group_by(Site, year) %>% 
  mutate(acc_precip = cumsum(rain), acc_tt = cumsum(tt)) %>%
  ungroup() %>% group_by(Site, day) %>% 
  summarize(acc_precip= mean(acc_precip, na.rm = T), acc_tt = mean(acc_tt, na.rm = T))
sdbtw_sites <- dbtw_sites %>% mutate(day = day-min(day)+1)

if (plotting) {
ggplot(dbtw_sites) + 
  aes(x = day, y = acc_precip, colour = Site) +
  geom_line() +
  scale_color_hue(direction = 1) +
  labs(x = "Day of Year", y = "Accumulated Precipitation (mm)") +
  theme_minimal()
ggplot(dbtw_sites) + 
  aes(x = day, y = acc_tt, colour = Site) +
  geom_line() +
  scale_color_hue(direction = 1) +
  labs(x = "Day of Year", y = "Accumulated Thermal Time") +
  theme_minimal()
#days after sowing
ggplot(sdbtw_sites) + 
  aes(x = day, y = acc_precip, colour = Site) +
  geom_line() +
  scale_color_hue(direction = 1) +
  labs(x = "Days after Sowing", y = "Acc. Precipitation (mm)") +
  theme_minimal()
ggplot(sdbtw_sites) + 
  aes(x = day, y = acc_tt, colour = Site) +
  geom_line() +
  scale_color_hue(direction = 1) +
  labs(x = "Days after Sowing", y = "Acc. Thermal Time") +
  theme_minimal()
}

#cross charts comparing accumulated precip/thermal time

wthn_sites <- filtmet %>% ungroup() %>% group_by(Site, year) %>% 
  summarize(acc_precip = sum(rain), acc_tt = sum(tt)) 

if (plotting) {
#comparing conditions over the last ten years at the same site
plot_dt <- filter(wthn_sites, Site %in% site_tag)
ggplot(plot_dt) +
  aes(x = acc_precip, y = acc_tt) +
  geom_vline(aes(xintercept = mean(acc_precip)), color = "black", linetype = "dashed") + 
  geom_hline(aes(yintercept = mean(acc_tt)), color = "black", linetype = "dashed") +
  geom_label(label = plot_dt$year, size = 3L) +
  labs(x = "Acc. Precipitation (mm)",y = "Acc. Thermal Time", 
       title = paste0("Conditions at [",site_tag,"]")) +
  theme_minimal()
}

#comparing conditions over the last ten years, faceted for several sites
means <- wthn_sites %>% group_by(Site) %>%
  summarise(mean_acc_precip = mean(acc_precip),
            mean_acc_tt = mean(acc_tt))

if (plotting) {
ggplot(wthn_sites) +
  geom_vline(data = means, aes(xintercept = mean_acc_precip), color = "black", linetype = "dashed") + 
  geom_hline(data = means, aes(yintercept = mean_acc_tt), color = "black", linetype = "dashed") +
  geom_label(label = wthn_sites$year, size = 3, 
             mapping = aes(x = acc_precip, y = acc_tt, color = year)) +
  labs(x = "Acc. Precipitation (mm)",y = "Acc. Thermal Time (GDD)") +
  theme_minimal() +
  facet_wrap(vars(Site)) +
  theme(legend.position = "none") 

#summarizing conditions over the last ten years, for several sites
plot_dt <- wthn_sites %>% summarize(acc_precip = mean(acc_precip), acc_tt = mean(acc_tt))
ggplot(plot_dt) +
  aes(x = acc_precip, y = acc_tt) +
  geom_vline(aes(xintercept = mean(acc_precip)), color = "black", linetype = "dashed") + 
  geom_hline(aes(yintercept = mean(acc_tt)), color = "black", linetype = "dashed") +
  geom_label(label = plot_dt$Site, size = 3) +
  theme_minimal() +
  labs(x = "Acc. Precipitation (mm)",y = "Acc. Thermal Time (GDD)", 
       title = "10 Year Site Averages for a Typical Growing Season") +
  theme(legend.position = "none") 
}


#hierarchical clustering weights based on variable importance to a trait
#site performance stability
#refine how crop type is handled for custom models, ex: _template files, croptype, creating and copying files, specifying custom files

if (plotting) {


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
}
badp <- c(min(period_durs$Period), badp, max(period_durs$Period))


#create color palette for heatmaps
pal_f <- colorRampPalette(brewer.pal(9,"RdYlBu")) #creates a continuous palette
palette <- rev(pal_f(50)[1:50])


#compare seasonal characteristics of trials, by maturity
matvals <- unique(trials_x$Mat)

ID_corr <- function(matval, final_x) {
  final_dt <- filter(final_x, Mat == matval) %>% 
    select(ID, Rain_2:WaterStress_11) %>% #needs to be genericized 
    
    #grab only numeric variables (no dates)
    select(where(is.numeric)) %>%  
    
    #remove constant parameters
    remove_constant(na.rm = TRUE)  
  
    #remove trials where no data was collected
  final_dt <- remove_empty(final_dt, which = c("rows"), cutoff = 0.9)
  
    #remove acc_emerged_tt because it's redundant
    #final_dt <- select(final_dt, !starts_with("AccEmTT"))
  
    #remove parameters that intersect with discarded periods
    final_dt <- select(final_dt, !ends_with(paste0("_",badp)))
  
    #remove parameters with near zero variance
    nzv_check <- sapply(final_dt, function(x){var(x, na.rm = TRUE)})
    nzv <- names(nzv_check)[nzv_check < 1e-5]
    final_dt <- select(final_dt, !any_of(nzv))
    
    #remove parameters which are autocorrelated, based on the full runs 
    final_full <- filter(final_dt, ID %in% full_run_IDs) %>% column_to_rownames("ID") #subset the data to successful runs
    var_cor <- cor(final_full)
    correlated <- caret::findCorrelation(var_cor, cutoff = 0.90, names = T)
    final_dt <- select(final_dt, !any_of(correlated)) #remove autocorrelated variables
    
    #plot removed autocorrelated variables
    row_annotation <- data.frame(
      Autocorrelation = ifelse(rownames(var_cor) %in% correlated, "Will Be Removed", "Not Removed"))
    rownames(row_annotation) <- rownames(var_cor)
    p1 <- pheatmap(var_cor, annotation_row = row_annotation, cex = 0.75, 
             annotation_colors = list(
               Autocorrelation = c("Will Be Removed" = "red", "Not Removed" = "black")), 
             color = palette, breaks = seq(from = -1, to = 1, length.out = 50))

  #scale the final parameters used for comparison
  scfinal_dt <- final_dt %>%
    column_to_rownames("ID") %>%
    scale() %>% as.data.frame() #scale variables
  
  #plot heatmap of correlation of final parameters
  var_cor2 <- cor(scfinal_dt, use = "pairwise.complete.obs")
  p2 <- pheatmap(var_cor2, main = paste("Parameter Correlations for Mat", matval),
           color = palette, breaks = seq(from = -1, to = 1, length.out = 50))
  
  id_cor <- cor(t(scfinal_dt), use = "pairwise.complete.obs")
  
  nametag <- filter(final_x, ID %in% colnames(id_cor)) %>% 
    select(ID, Site, PlantingDate_Sim) %>%
    mutate(Planting = format(PlantingDate_Sim, "%j/%Y"),
           tag = paste0(ID,": ", Site, " ", Planting)) 
  
  p3 <- pheatmap(id_cor, main = paste("Seasonal Correlations for Mat", matval),
           labels_row = nametag$tag, cex = 0.5,
           color = palette, breaks = seq(from = -1, to = 1, length.out = 50))

  #dendrograms
  pdend <- as.dendrogram(p3$tree_row)
  dend_decoration <- tibble(order = 1:length(labels(pdend)), 
                            ID = as.numeric(labels(pdend))) %>% 
    left_join(select(final_x, ID:Mat))
  pdend <- place_labels(pdend, nametag$tag)
  labels_cex(pdend) <- 0.5
  
  #dendrogram label colors
  unique_sites <- arrange(dend_decoration, Latitude) %>% pull(Site) %>% unique() # Get unique levels of "Site"
  n_sites <- length(unique_sites) # Number of unique sites
  # Generate a color palette (e.g., using RColorBrewer)
  color_palette <- mako(n_sites) 
  # Create a named vector for the color mapping
  color_mapping <- setNames(color_palette, unique_sites)
  # Add a new column with hex color values
  dend_decoration <- dend_decoration %>% mutate(Color = color_mapping[as.character(Site)])
  labels_colors(pdend) <- dend_decoration$Color
  
  par(mai = c(0.5,0,0,1))
  #plot_horiz.dendrogram(pdend, xlim = c(6,0), side = TRUE, horiz = TRUE, center = F)
  p4 <- plot(pdend, horiz = TRUE, center = T)
  par(mai = c(1,1,0,1))
  
  
  return(list(
    "IDs" = colnames(id_cor), #trial IDs
    "nametag" = nametag, #used for labels. it's ID/Site/Planting DOY/Year
    "used_params" = colnames(scfinal_dt),
    "final_dt" = final_dt, #unscaled parameters used for seasonal correlations
    "scfinal_dt" = scfinal_dt, #scaled parameters used for seasonal correlations
    "autocorr_pheatmap" = p1$gtable,
    "used_params_corr_pheatmap" = p2$gtable,
    "id_corr_pheatmap" = p3$gtable,
    "id_dend_obj" = pdend
  ))
}

mid1res <- ID_corr("mid1", final_x)
mid00res <- ID_corr("mid00", final_x)

#CUT THE TREE





#site distances??
sitedistances_dt <- read_csv("C:/Users/cmg3/Box/Gilbert/material/sitedistances_dt.csv")
sitedistances_dt <- filter(sitedistances_dt, Location1 %in% unique_sites, Location2 %in% unique_sites)
dist_dt <- sitedistances_dt %>% select(Location1, Location2, Distance) %>% pivot_wider(names_from = Location2, values_from = Distance)
dist_mx <- as.matrix(data.table(dist_dt), rownames = "Location1")
distance_tree <- hclust(as.dist(dist_mx))
par(mai = c(1,1,1,1))
plot(distance_tree)
}

# #with a quantile color scale
# vallist <- id_cor %>% as.vector() %>% sort() %>% unique()
# break_places <- round(seq(from = 1, to = length(vallist), length.out = 51),0)
# new_breaks <- vallist[break_places]
# pal_f <- colorRampPalette(brewer.pal(9,"RdYlBu")) #creates a continuous palette
# palette <- rev(pal_f(50)[1:50])
# pheatmap(id_cor, color = palette, breaks = new_breaks) 
# 



#accumulated precipitation and thermal time from time of sowing to time of harvest 
#(or end of development for unharvested trials) for each trial/genetics/site
# trial_comp <- select(daily_charact_x, Stage, ID, Rain, ThermalTime) %>% filter(!Stage %in% (c(1, max(Stage)))) %>% 
#   group_by(ID) %>% summarize(acc_precip = sum(Rain), acc_tt = sum(ThermalTime)) %>% 
#   left_join(.,select(trials_x, Site, Genetics, ID, Year))
 

if (plotting){
 
huh <- daily_charact_x %>% select(Stage, ID, Rain, ThermalTime, Date, DOY) %>%
  filter(!Stage %in% (c(1, max(Stage)))) %>% group_by(ID) %>% 
  mutate(AccRain = cumsum(Rain), AccTT = cumsum(ThermalTime), days_after_sowing = DOY - min(DOY)) %>%
  select(ID, Date, DOY, days_after_sowing, AccRain, AccTT)

trial_key <- select(trials_x, ID, Site, Genetics, Mat, PlantingDate_Sim, Year)
huh <- left_join(huh, trial_key) 
esquisser(huh) 

ggplot(huh) +
  aes(x = DOY, y = AccTT, group = ID) +
  geom_line(colour = "#112446") +
  theme_minimal() +
  facet_wrap(vars(Mat))

 
}
 
 
 
 
 
 
 
 