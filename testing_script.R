library(tidyverse)
library(apsimx)

#need to load final_x and period_x

rsl <- final_x

## Stages plot -------------------------------------------------------------

# Prepare results for plot
rsl_p <- rsl |>
  mutate(Genetics = factor(Genetics),
         Planting_genetics = paste(PlantingDate_Sim, Genetics, sep = "_")) |> # This will be the plot y axis
  select(PlantingDate_Sim, Genetics, HarvestDate_Sim, Planting_genetics, all_of(starts_with("Period_Start_Date"))) |>
  arrange(PlantingDate_Sim, Genetics) |>
  unique() |>
  pivot_longer(cols = starts_with("Period_Start_Date"), names_to = "Period", values_to = "Date") |> # make longer table with each stage per ID as a row
  mutate(Period = gsub("Period_Start_Date_", "", Period)) |> droplevels()

pkey <- mutate(period_key, Period = as.character(Period)) %>% select(Period, Label)


#use UI to define selected labels for this filtering
selected_labels <- pkey$Label

rsl_p2 <- left_join(rsl_p, pkey, by = "Period") %>% 
  filter(Label %in% selected_labels) %>% # allow people to filter to relevant periods
  filter(!is.na(Date)) #remove empty dates

#cp will need to be automatic for how many there are 
cp <- c(viridis(5)[1:4], "gray60") # Colors for Maturity groups

rsl_p2 |>
  mutate(Planting_genetics = factor(Planting_genetics),
         Label = factor(Label, ordered = TRUE)) |> # Turn stage into factor so it shows ordered in plot labels
  ggplot(aes(x = Date, y = Planting_genetics)) +
  geom_errorbarh(aes(xmin = PlantingDate_Sim, xmax = HarvestDate_Sim, color = Genetics),
                 height = 0,
                 position = position_dodge(width = 1)) +
  scale_shape_manual(values = 1:length(unique(rsl_p2$Label))) +
  geom_point(aes(color = Genetics, shape = Label),
             size = 3) +
  scale_x_date(date_breaks = "2 weeks", date_labels = "%b %d %Y") +
  scale_color_manual(values = cp) +
  theme_bigstatsr(size.rel = 0.6) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 0.5, size = 9)) +
  labs(x = "Date",
       y = "",
       title = "Developmental stages by planting time and maturity group",
       color = "Maturity group")

#need ability / download buttons to save this chart as png
#need ability / action buttons to change height in px and cex text size too