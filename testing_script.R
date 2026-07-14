library(tidyverse)
library(apsimx)
library(viridis)

daily_sim_outputs <- read_csv("output_files/results/daily_sim_outputs.csv")

dt <- select(daily_sim_outputs, Rain, ID, DOY, Period) 

dt$Rain <- cut(dt$Rain, breaks = quantile(dt$Rain, probs = c(0, 0.1, 0.25, 0.75, 0.9, 1), na.rm = T),
               include.lowest = T, right = F)

dt <- dt %>% group_by(ID, Period) %>% count(Rain) %>% mutate(sum = sum(n), per = n/sum)



library(dplyr)
library(purrr)

data <- daily_sim_outputs
vars <- select(daily_sim_outputs, Rain:WaterStress) %>% names()


period_freq_multi <- function(data, vars,
                              probs = c(0, 0.1, 0.25, 0.75, 0.9, 1)) {
  
  dt <- purrr::map_dfr(vars, function(v) {
    
    dt <- data %>%
      dplyr::select(ID, DOY, Period, value = all_of(v))
    
    # Compute variable-specific breaks
    breaks <- sort(unique(quantile(dt$value,
                                   probs = probs,
                                   na.rm = TRUE)))
    
    # Bin the values
    if (length(breaks) < 2) {
      dt$Bin <- factor("Constant")
    } else {
      dt$Bin <- cut(
        dt$value,
        breaks = breaks,
        include.lowest = TRUE,
        right = FALSE
      )
    }
    
    # Save the possible bins for this variable
    bin_levels <- levels(dt$Bin)
    
    dt %>%
      count(ID, Period, Bin, name = "n") %>%
      tidyr::complete(
        ID,
        Period,
        Bin = factor(bin_levels, levels = bin_levels),
        fill = list(n = 0)
      ) %>%
      group_by(ID, Period) %>%
      mutate(
        total = sum(n),
        per = if_else(total > 0, n / total, 0), #switch to NA to NA period-ID combos that didn't happen in the sim
        Variable = v
      ) %>%
      ungroup() %>%
      select(Variable, ID, Period, Bin, per)
  })
  
  dt %>%
    distinct(Variable, Period, Bin) %>%
    mutate(EnvMarker = paste0("MK_", row_number())) %>%
    right_join(dt, by = c("Variable", "Period", "Bin"))
}

huh <- period_freq_multi(
  daily_sim_outputs,
  names(select(daily_sim_outputs, Rain:WaterStress))
)

huh <- mutate(huh, fID = as.factor(ID))

library(ggplot2)
ggplot(huh) +
  aes(x = fID, y = EnvMarker, fill = per) +
  geom_tile() +
  scale_fill_viridis_c() +
  theme_minimal()

envmk_key <- select(huh, Variable, Period, Bin, EnvMarker) %>% unique()


widehuh <- select(huh, EnvMarker, ID, per) %>% pivot_wider(names_from = EnvMarker, values_from = per)

huhmx <- widehuh %>% column_to_rownames("ID") %>% t() %>% cor()
