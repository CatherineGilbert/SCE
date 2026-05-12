library(tidyverse)
library(apsimx)

templ_model_path <- "C:/Users/cmg3/Documents/GitHub/SCE/template_models/Soy_Template.apsimx"
inspect_apsimx(src.dir = "C:/Users/cmg3/Documents/GitHub/SCE/template_models", file = "Soy_Template.apsimx",
               node = "Other", parm = list("Simulations","Report"))
args(inspect_apsimx)

inspect_apsimx(src.dir = "C:/Users/cmg3/Documents/GitHub/SCE/template_models", file = "Soy_Template.apsimx",
               node = "Other", parm = "Simulations")

trials_df <- read_csv("C:/Users/cmg3/Documents/GitHub/SCE/example_input_files/abc_test.csv") 
trials_df <- mutate(trials_df, ID = row_number()) %>% rename(X = Longitude, Y = Latitude)
locs_df <- select(trials_df, X, Y) %>% distinct() %>% mutate(ID_Loc = row_number())
trials_df <- left_join(trials_df, locs_df, by = join_by(X,Y))


build_period_outputs <- function(daily_sim_outputs, new_config, trial_info) {
  
  RESERVE_VARS <- c("AccRain", "AccTT", "AccEmTT", "Duration", "Period_Start_Date", "Period_End_Date", "Period_Start_DOY", "Period_End_DOY", "Duration", "DOY", "Stage")
  
  sd_out <- daily_sim_outputs %>%
    mutate(Period = as.character(Period)) %>%
    left_join(select(new_config, Period, MergeGroup), by = "Period") %>%
    group_by(ID, MergeGroup) %>%
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
    rename(Period = MergeGroup) %>%
    relocate(ID, Period, Rain) %>% 
    relocate(AccRain, .after = Rain) %>% 
    relocate(AccTT, AccEmTT, .after = ThermalTime) %>%
    relocate(Period_Start_Date, Period_End_Date, Period_Start_DOY, Duration, Period_End_DOY, .after = last_col()) %>%
    arrange(ID, as.numeric(Period)) 
  
  #empty data for missing periods 
  idp <- tidyr::expand(tibble(sd_out), ID, Period) #full list of ID/Period combinations
  idp <- anti_join(idp, sd_out, by = join_by(ID,Period)) #which ID/Period combinations are absent in seasonal_data
  
  if (nrow(idp > 0)){
    col_names <- names(sd_out)[3:length(names(sd_out))]
    for (col in col_names) {
      idp[[col]] <- NA
    }
    idp <- mutate(idp, Duration = 0) #set duration of nonexistent periods to zero
    sd_out <- bind_rows(sd_out, idp) %>% arrange(ID, as.numeric(Period))
  }
  
  fx_out <- pivot_wider(
    sd_out %>% rename(PivotPeriod = Period),
    names_from  = PivotPeriod,
    values_from = setdiff(names(sd_out), c("ID", "Period"))
  ) %>%
    right_join(trial_info, ., by = "ID")
  
  list(seasonal_data = sd_out, final_x = fx_out, period_key = build_period_key(new_config))
}