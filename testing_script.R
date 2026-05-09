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