library(tidyverse)
library(apsimx)

templ_model_path <- "C:/Users/cmg3/Documents/GitHub/SCE/template_models/Soy_Template.apsimx"
inspect_apsimx(src.dir = "C:/Users/cmg3/Documents/GitHub/SCE/template_models", file = "Soy_Template.apsimx", node = "Crop", )
args(inspect_apsimx)
