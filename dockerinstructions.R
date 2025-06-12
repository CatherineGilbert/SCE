#library(shiny2docker)
#shiny2docker() #generates dockerfile and renv file for package versions

#remotes::install_github("josiahparry/r-sysreqs")
#library(sysreqs) #makes sure that the system requirements make sense for packages

#download .deb file from APSIMX downloads site, use Linux distrib to get .deb 
#and then extract it to get each of the individual files. put them in a folder with the project.
#we'll have to copy and extract these to the image

get_pkgs_sysreqs(c("shiny", 
"shinydashboard", 
"shinyWidgets", 
"shinyBS", 
"shinyjs", 
"DT", 
"geosphere", 
"pheatmap", 
"apsimx", 
"tidyverse", 
"daymetr", 
"data.table", 
"RColorBrewer", 
"janitor", 
"tidyr", 
"zip", 
"here", 
"future", 
"promises", 
"lubridate", 
"ggplot2", 
"viridisLite", 
"dendextend", 
"scales", 
"grid",
"apsimx", 
"tidyverse", 
"daymetr", 
"chirps", 
"nasapower", 
"data.table", 
"soilDB", 
"spData", 
"here", 
"tools", 
"parallel"), "ubuntu", 24.04) %>% print(n = 100)



#build docker image from docker hub terminal by navigating to directory and using docker build -t sce:first_attempt .

#to start the container run 
#docker run -p 3838:3838 your_image_name

#it should be available at localhost:3838