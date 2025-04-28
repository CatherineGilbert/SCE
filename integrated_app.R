library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyBS)
library(shinyjs)
library(DT)
library(pheatmap)
library(apsimx)
library(tidyverse)
library(daymetr)
library(data.table)
library(RColorBrewer)
library(janitor)
library(tidyr)
library(zip)
library(here)
library(future)
library(promises)
library(lubridate)
library(ggplot2)
library(viridisLite)
library(dendextend)
library(scales)
library(grid)

plan(multisession, workers = 2)

# Define UI ----
ui <- dashboardPage(
  skin = "black",
  dashboardHeader(
    title = tags$div(
      style = "display: flex; align-items: center;",
      tags$img(
        src = "aces.png",
        height = "40px",
        style = "margin-right: 10px;"
      ),
      tags$span("SCE", style = "font-size: 30px; font-weight: bold;")
    ),
    titleWidth = 300
  ),
  ## dashboardSidebar ----
  dashboardSidebar(
    width = 300,
    sidebarMenu(menuItem(
      "Upload and Analyze",
      tabName = "analysis",
      icon = icon("upload")
    )),
    sidebarMenuOutput("reveal_menu")
  ),
  
  
  ## dashbordBody CSS----
  dashboardBody(
    shinyjs::useShinyjs(),
    tags$head(tags$style(
      HTML(
        "
        body, .content-wrapper, .box-body, .main-sidebar, .sidebar-menu, .content {
          font-size: 18px; /* Increase the font size here */
        }
        h1, h2, h3, h4, h5, h6 {
          font-size: 1.25em; /* Adjust the headings' size proportionally */
        }
        .sidebar-menu li a {
          font-size: 18px; /* Adjust sidebar menu font size */
        }
        table {
          white-space: nowrap;
        }
      "
      )
    ),
    tags$script(
      HTML(
        "
        $(document).on('shiny:value', function(event) {
          setTimeout(function() {
            $(window).trigger('resize');
          }, 1000); // Increase the delay if needed
        });
        $(document).on('shiny:sessioninitialized', function(event) {
          $('.sidebar-toggle').on('click', function() {
            setTimeout(function() {
              $(window).trigger('resize');
            }, 250); // Adjust timing if necessary
          });
        });
        // Additional script to ensure heatmap plots resize correctly after rendering
        $(document).on('shiny:value', function(event) {
          if (event.name === 'heatmapPlot') {
            setTimeout(function() {
              $(window).trigger('resize');
            }, 500);
          }
        });
      "
      )
    )),
    
    ## tabItems ----
    ### front page UI ----
    tabItems(
      tabItem(tabName = "analysis",
              fluidPage(
                box(
                  h3("Input Trial Data"),
                  fileInput("fileUpload", "Upload Input File:", accept = c(".csv")),
                  fileInput(
                    "modelChoice",
                    "Select Template Crop Model:",
                    accept = c(".apsimx")
                  ),
                  selectInput(
                    "matType",
                    "Select Maturity Handling:",
                    choices = c(
                      "Soy" = "Soy",
                      "Maize" = "Maize",
                      "Direct" = "Direct"
                    )
                  ),
                  selectInput(
                    "weatherAquis",
                    "Select Weather Aquisition:",
                    choices = c(
                      "DAYMET" = "DAYMET",
                      "CHIRPS" = "CHIRPS",
                      "NASAPOWER" = "NASAPOWER"
                    )
                  ),
                  selectInput(
                    "soilAquis",
                    "Select Soil Aquisition:",
                    choices = c("SSURGO" = "SSURGO",
                                "ISRIC" = "ISRIC")
                  )
                ),
                box(
                  background = "green",
                  div(
                    style = "display: flex; align-items: center; gap: 10px;",
                    actionButton("runAnalysis", "Run Analysis", icon = icon("play")),
                    tags$i(
                      id = "runSpinner",
                      class = "fa fa-spinner fa-spin",
                      style = "display:none; font-size: 24px; color: white;"  # match spinner to your theme
                    )
                  )
                ),
                box(
                  h3("Progress"),
                  verbatimTextOutput("progressLog")
                ),
                box(
                  h3("Download Results"),
                  downloadButton("downloadData", "Download Results")
                )
                
              )),
      ### result view UI ----
      tabItem(tabName = "results",
              fluidPage(
                tags$head(
                  tags$style(HTML("

                    .left-panel {
                      display: flex;
                      flex: 1;
                      border-radius: 8px;
                      height: 100%;
                      flex-direction: column;
                      justify-content: center;
                    }
                    .left-panel p {
                      font-size: 1.05vw;
                      line-height: 1.5;
                      margin: 0;
                    }

                  "))
                ),
                fluidRow(
                  column(width = 3,
                    div(class = "left-panel", 
                        h3("Dataset Descriptions"),
                        p(
                          tags$strong("trials_x:"),
                          " aligns with the input file; contains sim parameters, outcomes, and identifying information.", 
                          tags$br(),
                          tags$strong("daily_charact_x:"),
                          " the combined total output of the APSIM simulations; contains the recorded values of the reporting variables for each day of each simulation.", 
                          tags$br(),
                          tags$strong("charact_x:"),
                          " the seasonal profile; contains environmental and biological parameters summarized by developmental period.", 
                          tags$br(),
                          tags$strong("final_x:"),
                          " joins trials_x and charact_x; contains the full outputs of the seasonal characterization engine in wide format. The naming convention of period-specific parameters is 'Variable_Period', e.g., 'Rain_5' is the mean rainfall within the fifth period of development.",
                          tags$br(),
                          tags$strong("period_key:"),
                          " which APSIM stages do the periods indicate."
                          )     
                        
                    )
                    ),
                    column(width = 9,
                            h3("Boxplot"),
                            fluidRow(
                              column(width = 6,
                                uiOutput("fileSelectPlotUI")
                                ),
                              column(width = 6,
                                     uiOutput("varSelectUI")
                                     )
                              ),
                            plotOutput("boxplot"),
                            downloadButton("downloadBoxplot", "Download Boxplot")
                        ),
                    
                ),
                div(selectInput(
                  "fileToView",
                  label = h3("View Result Files"),
                  choices = c(
                    "trials_x.csv",
                    "daily_charact_x.csv",
                    "charact_x.csv",
                    "final_x.csv",
                    "period_key.csv"
                  )
                )),
                DTOutput("viewData")
              )),
      ### heatmap UI ----
      tabItem(
        tabName = "heatmap",
        fluidPage(
          h3("Seasonal Covariate Heatmaps"),
          p(
            "This section allows the user to create a heatmap for a combination of reporting variable 
            and maturity, with a choice to view the values by trial or the mean values for all the 
            trials of a particular site. The X axis of the plot is the period within which the variable 
            is aggregated and the Y axis of the plot is the site. The sites are ordered on the Y axis 
            according to hierarchical clustering of the chosen variable, as indicated by the hierarchical
            tree on the plot's left margin. The cells of the heatmap are colored according to the values
            of the variable with red indicating higher values and blue indicating lower, with colors
            scaled relative to other values recorded for period at different trials/sites (column-wise).
            "),
          p("Use the dropdown menus to select which maturity group to view, the environmental parameter to inspect,
            and whether this parameter should be compared between individual trials or site summaries."
          ),
          p(
            "The trials are labeled in the format '[Trial ID]: [Site Name] [Date Planted]'."
          ),
          fluidRow(
            column(width = 3,
                   uiOutput("season_matSelectUI")),
            column(width = 3,
                   uiOutput("season_varHeatmapUI")),
            column(width = 3,
                   selectInput(inputId = "season_heatBy", label = "By Trial or by Site?", choices = c("By Trial","By Site"), selected = "By Trial")
            ),
            column(width = 3,
                   numericInput(
                     inputId = "season_cex",
                     label = "Adjust Label Size",
                     value = 16,        
                     min = 0,          
                     max = 50,          
                     step = 1        
                   ))
          ),
          uiOutput("season_heatmapPlotUI"),
          downloadButton("season_downloadHeatmap", "Download SC Heatmap"),
          downloadButton("season_downloadMatrix", "Download SC Matrix"),
          h3("Period Key"),
          DTOutput("viewKey")
        )
      ),
      ### trial comp UI -----
      tabItem(
        tabName = "trial_comp",
        fluidPage(
          h3("Seasonal Similarity of Trials"),
          p(
            ""
          ),
          p(
            "The trials are labeled in the format '[Trial ID]: [Site Name] [Date Planted]'."
          ),
          fluidRow(
            column(width = 3,
                   uiOutput("trial_matSelectUI")),
            column(width = 3,
                   numericInput(
                     inputId = "trial_cex",
                     label = "Adjust Label Size",
                     value = 16,        
                     min = 0,          
                     max = 50,          
                     step = 1        
                   ))
          ),
          uiOutput("comp_heatmapPlotUI"),
          downloadButton(
            "trial_downloadHeatmap",
            "Download Heatmap of Seasonal Correlations"
          ),
          downloadButton("downloadEnvMatrix", "Download Seasonal Correlation Matrix"),
          br(),
          h3("Dendrogram of Seasonal Similarities"),
          uiOutput("dendroPlotUI"),
          downloadButton("trial_downloadDendro", "Download Dendrogram Plot"),
          downloadButton("downloadDendroObj", "Download Dendrogram Object"),
          br(),
          fluidRow(
            column(width = 8, 
                   DTOutput("paramsTable")),
            column(width = 4, 
              checkboxInput("exclude_startend", "Exclude bracketing periods before sowing / after harvest", value = TRUE),
              checkboxInput("exclude_badp", "Exclude developmental periods a day or less in length", value = TRUE),
              numericInput("nzv_chk", "Minimum parameter variance:", value = 1e-6, min = 1e-10, max = 1),
              numericInput("empty_chk", "Minimum trial data completeness:", value = 0.9, min = 0, max = 1, step = 0.01),
              numericInput("var_chk", "Maximum parameter correlation:", value = 0.90, min = 0, max = 1, step = 0.01),
            )
          )
        )
      ),
      ### TT / precip UIs ----
      tabItem(tabName = "daily_between_sites",
              fluidPage(
                p(
                  "This section allows you to compare daily accumulated precipitation and thermal time between different sites. Select the comparison type and sites for analysis."
                ),
                selectInput(
                  "comparisonType",
                  "Select Comparison Type",
                  choices = c(
                    "Acc. Precip. (Day of Year)" = "precip_doy",
                    "Acc. Precip. (Days after Sowing)" = "precip_das",
                    "Acc. Thermal Time (Day of Year)" = "tt_doy",
                    "Acc. Thermal Time (Days after Sowing)" = "tt_das"
                  )
                ),
                fluidRow(
                  column(width = 2,  # Adjust the width as needed
                         uiOutput("siteSelectionUI"))
                  ,
                  column(
                    width = 10,
                    # Adjust the width as needed
                    plotOutput("comparisonPlot"),
                    downloadButton("downloadComparisonPlot", "Download Plot")
                  )
                )
              )),
      tabItem(tabName = "faceted_comparison",
              fluidPage(
                p(
                  "This section provides a faceted comparison of accumulated precipitation and thermal time for different sites. Select the sites to visualize the comparison."
                ),
                fluidRow(
                  column(width = 2,
                         uiOutput("siteSelectionUI_faceted")),
                  column(
                    width = 10,
                    plotOutput("facetedComparisonPlot"),
                    downloadButton("downloadFacetedComparisonPlot", "Download Plot")
                  )
                )
              )),
      tabItem(tabName = "between_sites",
              fluidPage(
                p(
                  "This section visualizes the 10-year site averages for a typical growing season, comparing accumulated precipitation and thermal time between selected sites."
                ),
                fluidRow(
                  column(width = 2,
                         uiOutput("siteSelectionUI_between")),
                  column(
                    width = 10,
                    plotOutput("plotBetweenSites"),
                    downloadButton("downloadBetweenSitesPlot", "Download Plot")
                  )
                )
              ))
    )
  )
)

# Define server logic ----
server <- function(input, output, session) {
  
  codes_dir <- here()
  input_dir <- paste0(codes_dir,"/input")
  unlink(input_dir,recursive = T) ; dir.create(input_dir)
  output_dir <- paste0(codes_dir,"/apsimx_output")
  setwd(output_dir)
  results_dir <- paste0(output_dir,"/output")
  
  # Reactive values for storing the analysis state and the selected variable
  #analysisDone <- reactiveVal(FALSE)
  analysisInProgress <- reactiveVal(FALSE)
  analysisDone <- reactiveVal(TRUE)
  
  selectedVariable <- reactiveVal()
  
  #create color palette for heatmaps
  pal_f <- colorRampPalette(brewer.pal(9,"RdYlBu")) #creates a continuous palette
  palette <- rev(pal_f(50)[1:50])
  print("palette")
  
# Front Page / Analysis ----
  ## select template model ------
  observeEvent(input$modelChoice, {
    tryCatch({
      file.remove(list.files(input_dir, pattern = ".apsimx", full.names = TRUE))
      tmp_path <- paste0(input_dir, "/", input$modelChoice$name)
      file.copy(input$modelChoice$datapath, 
                tmp_path, overwrite = TRUE)
      if (file.exists(tmp_path)) {
        cat("Template model copy successful\n")
      } else {
        cat("Template model copy failed\n")
      }
    }, error = function(e) {
      cat("An error occurred during template model file copy: ", e$message, "\n")
    })
  })
  
  ## file upload ---- 
  observeEvent(input$fileUpload, {
    tryCatch({
      file.remove(list.files(input_dir, pattern = ".csv", full.names = TRUE))
      tmp_path <- paste0(input_dir, "/", input$fileUpload$name)
      file.copy(input$fileUpload$datapath, 
                tmp_path, overwrite = TRUE)
      if (file.exists(tmp_path)) {
        cat("File copy successful\n")
      } else {
        cat("File copy failed\n")
      }
    }, error = function(e) {
      cat("An error occurred during file copy: ", e$message, "\n")
    })
  })
  
  ## set parameters -------
  weather_aquis <- reactiveVal("DAYMET")
  soil_aquis <- reactiveVal("SSURGO")
  mat_handling <- reactiveVal("Soy")
  
  observeEvent(input$matType, {
    mat_handling(input$matType)
  })
  observeEvent(input$soilAquis,{
    soil_aquis(input$soilAquis)
  })
  observeEvent(input$weatherAquis,{
    weather_aquis(input$weatherAquis)
  })
  
  ## set progress counters -------
  nloc <- reactiveVal(NULL)
  ntrials <- reactiveVal(NULL)
  met_count <- reactiveVal(0)
  soil_count <- reactiveVal(0)
  sim_count <- reactiveVal(0)
  out_count <- reactiveVal(0)
  valid_count <- reactiveVal(0)
  
  ## run the analysis ----
  observeEvent(input$runAnalysis, {
    req(input$fileUpload, input$modelChoice)
    
    if (analysisInProgress()) {
      cat("Analysis already in progress.\n")
    } else {
      analysisInProgress(TRUE)
      analysisDone(FALSE)
      
      input <- read_csv(list.files(input_dir, pattern = ".csv", full.names = TRUE))
      
      #clear existing files
      unlink(paste0(output_dir,"/met"),recursive = T) ; dir.create(paste0(output_dir,"/met"))
      unlink(paste0(output_dir,"/soils"),recursive = T) ; dir.create(paste0(output_dir,"/soils"))
      unlink(paste0(output_dir,"/apsim"),recursive = T) ; dir.create(paste0(output_dir,"/apsim"))
      
      #reset counters for the progress updates
      nloc(nrow(distinct(select(input, Latitude, Longitude))))
      ntrials(nrow(input))
      met_count(0)
      soil_count(0)
      sim_count(0)
      out_count(0)
      valid_count(0)
      
      #set parameters
      parms <- tibble(mat_handling = mat_handling(), 
                      weather_aquis = weather_aquis(), 
                      soil_aquis = soil_aquis())
      write_csv(parms, paste0(codes_dir,"/apsimx_output/parameters.csv"))
      
      future({
        cat("Running analysis ...")
        source(paste0(codes_dir,"/apsimx.R"))  # Run the APSIMX analysis
      }) %>% then(function() {
        cat("Analysis finished.\n")
        analysisInProgress(FALSE)
        analysisDone(TRUE)  
        count_files()
      }) %>% catch(function(err) {  # Catch errors gracefully
        cat("Error in analysis:", err$message, "\n")
        analysisInProgress(FALSE)
      })
      
      observe({
        req(analysisInProgress())  # Only count while analysis is running
        count_files()
        invalidateLater(500, session) 
      })
    }
  })
  
  ## live folder updates ----------
  soil_dir <- paste0(codes_dir,"/apsimx_output/soils")
  met_dir <- paste0(codes_dir,"/apsimx_output/met")
  apsim_dir <- paste0(codes_dir,"/apsimx_output/apsim")
  
  # Function to count files in each directory
  count_files <- function() {
    if (met_count() != nloc()) {
      met_count(length(list.files(met_dir, pattern = "\\.met$", recursive = FALSE))) 
    } 
    if (soil_count() != nloc()) {
      # this one's different because soils aren't parallelized and missing soils aren't added as files
      valid_soils <- list.files(soil_dir, pattern = "\\.soils$", recursive = FALSE) 
      if (length(valid_soils) == 0){
        newest_soil <- 0
      } else {
        newest_soil <- unlist(regmatches(valid_soils, gregexpr(pattern = "\\d+", valid_soils))) %>%
          as.numeric() %>% max(na.rm = TRUE)
      }
      soil_count(newest_soil)  
    } 
    if (sim_count() != ntrials()) {
      sim_count(length(list.files(apsim_dir, pattern = "\\.apsimx$", recursive = TRUE)))
    } 
      out_count(length(list.files(apsim_dir, pattern = "\\.db$", recursive = TRUE)))
      valid_count(length(list.files(apsim_dir, pattern = "\\.csv$", recursive = TRUE)))
  }
  
  output$progressLog <- renderText({
    req(analysisDone() || analysisInProgress())
    
    logs <- c()
    
    logs <- c(logs, "Starting ...")
    
    if (met_count() > 0) {
      logs <- c(logs, sprintf("%d .met files collected (%.1f%%)", 
                              met_count(), 100 * met_count() / nloc()))
    }
    
    if (soil_count() > 0) {
      logs <- c(logs, sprintf("%d soil profiles collected (%.1f%%)", 
                              soil_count(), 100 * soil_count() / nloc()))
    }
    
    if (sim_count() > 0) {
      logs <- c(logs, sprintf("%d apsimx files generated (%.1f%%)", 
                              sim_count(), 100 * sim_count() / ntrials()))
    }
    
    if (out_count() > 0) {
      logs <- c(logs, sprintf("%d simulations finished (%.1f%%)\n[[%d sims confirmed successful (%.1f%%)]]",
                              out_count(), 100 * out_count() / ntrials(),
                              valid_count(), 100 * valid_count() / ntrials()))
    }
    
    if (out_count() == ntrials()){
      logs <- c(logs, "Processing ...")
    }
    
    if (analysisDone()) {
      logs <- c(logs, "Finished.")
    }
    
    paste(logs, collapse = "\n")
  })
  
  site_list <- reactiveVal(NULL)
  

  ## immediately after analysis ----
  observe({
    req(analysisDone())
    
    trials_x <<- read_csv(paste0(results_dir, "/trials_x.csv"))
    final_x <<- read_csv(paste0(results_dir, "/final_x.csv"))
    charact_x <<- read_csv(paste0(results_dir, "/charact_x.csv"))
    daily_charact_x <<- read_csv(paste0(results_dir, "/daily_charact_x.csv"))
    period_key <<- read_csv(paste0(results_dir, "/period_key.csv"))
    
    nametag <<- select(final_x, ID, Site, PlantingDate_Sim, Mat) %>% 
      mutate(tag = paste0(ID,": ", Site, " ", PlantingDate_Sim),
             mtag = paste0(ID,": ", Mat, " ", Site, " ", PlantingDate_Sim)) 
    
    #start and end of simulation as doy, going over 365 if wrapping over the new year
    startend <- select(trials_x, Site, Year, ID, Mat, PlantingDate_Sim, HarvestDate_Sim) %>%
      mutate(first_doy = yday(PlantingDate_Sim), 
             until_final =  as.numeric(as_date(HarvestDate_Sim) - as_date(PlantingDate_Sim)),
             final_doy = first_doy + until_final) #done this way because final_doy can go over 365
    #mean start doy and end doy for each site
    mean_startend <<- group_by(startend, Site) %>% 
      summarize(first_doy = mean(first_doy, na.rm = T), final_doy = mean(final_doy, na.rm = T))
    
    #get thermal time and precip for the last ten years of records
    prev_year <- as.numeric(substr(Sys.time(),1,4)) - 1
    bigmet <- data.frame()
    for(s in 1:max(trials_x$ID_Loc)){
      try({
        lil_met <- read_apsim_met(paste0("./met/loc_",s,".met"), verbose = F) %>% as_tibble() %>%
          filter(year >= prev_year - 9, year <= prev_year) %>% mutate(ID_Loc = s)
        bigmet <- bind_rows(bigmet, lil_met)
      })
    }
    bigmet <- trials_x %>% select(Site, ID_Loc) %>% distinct() %>% left_join(bigmet) %>% group_by(Site, ID_Loc, year, day)
    max_temp = 34 #thermal time max temp
    base_temp = 0 #thermal time base temp
    bigmet <- mutate(bigmet, tt = max((min(maxt,max_temp)+max(mint,base_temp))/2 - base_temp,0)) %>% ungroup() 
    
    #seasons limited to average start and end of simulations
    filtmet <<- bigmet %>% left_join(mean_startend) %>% filter(day >= first_doy & day <= final_doy)
    
    #count again 
    nloc(nrow(distinct(select(trials_x, Latitude, Longitude))))
    ntrials(nrow(trials_x))
    count_files()
    
    site_list(sort(unique(trials_x$Site)))

  }) %>% bindEvent(analysisDone())

  ## enable rest of the menu ----
  output$reveal_menu <- renderMenu({
    req(analysisDone())
    
    sidebarMenu(
      menuItem(
        "View Results", 
        tabName = "results", 
        icon = icon("table-list")
      ),
      menuItem(
        "View Heatmaps",
        tabName = "heatmap",
        icon = icon("fire")
      ),
      menuItem(
        "View Trial Similarities",
        tabName = "trial_comp",
        icon = icon("seedling")
      ),
      menuItem(
        "Thermal Time / Precipitation",
        icon = icon("cloud-sun-rain"),
        menuSubItem(
          "Typical TT/Precip Accumulation",
          tabName = "daily_between_sites",
          icon = icon("chart-line")
        ),
        menuSubItem(
          "Site Yearly TT/Precip Totals",
          tabName = "faceted_comparison",
          icon = icon("chart-area")
        ),
        menuSubItem(
          "Ten Year Site TT/Precip Means",
          tabName = "between_sites",
          icon = icon("chart-bar")
        ))
    )
  }
  )
  
  ## show and hide spinner ----
  observe({
    if (analysisInProgress()) {
      shinyjs::show("runSpinner")
    } else {
      shinyjs::hide("runSpinner")
    }
  })
  
## download results ----
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0("results_", Sys.Date(), ".zip")  # Name the zip file
    },
    content = function(file) {
      # Create a temporary directory to store the files
      temp_dir <- tempdir()
      files <- list.files(results_dir, full.names = TRUE)
      
      # Copy the selected files to the temporary directory
      file_paths <- file.path(temp_dir, basename(files))
      file.copy(files, file_paths)
      
      # Create a zip file from the files in the temporary directory
      zip::zipr(file, files = file_paths)
    }
  )
  
  ## disable downloads button if no analysis ----
  observe({
    if (analysisDone()) {
      shinyjs::enable("downloadData")
    } else {
      shinyjs::disable("downloadData")
    }
  })
  
  # View Results & Boxplot ----
  ## viewData / view data in tables below boxplot ----  
  output$viewData <- renderDT({
    req(analysisDone())
    
    data <- switch(input$fileToView,
                               "trials_x.csv" = trials_x,
                               "daily_charact_x.csv" = daily_charact_x,
                               "charact_x.csv" = charact_x,
                               "final_x.csv" = final_x,
                               "period_key.csv" = period_key)
    
    rdata <- mutate(data, across(where(is.numeric), ~ round(.x, 4)))
    datatable(rdata, 
              escape = FALSE,
              class = 'compact stripe',
              options = list(
                scrollX = TRUE
              ))
    
  })
  
  ## fileSelectPlotUI / select file for boxplot ----
  output$fileSelectPlotUI <- renderUI({
    req(analysisDone())
    files <- c("trials_x.csv", "daily_charact_x.csv", "charact_x.csv", "final_x.csv")
    selectInput("fileSelectPlot", "Select File to Plot", choices = files, selected = "charact_x.csv")
  })
  
  ## varSelect_boxplot ----
  output$varSelectUI <- renderUI({
    req(analysisDone(), input$fileSelectPlot)
    data <- switch(input$fileSelectPlot,
                   "trials_x.csv" = trials_x,
                   "daily_charact_x.csv" = daily_charact_x,
                   "charact_x.csv" = charact_x,
                   "final_x.csv" = final_x)
    
    selectInput("varSelect_boxplot", "Select Variable", choices = names(data)[-1], selected = "Rain")
    
  })
  
  observeEvent(input$varSelect_boxplot, {
    selectedVariable(input$varSelect_boxplot)
  }, ignoreInit = TRUE)
  
  ## store the generated boxplot for download ----
  boxplot_data <- reactiveVal()
  
  output$boxplot <- renderPlot({
    req(analysisDone(), selectedVariable())
    selected_file <- input$fileSelectPlot  # Use the selected file
    
    data <- switch(input$fileSelectPlot,
                   "trials_x.csv" = trials_x,
                   "daily_charact_x.csv" = daily_charact_x,
                   "charact_x.csv" = charact_x,
                   "final_x.csv" = final_x)
    
    if (!selected_file %in% c("trials_x.csv","final_x.csv")) {
      data <- left_join(data, trials_x[, c("ID", "Site")], by = "ID")
    }
    
    data$Site <- as.factor(data$Site)  # Ensure Site is treated as a factor
    
    selected_var <- selectedVariable()
    
    # Check if selected_var is in the column names of data
    if(selected_var %in% names(data)) {
      # Create the box plot
      p <- ggplot(data, aes(x = Site, y = .data[[selected_var]], fill = Site)) +
        geom_boxplot() +  # Use geom_boxplot to create a box plot
        labs(x = "Site", y = selected_var, title = paste0("Values of ",as.character(selected_var)," in ", as.character(selected_file), ", by Site")) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              legend.position = "none")
      
      boxplot_data(p)  # Store the plot in a reactive value
      print(p)  # Render the plot
    } else {
      print(paste("Error: Variable", selected_var, "not found in data frame"))
    }
  })
  
  ## download handler for the boxplot ----
  output$downloadBoxplot <- downloadHandler(
    filename = function() {
      paste0("boxplot_", input$varSelect_boxplot, "_", Sys.Date(), ".png")
    },
    content = function(file) {
      png(file, width = 1000, height = 1000)
      print(boxplot_data())  # Print the stored plot
      dev.off()
    }
  )
  
  
  # Seasonal Covariate Heatmaps ----  
  
  season_heatmap_plot <- reactiveVal(NULL)
  season_heatmap_matrix <- reactiveVal(NULL)
  
  ## select maturity for heatmap -----
  output$season_matSelectUI <- renderUI({
    gen_choices <- unique(trials_x$Mat)
    selectInput(inputId = "season_matSelect", label = "Select Maturity", choices = gen_choices, selected = gen_choices[1])
  })
  
  ## select variable for heatmap ----
  output$season_varHeatmapUI <- renderUI({
    varchoice <- charact_x %>% ungroup() %>% select(where(is.numeric) & !c(ID, Period)) %>% names()
    selectInput("season_varSelect", "Select Parameter", choices = varchoice)
  })
  
  ## generate heatmap ----
  output$season_heatmapPlot <- renderPlot(
    {
      req(input$season_varSelect)  # Ensure a variable is selected
      req(input$season_heatBy)
      matsel <- input$season_matSelect 
      var <- input$season_varSelect
      heatby <- input$season_heatBy
      heatcex <- input$season_cex
      
      if (heatby == "By Site") {
        var_mat <- filter(final_x, Mat == matsel) %>% select(ID, Site, starts_with(var)) %>% select(-ID) %>%
          group_by(Site) %>% summarize(across(where(is.numeric), function(x){mean(x,na.rm=T)})) %>%
          column_to_rownames("Site") %>%
          remove_empty(which = "rows") %>%
          as.matrix()
        paste1 <- "Recorded Means of "
        paste2 <- " by Site (Maturity: "
      } else {
        var_mat <- filter(final_x, Mat == matsel) %>% select(ID, starts_with(var)) %>%
          group_by(ID) %>% summarize(across(where(is.numeric), function(x){mean(x,na.rm=T)})) %>%
          left_join(., select(nametag, ID, tag), by = join_by(ID)) %>% 
          select(-ID) %>% column_to_rownames("tag")
        paste1 <- "Recorded Values of "
        paste2 <- " by Trial (Maturity: "
      }
      
      colnames(var_mat) <- 1:ncol(var_mat)
      var_mat <- remove_empty(var_mat, which = "rows") %>% as.matrix()
      var_mat[is.nan(var_mat)] <- NA
      
      if (all(var_mat == var_mat[1,1], na.rm = T)){  #check if matrix is constant
        heatmap <- pheatmap(var_mat, angle_col = 0,
                            #color = palette,
                            breaks=c(var_mat[1,1]-2,var_mat[1,1]-1,var_mat[1,1]+1,var_mat[1,1]+2),
                            fontsize = 16, 
                            fontsize_col = heatcex,
                            fontsize_row = heatcex,
                            fontsize_number =  0.75*heatcex,
                            display_numbers = round(var_mat, 2), 
                            number_color = "grey10", 
                            number_format = "%.2f", 
                            legend = F,
                            cluster_cols = F,
                            cluster_rows = T,
                            main = paste0(paste1,var,paste2,matsel,")"),
                            silent = TRUE)
      } else {
        heatmap <- pheatmap(var_mat,angle_col = 0,
                            fontsize = 16, 
                            fontsize_col = heatcex,
                            fontsize_row = heatcex,
                            fontsize_number = 0.75*heatcex,
                            color = palette,
                            display_numbers = round(var_mat, 2), 
                            number_color = "grey10", 
                            scale = "column",
                            number_format = "%.2f", 
                            legend = F,
                            cluster_cols = F,
                            cluster_rows = T,
                            main = paste0(paste1,var,paste2,matsel,")"),
                            silent = TRUE)
      }
      season_heatmap_plot(heatmap)
      season_heatmap_matrix(var_mat)
      
      side_label <- if (input$season_heatBy == "By Site") "Sites" else "Trials"
      
      # Draw plot and add labels
      grid.newpage()
      # Create a slightly smaller viewport to make room for labels
      pushViewport(viewport(layout = grid.layout(3, 3,
                                                 widths = unit.c(unit(1.5, "lines"), unit(1, "null"), unit(3, "lines")),
                                                 heights = unit.c(unit(1.5, "lines"), unit(1, "null"), unit(3, "lines"))
      )))
      
      # Draw heatmap in center of layout (row 2, col 2)
      pushViewport(viewport(layout.pos.row = 2, layout.pos.col = 2))
      grid.draw(heatmap$gtable)
      popViewport()
      
      # Draw side and bottom labels relative to layout
      
      # Right-side "Trials/Sites" label (col 3, vertically centered)
      pushViewport(viewport(layout.pos.row = 2, layout.pos.col = 3))
      grid.text(side_label,
                x = unit(0.5, "npc"),
                y = unit(0.5, "npc"),
                rot = 270,
                just = "center",
                gp = gpar(fontsize = 16))
      popViewport()
      
      # Bottom "Developmental Periods" label (row 3, centered)
      pushViewport(viewport(layout.pos.row = 3, layout.pos.col = 2))
      grid.text("Developmental Periods",
                x = unit(0.5, "npc"),
                y = unit(0.5, "npc"),
                just = "center",
                gp = gpar(fontsize = 16))
      popViewport()
      
      popViewport() 
    })
  
  ## render heatmap ----
    output$season_heatmapPlotUI <- renderUI({
      graphics.off()
      req(input$season_varSelect)  # Ensure there's a selected value
      plotOutput("season_heatmapPlot", height = "600px", width = "100%")
    })
  
  ## heatmap download handler ----
  output$season_downloadHeatmap <- downloadHandler(
    filename = function() {
      paste0("season-heatmap_", input$season_varSelect, "_", Sys.Date(), ".png")
    },
    content = function(file) {
      # Use the stored heatmap for the download
      png(file, width = 1400, height = 1000)
      
      side_label <- if (input$season_heatBy == "By Site") "Sites" else "Trials"

      grid.newpage()
      # Create a slightly smaller viewport to make room for labels
      pushViewport(viewport(layout = grid.layout(3, 3,
                                                 widths = unit.c(unit(1.5, "lines"), unit(1, "null"), unit(3, "lines")),
                                                 heights = unit.c(unit(1.5, "lines"), unit(1, "null"), unit(3, "lines"))
      )))
      
      # Draw heatmap in center of layout (row 2, col 2)
      pushViewport(viewport(layout.pos.row = 2, layout.pos.col = 2))
      grid.draw(season_heatmap_plot()$gtable)
      popViewport()
      
      # Draw side and bottom labels relative to layout
      
      # Right-side "Trials" label (col 3, vertically centered)
      pushViewport(viewport(layout.pos.row = 2, layout.pos.col = 3))
      grid.text( side_label,
                x = unit(0.5, "npc"),
                y = unit(0.5, "npc"),
                rot = 270,
                just = "center",
                gp = gpar(fontsize = 16))
      popViewport()
      
      # Bottom "Developmental Periods" label (row 3, centered)
      pushViewport(viewport(layout.pos.row = 3, layout.pos.col = 2))
      grid.text("Developmental Periods",
                x = unit(0.5, "npc"),
                y = unit(0.5, "npc"),
                just = "center",
                gp = gpar(fontsize = 16))
      popViewport()
      
      popViewport() 
      
      dev.off()
    }
  )

  ## heatmap matrix download handler ----- 
  output$season_downloadMatrix <- downloadHandler(
    filename = function() {
      paste0("season-matrix_", input$season_matSelectUI, "_", "","_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(season_heatmap_matrix(), file)
    }
  )
  
  ## show key for periods -----
  
  output$viewKey <- renderDT({
    req(analysisDone())
    datatable(period_key,
              rownames = FALSE,
              class = 'compact stripe',
              options = list(
                paging = FALSE,
                searching = FALSE
              ))
  })
  
  
  
  # Trial Comparisons ----  
  
  
  comp_heatmap_plot <- reactiveVal(NULL)
  
  out_IDs <- reactiveVal(NULL)
  out_nametag <- reactiveVal(NULL)
  out_used_params <- reactiveVal(NULL)
  out_final_dt <- reactiveVal(NULL)
  out_scfinal_dt <- reactiveVal(NULL)
  out_id_cor <- reactiveVal(NULL)
  out_used_params_corr_pheatmap <- reactiveVal(NULL)
  out_id_corr_pheatmap <- reactiveVal(NULL)
  out_id_dend_obj <- reactiveVal(NULL)
  
  ## select maturity for comparisons -----
  output$trial_matSelectUI <- renderUI({
    req(analysisDone())
    gen_choices <- unique(trials_x$Mat)
    selectInput(inputId = "trial_matSelect", label = "Select Maturity", choices = gen_choices, selected = gen_choices[1])
  })
  
  ## generate trial comparisons ------
  ID_corr <- function(matsel, final_x, charact_x, 
                      exclude_badp = TRUE, exclude_startend = TRUE,
                      nzv_chk = 1e-6, empty_chk = 0.9, var_chk = 0.9) {
    
    trialcex <- input$trial_cex
    if (FALSE){
      exclude_badp = TRUE
      exclude_startend = TRUE
      nzv_chk = 1e-6
      empty_chk = 0.9
      var_chk = 0.9
      trialcsx = 16
      
      #create color palette for heatmaps
      pal_f <- colorRampPalette(brewer.pal(9,"RdYlBu")) #creates a continuous palette
      palette <- rev(pal_f(50)[1:50])
      print("palette")
    }
  
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
    varn <- charact_x %>% ungroup() %>% select(where(is.numeric) & 
              !c(ID, Period, Period_Start_DOY, Duration, Period_End_DOY)) %>% names()
    
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
    
    #plot heatmap of correlation of final parameters
    var_cor2 <- cor(scfinal_dt, use = "pairwise.complete.obs")
    
    if (nrow(var_cor2) > 2 & !any(is.na(var_cor2))){
      p2 <- pheatmap(var_cor2, main = paste("Parameter Correlations for Mat", matsel), legend = F, 
                     color = palette, breaks = seq(from = -1, to = 1, length.out = 50), silent = T)
    } else {
      p2 <- pheatmap(var_cor2, main = paste("Parameter Correlations for Mat", matsel), legend = F, 
                     color = palette, breaks = seq(from = -1, to = 1, length.out = 50),
                     cluster_cols = F, cluster_rows = F, silent = T)
    }
    
    #plot heatmap of correlation of trials by those parameters
    id_cor <- cor(t(scfinal_dt), use = "pairwise.complete.obs")
    
    tagnames <- filter(nametag, ID %in% id_list) %>% pull(tag)
    
    if (nrow(id_cor) > 2 & !any(is.na(id_cor))){ #prevent errors when trying to generate small plots
      p3 <- pheatmap(id_cor, 
                     main = paste0("Seasonal Correlations (Maturity: ", matsel, ")"),
                     labels_row = tagnames, 
                     legend = F,
                     fontsize = 16, 
                     fontsize_col = trialcex,
                     fontsize_row = trialcex,
                     fontsize_number = 0.75 * trialcex,
                     display_numbers = round(id_cor, 2), 
                     number_color = "grey10", 
                     number_format = "%.2f", 
                     color = palette, 
                     breaks = seq(from = -1, to = 1, length.out = 50),
                     angle_col = 0, silent = T)
      
      #dendrograms
      pdend <- as.dendrogram(p3$tree_row)
      
    } else {
      p3 <- pheatmap(id_cor, main = paste0("Seasonal Correlations (Maturity: ", matsel, ")"),
                     labels_row = tagnames, 
                     fontsize = 16, 
                     fontsize_col = trialcex,
                     fontsize_row = trialcex,
                     fontsize_number = 0.75 * trialcex,                            
                     display_numbers = round(id_cor, 2), 
                     number_color = "grey10", 
                     legend = F,
                     number_format = "%.2f", 
                     color = palette, breaks = seq(from = -1, to = 1, length.out = 50),
                     cluster_cols = F, cluster_rows = F, angle_col = 0, silent = T)
      pdend <- NULL
    }
    
    out_IDs(colnames(id_cor)) #trial IDs
    out_nametag(nametag) #used for labels. it's ID/Site/Planting DOY/Year
    out_used_params(param_status)
    out_final_dt(final_dt) #unscaled parameters used for seasonal correlations
    out_scfinal_dt(scfinal_dt) #scaled parameters used for seasonal correlations
    out_id_cor(id_cor)
    out_used_params_corr_pheatmap(p2$gtable)
    out_id_corr_pheatmap(p3$gtable)
    out_id_dend_obj(pdend)
    
    print(id_list)
    print(nametag)
  }
  
  
  ## select maturity, run analyses -----
  
  observeEvent(
    {
      input$trial_matSelect
      input$exclude_badp
      input$exclude_startend
      input$nzv_chk
      input$empty_chk
      input$var_chk
      input$trial_cex
    },
    {
    tryCatch({
      ID_corr(matsel = input$trial_matSelect, 
              final_x = final_x, 
              charact_x = charact_x,
              exclude_badp = input$exclude_badp, 
              exclude_startend = input$exclude_startend,
              nzv_chk = input$nzv_chk, 
              empty_chk = input$empty_chk, 
              var_chk = input$var_chk)
    }, error = function(e) {
      message("Error when re-rendering:", e$message)
    })
    
  })
  
  ## render trial comp heatmap ----
  observe({
    output$comp_heatmapPlotUI <- renderUI({
      graphics.off()
      plotOutput("comp_heatmapPlot", height = "600px", width = "100%")
    })
  })

  output$comp_heatmapPlot <- renderPlot(
    {
      req(analysisDone())
      req(input$trial_matSelect)
      
      if (is.null(out_id_corr_pheatmap())) {
        print("Heatmap object is NULL")}
      else {
        print("Plotting heatmap")
        plot(out_id_corr_pheatmap())
      }
    })
  
  
 ## render param table -------
  output$paramsTable <- renderDataTable({
    datatable(out_used_params(),
              options = list(
                scrollY = "400px",
                paging = FALSE
              ))  %>%
      formatStyle("Status", 
        backgroundColor = styleEqual(c("Kept", "Discarded (Start/End Period)", 
                                       "Discarded (Shortened Period)", "Discarded (Low Variance)",
                                       "Discarded (Multicollinearity)"), 
                                     c("lightgreen", "salmon", 
                                       "salmon", "salmon", "salmon"))
      )
  })
  
  ## trial comp heatmap / matrix downloads ----
  output$trial_downloadHeatmap <- downloadHandler(
    filename = function() {
      paste0("sim-heatmap_", input$trial_matSelect, "_", Sys.Date(), ".png")
    },
    content = function(file) {
      # Use the stored heatmap for the download
      png(file, width = 2400, height = 2000)
      grid::grid.draw(out_id_corr_pheatmap())  # Draw the stored heatmap
      dev.off()
    }
  )
  
  output$downloadEnvMatrix <- downloadHandler(
    filename = function() {
      paste0("sim-matrix_", input$trial_matSelect, "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      csv <- out_id_cor()
      write.csv(csv, file)
    }
  )
  
  ##render dendrograms -----
  output$dendroPlot <- renderPlot({
    req(input$trial_matSelect)
    if (is.null(out_id_dend_obj())) {
      print("Dendrogram object is NULL")
    } else {
      plot(out_id_dend_obj(), horiz = TRUE)
    }
  })
  
  observe({
    output$dendroPlotUI <- renderUI({
      req(input$trial_matSelect)  # Ensure there's a selected value
      plotOutput("dendroPlot", height = "400px", width = "100%")
    })
  })
  
  ## dendrogram downloads ------
  output$trial_downloadDendro <- downloadHandler(
    filename = function() {
      paste0("dendrogram-plot_", input$trial_matSelect, "_", Sys.Date(), ".png")
    },
    content = function(file) {
      # Use the stored heatmap for the download
      png(file, width = 1400, height = 1000)
      grid::grid.draw(out_id_dend_obj())  # Draw the stored heatmap
      dev.off()
    }
  )
  
  output$downloadDendroObj <- downloadHandler(
    filename = function() {
      paste0("dendrogram-obj_", input$trial_matSelect, "_", Sys.Date(), ".rds")
    },
    content = function(file) {
      write.csv(out_id_dend_obj(), file)
    }
  )
  
  # TT / Precip Charts ----
  
  ## Site selections -------
  ### site selection UIs ----
  
  output$siteSelectionUI <- renderUI({
    fluidRow(
      column(width = 12,
             actionButton("selectAllSites", "Select All"),
             actionButton("unselectAllSites", "Unselect All")
      ),
      column(width = 12,
             checkboxGroupInput("selectedSites", "Select Sites", choices = site_list(), selected = site_list()[1:2])
      )
    )
  })
  
  output$siteSelectionUI_faceted <- renderUI({
    fluidRow(
      column(width = 12,
             actionButton("selectAllSites_faceted", "Select All"),
             actionButton("unselectAllSites_faceted", "Unselect All")
      ),
      column(width = 12,
             checkboxGroupInput("selectedSites_faceted", "Select Sites", choices = site_list(), selected = site_list()[1:2])
      )
    )
  })
  
  output$siteSelectionUI_between <- renderUI({
    fluidRow(
      column(width = 12,
             actionButton("selectAllSites_between", "Select All"),
             actionButton("unselectAllSites_between", "Unselect All")
      ),
      column(width = 12,
             checkboxGroupInput("selectedSites_between", "Select Sites", choices = site_list(), selected = site_list()[1:2])
      )
    )
  })
  
  ### select all / unselect all ----  
  observeEvent(input$selectAllSites, {
    updateCheckboxGroupInput(session, "selectedSites", selected = site_list())
  })
  
  observeEvent(input$unselectAllSites, {
    updateCheckboxGroupInput(session, "selectedSites", selected = character(0))
  })
  
  observeEvent(input$selectAllSites_faceted, {
    updateCheckboxGroupInput(session, "selectedSites_faceted", selected = site_list())
  })
  
  observeEvent(input$unselectAllSites_faceted, {
    updateCheckboxGroupInput(session, "selectedSites_faceted", selected = character(0))
  })
  
  observeEvent(input$selectAllSites_between, {
    updateCheckboxGroupInput(session, "selectedSites_between", selected = site_list())
  })
  
  observeEvent(input$unselectAllSites_between, {
    updateCheckboxGroupInput(session, "selectedSites_between", selected = character(0))
  })  
  
  ## TT/precip1 (comparison) ---- 
  ### store the generated daily TT/precip plot for download ----
  comparison_plot_data <- reactiveVal()
  
  output$comparisonPlot <- renderPlot({
    req(input$comparisonType)
    
    #accumulation of thermal time / precip for an average season at each site
    #doy of sowing/harvest set on average dates based on trials that were input
    dbtw_sites <- filter(filtmet, Site %in% input$selectedSites) %>% 
      group_by(Site, year) %>% 
      mutate(acc_precip = cumsum(rain), acc_tt = cumsum(tt)) %>%
      ungroup() %>% group_by(Site, day) %>% 
      summarize(acc_precip = mean(acc_precip, na.rm = T), acc_tt = mean(acc_tt, na.rm = T))
    #conversion to days after sowing
    sdbtw_sites <- dbtw_sites %>% mutate(day = day - min(day) + 1)
    
    if (input$comparisonType == "precip_doy") {
      p <- ggplot(dbtw_sites)  + 
        aes(x = day, y = acc_precip, colour = Site) +
        geom_line() +
        scale_color_hue(direction = 1) +
        labs(x = "Day of Year", y = "Accumulated Precipitation (mm)") +
        theme_minimal()
    } else if (input$comparisonType == "tt_doy") {
      p <- ggplot(dbtw_sites)  + 
        aes(x = day, y = acc_tt, colour = Site) +
        geom_line() +
        scale_color_hue(direction = 1) +
        labs(x = "Day of Year", y = "Accumulated Thermal Time") +
        theme_minimal()
    } else if (input$comparisonType == "precip_das") {
      p <- ggplot(sdbtw_sites) + 
        aes(x = day, y = acc_precip, colour = Site) +
        geom_line() +
        scale_color_hue(direction = 1) +
        labs(x = "Days after Sowing", y = "Accumulated Precipitation (mm)") +
        theme_minimal()
    } else if (input$comparisonType == "tt_das") {
      p <- ggplot(sdbtw_sites) + 
        aes(x = day, y = acc_tt, colour = Site) +
        geom_line() +
        scale_color_hue(direction = 1) +
        labs(x = "Days after Sowing", y = "Accumulated Thermal Time") +
        theme_minimal()
    }
    
    comparison_plot_data(p)  # Store the plot in a reactive value
    print(p)  # Render the plot
  })
  
  ### download handler for the daily TT/Precip plot ----
  output$downloadComparisonPlot <- downloadHandler(
    filename = function() {
      paste0("comparison-plot_", input$comparisonType, "_", Sys.Date(), ".png")
    },
    content = function(file) {
      png(file, width = 1400, height = 1000)
      print(comparison_plot_data())  # Print the stored plot
      dev.off()
    }
  )
  
  ## TT/precip2 (faceted) ---- 
  ### store the generated TT/precip 2 plot (faceted) for download ----
  faceted_comparison_plot_data <- reactiveVal()
  
  output$facetedComparisonPlot <- renderPlot({
    req(input$selectedSites_faceted)
    selected_sites <- input$selectedSites_faceted

    #cross charts comparing accumulated precip/thermal time
    wthn_sites <- filter(filtmet, Site %in% selected_sites) %>% 
      ungroup() %>% group_by(Site, year) %>% 
      summarize(acc_precip = sum(rain), acc_tt = sum(tt)) 
    
    means <- wthn_sites %>% group_by(Site) %>%
      summarise(mean_acc_precip = mean(acc_precip, na.rm = TRUE),
                mean_acc_tt = mean(acc_tt, na.rm = TRUE))
    
    p <- ggplot(wthn_sites, aes(x = acc_precip, y = acc_tt)) +
      geom_vline(data = means, aes(xintercept = mean_acc_precip), color = "black", linetype = "dashed") +
      geom_hline(data = means, aes(yintercept = mean_acc_tt), color = "black", linetype = "dashed") +
      geom_label(label = wthn_sites$year, size = 3, 
                 aes(color = year)) +
      labs(x = "Acc. Precipitation (mm)", y = "Acc. Thermal Time") +
      facet_wrap(vars(Site)) +
      theme_minimal() +
      theme(legend.position = "none")
    
    faceted_comparison_plot_data(p)  # Store the plot in a reactive value
    print(p)  # Render the plot
  })
  
  ### download handler for TT/precip 2 plot ----
  output$downloadFacetedComparisonPlot <- downloadHandler(
    filename = function() {
      paste0("faceted-comparison-plot_", Sys.Date(), ".png")
    },
    content = function(file) {
      png(file, width = 1400, height = 1000)
      print(faceted_comparison_plot_data())  # Print the stored plot
      dev.off()
    }
  )  
  
  ## TT/precip3 (between) ---- 
  ### store the generated TT/precip 3 plot (between) for download ----
  between_sites_plot_data <- reactiveVal()
  
  output$plotBetweenSites <- renderPlot({
    req(analysisDone())
    selected_sites <- input$selectedSites_between
  
    #cross charts comparing accumulated precip/thermal time
    wthn_sites <- filter(filtmet, Site %in% selected_sites) %>%
      ungroup() %>% group_by(Site, year) %>% 
      summarize(acc_precip = sum(rain), acc_tt = sum(tt)) 
    
    wthn_sites2 <- wthn_sites %>% 
      group_by(Site) %>%
      summarize(acc_precip = mean(acc_precip, na.rm = TRUE),
                acc_tt = mean(acc_tt, na.rm = TRUE))
    
    p <- ggplot(wthn_sites2) +
      aes(x = acc_precip, y = acc_tt) +
      geom_vline(aes(xintercept = mean(acc_precip)), color = "black", linetype = "dashed") + 
      geom_hline(aes(yintercept = mean(acc_tt)), color = "black", linetype = "dashed") +
      geom_label(aes(label = Site), size = 3) +
      theme_minimal() +
      labs(x = "Acc. Precipitation (mm)", y = "Acc. Thermal Time", 
           title = "10 Year Site Averages for a Typical Growing Season") +
      theme(legend.position = "none")
    
    between_sites_plot_data(p)  # Store the plot in a reactive value
    print(p)  # Render the plot
  })
  
  ### download handler for TT/precip 3 plot -----
  output$downloadBetweenSitesPlot <- downloadHandler(
    filename = function() {
      paste0("between-sites-plot_", Sys.Date(), ".png")
    },
    content = function(file) {
      png(file, width = 1400, height = 1000)
      print(between_sites_plot_data())  # Print the stored plot
      dev.off()
    }
  )
  
}

# Run the app ----
shinyApp(ui = ui, server = server)