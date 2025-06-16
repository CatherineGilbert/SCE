library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyBS)
library(shinyjs)
library(DT)
library(geosphere)
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
  title = "Seasonal Characterization Engine",
  skin = "black",
  dashboardHeader(
    title = tags$div(
      style = "display: flex; align-items: center;",
      tags$img(
        src = "aces.png",
        height = "40px",
        style = "margin-right: 10px;"
      ),
      tags$span("Seasonal Characterization Engine", style = "font-size: 12px; font-weight: bold;")
    ),
    titleWidth = 300
  ),
  ## dashboardSidebar ----
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      menuItem(
        "About",
        tabName = "info",
        icon = icon("circle-info")
      ),
      menuItem(
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
                fluidRow(
                box(
                  h3("Input Trial Data"),
                  fileInput(
                    "fileUpload",
                    label = tagList(
                      " Upload Input File:",
                      shiny::span(icon("info-circle"), id = "tip_input")
                    ), accept = c(".csv")
                  ),
                  fileInput(
                    "modelChoice",
                    label = tagList(
                      "Select Template Crop Model:",
                      shiny::span(icon("info-circle"), id = "tip_tempmodel")
                    ), 
                    accept = c(".apsimx")
                  ),
                  selectInput(
                    "matType",
                    label = tagList(
                      "Select Maturity Handling:",
                      shiny::span(icon("info-circle"), id = "tip_mat_hndl")
                    ), 
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
              ),
              div(
                   p("Created by Catherine Gilbert, Sam Shi, and German Mandrini")
              )
              ),
              bsTooltip("tip_input", "A trial dataset with the columns Site, Planting, Genetics, Latitude, and Longitude. Example input data is available in project files; see documentation for more information about formatting.", "right", options = list(container = "body")),
              bsTooltip("tip_tempmodel", "The template model provided here-- its crop module, reporting variables, and management controls-- will be used as the basis for all trial simulations.", "right", options = list(container = "body")),
              bsTooltip("tip_mat_hndl", "How the Genetics column of the input should be translated into the generic cultivars that APSIM uses to define the crop phenology. Soy and Maize are intended to be used with the template models provided. See documentation for more information on these functions. Choosing Direct handling will set the phenology for the simulation using whatever APSIM cultivar name is in the Genetics column.", "right", options = list(container = "body"))
              ),
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
                               tags$strong("trial_info:"),
                               " aligns with the input file; contains sim parameters, outcomes, and identifying information.", 
                               tags$br(),
                               tags$strong("daily_sim_outputs:"),
                               " the combined total output of the APSIM simulations; contains the recorded values of the reporting variables for each day of each simulation.", 
                               tags$br(),
                               tags$strong("seasonal_data:"),
                               " the seasonal profile; contains environmental and biological parameters summarized by developmental period.", 
                               tags$br(),
                               tags$strong("final_x:"),
                               " joins trial_info and seasonal_data; contains the full outputs of the seasonal characterization engine in wide format. The naming convention of period-specific parameters is 'Variable_Period', e.g., 'Rain_5' is the mean rainfall within the fifth period of development.",
                               tags$br(),
                               tags$strong("period_key:"),
                               "  table showing which APSIM stage each Period maps to."
                             )     
                             
                         )
                  ),
                  column(width = 9,
                         h3("Boxplot"),
                         fluidRow(
                           column(width = 6,
                                  selectInput("fileSelectPlot", "Select File to Plot", 
                                              choices = c("trial_info.csv", "daily_sim_outputs.csv", "seasonal_data.csv", "final_x.csv"), selected = "seasonal_data.csv")
                           ),
                           column(width = 6,
                                  uiOutput("varSelectUI")
                           )
                         ),
                         plotOutput("boxplot"),
                         downloadButton("downloadBoxplot", "Download Boxplot")
                  )
                  
                ),
                div(selectInput(
                  "fileToView",
                  label = h3("View Result Files"),
                  choices = c(
                    "trial_info.csv",
                    "daily_sim_outputs.csv",
                    "seasonal_data.csv",
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
          h3("Seasonal Heatmaps"),
          p("This section allows you to create a heatmap to visually compare values of a seasonal parameter 
            across the periods of the crop's development between different trials or sites."),
          p("Use the dropdown menus to select which maturity group to view, the parameter to inspect,
            and whether this comparison should be made between trials or sites. If trials are chosen, 
            the cell values will simply be the recorded values of that parameter for that combination of period 
            and trial. If sites are chosen this will be the means of the same values for each site."
          ),
          p("The X axis of the plot is the developmental period and the Y axis of the plot is the trial/site. 
            Trials are labeled in the format '[Trial ID]: [Site Name] [Date Planted]'.
            Trials/sites are ordered on the Y axis according to hierarchical clustering of the chosen variable,
            as indicated by the dendrogram on the plot's left margin. Cells are colored by value, red/high to blue/low.
            The coloring is scaled relative to the other values recorded in that period for the trials/sites compared (column-wise).
          "),
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
          downloadButton("season_downloadHeatmap", "Download Seasonal Heatmap"),
          downloadButton("season_downloadMatrix", "Download Seasonal Matrix"),
          h3("Period Key"),
          DTOutput("viewKey")
        )
      ),
      ### trial comp UI -----
      tabItem(
        tabName = "trial_comp",
        fluidPage(
          h3("Seasonal Similarity of Trials"),
          p("
            This section allows you to view the similarities of the trials in terms of their seasonal profiles.
            You can select the maturity genetics to use for these comparisons using the drop down menu. 
          "),
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
          downloadButton("download_vardates", "Download Comparison Report"),
          br(),
          p("
            Above is a heatmap showing a similarity matrix created from the correlation of trial seasonal profiles. 
            The matrix is symmetrical: the columns are labeled below with the trial ID, and the rows are labeled to the 
            right in the format [Trial ID:] [Site Name] [Date Planted]. 
          "),
          h3("Dendrogram of Seasonal Similarities"),
          uiOutput("dendroPlotUI"),
          downloadButton("trial_downloadDendro", "Download Dendrogram Plot"),
          downloadButton("downloadDendroObj", "Download Dendrogram Object"),
          p("
            The dendrogram above is the same dendrogram at the sides of the heatmap plot, but pulled out for easier viewing. 
          "),
          h3("Parameter Controls"),
          p("  
            Using the controls below, you can change the decision criteria for including or excluding seasonal
            covariates from the similarity calculations. Below and to the right is a scrollable table of all of the 
            seasonal covariates available for the similarity analysis. The first column, 'Parameter', gives the name of
            the covariate, and the second column, 'Status', gives whether or not it was included in the analysis and 
            on what criteria. The last column, 'Override', gives you the option to override whatever other criteria you set and 
            forcibly include or exclude the parameter from the analysis. The 'Apply Overrides' button *must* be used to apply changes.
            The button on the bottom right can be used to download this table. 
          "),
          fluidRow(
            column(width = 4,
                   checkboxInput("exclude_startend", 
                                 tagList(
                                   "Exclude start and end periods before sowing / after harvest", 
                                   shiny::span(icon("info-circle"), id = "tip_exclude_startend")
                                 ), 
                                 value = TRUE),
                   
                   numericInput("min_dur", 
                                label = tagList(
                                  "Min period duration:", 
                                  shiny::span(icon("info-circle"), id = "tip_min_dur")
                                ), 
                                value = 1, min = 0, max = 366, step = 1),
                   
                   numericInput("nzv_chk", 
                                label = tagList(
                                  "Min parameter variance:", 
                                  shiny::span(icon("info-circle"), id = "tip_nzv_chk")
                                ), 
                                value = 1e-6, min = 1e-10, max = 1),
                   
                   numericInput("empty_chk", 
                                label = tagList(
                                  "Min trial data completeness:", 
                                  shiny::span(icon("info-circle"), id = "tip_empty_chk")
                                ), 
                                value = 0.9, min = 0, max = 1, step = 0.01),
                   
                   numericInput("var_chk", 
                                label = tagList(
                                  "Max parameter correlation:", 
                                  shiny::span(icon("info-circle"), id = "tip_var_chk")
                                ), 
                                value = 0.90, min = 0, max = 1, step = 0.01),
                   
                   downloadButton("downloadParamTable", "Download Parameter Table")
            ),
            column(width = 8, 
                   div(
                     id = "scroll-container",
                     uiOutput("customParamTableUI")
                   ),
                   fluidRow(
                     column(width = 6, actionButton("apply_overrides","Apply Overrides", 
                                                    style = "width: 100%; text-align: center;font-weight: bold;")),
                     column(width = 6, actionButton("reset_overrides","Reset Overrides", 
                                                    style = "width: 100%; text-align: center;font-weight: bold;"))
                   ))
          )
        ),
        bsTooltip("tip_exclude_startend", "Remove seasonal covariates outside the strict duration of crop development. Drops seasonal covariates associated with the first and last phenological periods, which contain the two weeks before sowing and the two weeks after harvest respectively.", "right", options = list(container = "body")),
        bsTooltip("tip_min_dur", "Drop seasonal covariates associated with periods that have a mean duration shorter than this value (in days). Useful for removing shortened periods (such as those a day or less in length) which may be part of the APSIM model definition but may not be relevant to the seasonal profile.", "right", options = list(container = "body")),
        bsTooltip("tip_nzv_chk", "Drop seasonal covariates with a variance lower than this value. Used to remove variables with near-zero variance, which are likely uninformative.", "right", options = list(container = "body")),
        bsTooltip("tip_empty_chk", "Drop trials with too much missing data (less than this proportion of their seasonal data is available). In the case that a simulation fails or is cut short, this can be used to remove suspicious trial data.", "right", options = list(container = "body")),
        bsTooltip("tip_var_chk", "Drop highly correlated seasonal covariates. For highly correlated pairs, the variable with the largest mean absolute correlation is removed until all pair-wise correlations in the matrix are below this value.", "right", options = list(container = "body"))
      ),
      ### TT / precip UIs ----
      tabItem(tabName = "gdd_equation",
              fluidPage(
                p(
                  "Modify the GDD equation used to calculate Thermal Time for these charts."
                ),
                div(
                  withMathJax(
                    helpText("$$
      \\text{GDD} = \\frac{\\min(T_{\\text{max daily}}, T_{\\text{upper}}) + \\max(T_{\\text{min daily}}, T_{\\text{base}})}{2} - T_{\\text{base}}
    $$")
                  )
                ),
                numericInput(
                  inputId = "base_temp",
                  label = "Set Base Temperature (C)",
                  value = 10,        
                  min = -100,          
                  max = 100,          
                  step = 1        
                ), 
                numericInput(
                  inputId = "max_temp",
                  label = "Set Upper Limit for Temperature (C)",
                  value = 30,        
                  min = -100,          
                  max = 100,          
                  step = 1        
                ), 
                uiOutput("current_GDD_settings"),
                actionButton(
                  inputId = "recalc_GDD",
                  label = "Recalculate Daily GDD"
                )
              )),
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
                  column(width = 3,  
                         uiOutput("siteSelectionUI"))
                  ,
                  column(
                    width = 9,
                    plotOutput("comparisonPlot"),
                    downloadButton("downloadComparisonPlot", "Download Plot")
                  )
                )
              )),
      tabItem(tabName = "faceted_comparison",
              fluidPage(
                p(
                  "This section provides a faceted comparison of accumulated precipitation and thermal time at a site on a yearly basis. The dashed horizontal line on the graph represents the mean total thermal time for the last ten years, while the dashed vertical line represents the mean total precipitation for the last ten years. Select the sites to visualize the comparison."
                ),
                fluidRow(
                  column(width = 3,
                         uiOutput("siteSelectionUI_faceted")),
                  column(
                    width = 9,
                    plotOutput("facetedComparisonPlot"),
                    downloadButton("downloadFacetedComparisonPlot", "Download Plot")
                  )
                )
              )),
      tabItem(tabName = "between_sites",
              fluidPage(
                p(
                  "This figure shows the 10-year averages of accumulated thermal time and precipitation for a typical growing season at each site. The dashed horizontal line represents the mean total thermal time for all selected sites, while the dashed vertical line represents the mean total precipitation for all selected sites. "
                ),
                fluidRow(
                  column(width = 3,
                         uiOutput("siteSelectionUI_between")),
                  column(
                    width = 9,
                    plotOutput("plotBetweenSites"),
                    downloadButton("downloadBetweenSitesPlot", "Download Plot")
                  )
                )
              )),
      tabItem(tabName = "info",
              fluidPage(
                p("info")
              ))
    )
  )
)

# Define server logic ----
server <- function(input, output, session) {
  
  codes_dir <- here()
  input_dir <- paste0(codes_dir,"/input")
  unlink(input_dir,recursive = T) ; dir.create(input_dir)
  
  # Reactive values for storing the analysis state and the selected variable
  #analysisDone <- reactiveVal(FALSE)
  analysisInProgress <- reactiveVal(FALSE)
  analysisDone <- reactiveVal(TRUE)
  
  output_dir <- paste0(codes_dir,"/output_files")
  if(!dir.exists(output_dir)) {dir.create(output_dir)}
  setwd(output_dir)
  results_dir <- paste0(output_dir,"/results")
  
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
      write_csv(parms, paste0(codes_dir,"/output_files/parameters.csv"))
      
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
  soil_dir <- paste0(codes_dir,"/output_files/soils")
  met_dir <- paste0(codes_dir,"/output_files/met")
  apsim_dir <- paste0(codes_dir,"/output_files/apsim")
  
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
  filtmet <- reactiveVal(NULL)
  base_temp <- reactiveVal()
  max_temp <- reactiveVal()
  
  
  ## immediately after analysis ----
  observe({
    req(analysisDone())
    
    trial_info <<- read_csv(paste0(results_dir, "/trial_info.csv"))
    final_x <<- read_csv(paste0(results_dir, "/final_x.csv"))
    seasonal_data <<- read_csv(paste0(results_dir, "/seasonal_data.csv"))
    daily_sim_outputs <<- read_csv(paste0(results_dir, "/daily_sim_outputs.csv"))
    period_key <<- read_csv(paste0(results_dir, "/period_key.csv"))
    
    nametag <<- select(final_x, ID, Site, PlantingDate_Sim, Mat) %>% 
      mutate(tag = paste0(ID,": ", Site, " ", PlantingDate_Sim),
             mtag = paste0(ID,": ", Mat, " ", Site, " ", PlantingDate_Sim)) 
    
    #start and end of simulation as doy, going over 365 if wrapping over the new year
    startend <- select(trial_info, Site, Year, ID, Mat, PlantingDate_Sim, HarvestDate_Sim) %>%
      mutate(first_doy = yday(PlantingDate_Sim), 
             until_final =  as.numeric(as_date(HarvestDate_Sim) - as_date(PlantingDate_Sim)),
             final_doy = first_doy + until_final) #done this way because final_doy can go over 365
    #mean start doy and end doy for each site
    mean_startend <<- group_by(startend, Site) %>% 
      summarize(first_doy = mean(first_doy, na.rm = T), final_doy = mean(final_doy, na.rm = T))
    
    #get thermal time and precip for the last ten years of records
    prev_year <- as.numeric(substr(Sys.time(),1,4)) - 1
    bigmet <- data.frame()
    for(s in 1:max(trial_info$ID_Loc)){
      try({
        lil_met <- read_apsim_met(paste0("./met/loc_",s,".met"), verbose = F) %>% as_tibble() %>%
          filter(year >= prev_year - 9, year <= prev_year) %>% mutate(ID_Loc = s)
        bigmet <- bind_rows(bigmet, lil_met)
      })
    }
    bigmet <- trial_info %>% select(Site, ID_Loc) %>% distinct() %>% left_join(bigmet) %>% group_by(Site, ID_Loc, year, day)
    bigmet <<- bigmet
    
    base_temp(input$base_temp)
    max_temp(input$max_temp)
    
    bigmet_gdd <- mutate(bigmet, tt = max((min(maxt,max_temp()) + max(mint,base_temp()))/2 - base_temp(),0)) %>% ungroup()
    bigmet_gdd <- bigmet_gdd %>% left_join(mean_startend) %>% filter(day >= first_doy & day <= final_doy)
    filtmet(bigmet_gdd)
    
    #count again 
    nloc(nrow(distinct(select(trial_info, Latitude, Longitude))))
    ntrials(nrow(trial_info))
    count_files()
    
    site_list(sort(unique(trial_info$Site)))
    
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
        "View Seasonal Heatmaps",
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
          "Modify GDD Equation",
          tabName = "gdd_equation",
          icon = icon("calculator")
        ),
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
  
  ## set filtmet from GDD ------------
  
  observeEvent({
    input$recalc_GDD
  }, {
    base_temp(input$base_temp)
    max_temp(input$max_temp)
    bigmet_gdd <- mutate(bigmet, tt = max((min(maxt,input$max_temp) + max(mint,input$base_temp))/2 - input$base_temp,0)) %>% ungroup()
    bigmet_gdd <- bigmet_gdd %>% left_join(mean_startend) %>% filter(day >= first_doy & day <= final_doy)
    filtmet(bigmet_gdd)
  })
  
  
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
                   "trial_info.csv" = trial_info,
                   "daily_sim_outputs.csv" = daily_sim_outputs,
                   "seasonal_data.csv" = seasonal_data,
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
  
  selectedVariable <- reactiveVal()
  
  ## varSelect_boxplot ----
  output$varSelectUI <- renderUI({
    req(analysisDone(), input$fileSelectPlot)
    data <- switch(input$fileSelectPlot,
                   "trial_info.csv" = trial_info,
                   "daily_sim_outputs.csv" = daily_sim_outputs,
                   "seasonal_data.csv" = seasonal_data,
                   "final_x.csv" = final_x,
                   "period_key.csv" = period_key)
    
    selectInput("varSelect_boxplot", "Select Variable", choices = names(data)[-1])
    
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
                   "trial_info.csv" = trial_info,
                   "daily_sim_outputs.csv" = daily_sim_outputs,
                   "seasonal_data.csv" = seasonal_data,
                   "final_x.csv" = final_x,
                   "period_key.csv" = period_key)
    
    if (!selected_file %in% c("trial_info.csv","final_x.csv")) {
      data <- left_join(data, trial_info[, c("ID", "Site")], by = "ID")
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
  
  
  # Seasonal Heatmaps ----  
  
  season_heatmap_plot <- reactiveVal(NULL)
  season_heatmap_matrix <- reactiveVal(NULL)
  
  ## select maturity for heatmap -----
  output$season_matSelectUI <- renderUI({
    gen_choices <- unique(trial_info$Mat)
    selectInput(inputId = "season_matSelect", label = "Select Maturity", choices = gen_choices, selected = gen_choices[1])
  })
  
  ## select variable for heatmap ----
  output$season_varHeatmapUI <- renderUI({
    varchoice <- seasonal_data %>% ungroup() %>% select(where(is.numeric) & !c(ID, Period)) %>% names()
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
      paste0("season-heatmap_", input$season_matSelect, "_", 
             input$season_varSelect, "_", input$season_heatBy,"_",Sys.Date(), ".png")
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
      paste0("season-matrix_", input$season_matSelect, "_", 
             input$season_varSelect, "_", input$season_heatBy,"_",Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(season_heatmap_matrix(), file)
    }
  )
  
  ## show key for periods -----
  
  output$viewKey <- renderDT({
    req(analysisDone())
    datatable(arrange(period_key, Period),
              rownames = FALSE,
              class = 'compact stripe',
              options = list(
                paging = FALSE,
                searching = FALSE
              ))
  })
  
  
  
  # Trial Comparisons ----  
  
  out_IDs <- reactiveVal(NULL)
  out_nametag <- reactiveVal(NULL)
  out_used_params <- reactiveVal(NULL)
  out_final_dt <- reactiveVal(NULL)
  out_scfinal_dt <- reactiveVal(NULL)
  out_id_cor <- reactiveVal(NULL)
  out_used_params_corr_pheatmap <- reactiveVal(NULL)
  out_id_corr_pheatmap <- reactiveVal(NULL)
  out_id_dend_obj <- reactiveVal(NULL)
  
  pal_f <- colorRampPalette(brewer.pal(9,"RdYlBu")) #creates a continuous palette
  palette <- rev(pal_f(50)[1:50])
  
  param_overrides <- reactiveValues()
  
  ## maturity selection UI ---- 
  output$trial_matSelectUI <- renderUI({
    req(analysisDone())
    gen_choices <- unique(trial_info$Mat)
    current_val <- isolate(input$trial_matSelect)
    if (is.null(current_val) || !(current_val %in% gen_choices)) {
      current_val <- gen_choices[1]
    }
    selectInput(inputId = "trial_matSelect", label = "Select Maturity", choices = gen_choices, selected = current_val)
  })
  
  ## generate trial comparisons ------
  ID_corr <- function(matsel, final_x, seasonal_data, 
                      min_dur = 1, exclude_startend = TRUE,
                      nzv_chk = 1e-6, empty_chk = 0.9, var_chk = 0.9,
                      param_overrides = list()) {
    
    print("Running ID_corr Analysis")
    print(paste("matselin ID_corr function:", matsel))
    trialcex <- input$trial_cex

    #generic remove periods that don't have enough data to be used (remove 6 in this case)
    #filter to trials that ended successfully
    full_run_IDs <- select(final_x, ID, MaxStage) %>% 
      filter(!is.na(MaxStage)) %>%
      filter(MaxStage == max(MaxStage)) %>% pull(ID)
    #find, of successful trials, periods where the mean duration < 1
    period_durs <- select(seasonal_data, ID, Period, Duration) %>% filter(ID %in% full_run_IDs) %>% 
      group_by(Period) %>% summarise(Duration = mean(Duration)) 
    if(any(period_durs$Duration < min_dur)) {
      badp <- filter(period_durs, Duration < min_dur) %>% pull(Period) #define discarded periods as periods with mean duration < 1
    } else {
      badp <- NULL
    }
    
    #get names of variables to use for comparison
    varn <- seasonal_data %>% ungroup() %>% select(where(is.numeric) & 
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
    if (!is.null(badp)) {
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
    
    # === APPLY OVERRIDES ===
    override_kept <- names(param_overrides)[param_overrides == "Keep"]
    override_discarded <- names(param_overrides)[param_overrides == "Discard"]
    
    # Force keep: If it's not already in final_dt, try to add it back (if it exists in full_dt)
    for (p in override_kept) {
      if (!(p %in% names(final_dt)) && (p %in% names(final_dt_locked))) {
        final_dt[[p]] <- final_dt_locked[[p]]
      }
    }
    
    # Force discard: remove from final_dt even if not caught by previous rules
    final_dt <- select(final_dt, -any_of(override_discarded))
    # === END OVERRIDES === 
    
    
    #plot removed variables
    param_status <- data.frame(
      Parameter = full_varlist
    ) %>% mutate(Status = case_when(
      Parameter %in% override_kept ~ "Kept (Override)",
      Parameter %in% override_discarded ~ "Discarded (Override)",
      Parameter %in% startend_vars ~ "Discarded (Start/End Period)",
      Parameter %in% badp_vars ~ "Discarded (Shortened Period)",
      Parameter %in% nzv_vars ~ "Discarded (Low Variance)",
      Parameter %in% correlated_vars ~ "Discarded (Multicollinearity)",
      TRUE ~ "Kept"
    ))
    
    #scale the final parameters used for comparison
    scfinal_dt <- final_dt %>%
      column_to_rownames("ID") %>%
      scale() %>% as.data.frame() #scale variables
    
    #list of IDs
    id_list <- final_dt$ID
    
    #plot heatmap of correlation of final parameters
    var_cor2 <- cor(scfinal_dt, use = "pairwise.complete.obs")
    
    # if (nrow(var_cor2) > 2 & !any(is.na(var_cor2))){
    #   p2 <- pheatmap(var_cor2, main = paste("Parameter Correlations for Mat", matsel),
    #                  fontsize = 16,  legend = F, cex = 0.75,
    #                  color = palette, breaks = seq(from = -1, to = 1, length.out = 50),
    #                  silent = T)
    # } else {
    #   p2 <- pheatmap(var_cor2, main = paste("Parameter Correlations for Mat", matsel),
    #                  fontsize = 16, legend = F, cex = 0.75,
    #                  color = palette, breaks = seq(from = -1, to = 1, length.out = 50),
    #                  cluster_cols = F, cluster_rows = F, silent = T)
    # }
    
    #plot heatmap of correlation of trials by those parameters
    id_cor <- cor(t(scfinal_dt), use = "pairwise.complete.obs")
    
    tagnames <- filter(nametag, ID %in% id_list) %>% pull(tag)
    
    if (nrow(id_cor) > 2 & !any(is.na(id_cor))){ #prevent errors when trying to generate small plots
      
      if (nrow(id_cor) < 100){
        p3 <- pheatmap(id_cor, 
                       main = paste0("Seasonal Correlations (Maturity: ", matsel, ")"),
                       labels_row = tagnames, 
                       cex = 1, 
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
                       angle_col = 0,
                       silent = T)
        
        #dendrograms
        pdend <- as.dendrogram(p3$tree_col)
        
      } else {
        p3 <- pheatmap(id_cor, 
                       main = paste0("Seasonal Correlations (Maturity: ", matsel, ")"),
                       labels_row = tagnames, 
                       cex = 1, 
                       legend = F,
                       fontsize = 16, 
                       fontsize_col = trialcex,
                       fontsize_row = trialcex,
                       fontsize_number = 0.75 * trialcex,
                       color = palette, 
                       breaks = seq(from = -1, to = 1, length.out = 50),
                       angle_col = 0,
                       silent = T)
        
        #dendrograms
        pdend <- as.dendrogram(p3$tree_row)
      }
      
    } else {
      p3 <- pheatmap(id_cor, main = paste0("Seasonal Correlations (Maturity: ", matsel, ")"),
                     labels_row = tagnames, cex = 1,
                     fontsize = 16,
                     fontsize_col = trialcex,
                     fontsize_row = trialcex,
                     fontsize_number = 0.75 * trialcex,
                     display_numbers = round(id_cor, 2), 
                     number_color = "grey10", 
                     legend = F,
                     number_format = "%.2f", 
                     color = palette, breaks = seq(from = -1, to = 1, length.out = 50),
                     cluster_cols = F, cluster_rows = F, angle_col = 0,
                     silent = T)
      pdend <- NULL
    }
    
    out_IDs(colnames(id_cor)) #trial IDs
    out_nametag(nametag) #used for labels. it's ID/Site/Planting DOY/Year
    out_used_params(param_status)
    out_final_dt(final_dt) #unscaled parameters used for seasonal correlations
    out_scfinal_dt(scfinal_dt) #scaled parameters used for seasonal correlations
    out_id_cor(id_cor)
    #out_used_params_corr_pheatmap(p2$gtable)
    out_id_corr_pheatmap(NULL)  # Force a change by clearing first
    out_id_corr_pheatmap(p3$gtable)
    out_id_dend_obj(pdend)
  }
  
  ## function to run ID_corr -----
  run_ID_corr <- function() {
    req(analysisDone())
    req(input$trial_matSelect)
    tryCatch({
      ID_corr(
        matsel = input$trial_matSelect,
        final_x = final_x,
        seasonal_data = seasonal_data,
        min_dur = input$min_dur,
        exclude_startend = input$exclude_startend,
        nzv_chk = input$nzv_chk,
        empty_chk = input$empty_chk,
        var_chk = input$var_chk,
        param_overrides = reactiveValuesToList(param_overrides)
      )
    }, error = function(e) {
      message("Error when running ID_corr:", e$message)
    })
  }
  
  ## run ID_corr if inputs change ----
  observeEvent({
    input$trial_matSelect
    input$min_dur
    input$exclude_startend
    input$nzv_chk
    input$empty_chk
    input$var_chk
    input$trial_cex
  }, {
    run_ID_corr()
  })
  
  
  ## param table container ----
  output$customParamTableUI <- renderUI({
    req(out_used_params())
    
    header <- fluidRow(
      column(4, strong("Parameter")),
      column(4, strong("Status")),
      column(4, strong("Override"))
    )
    
    # dynamically create uiOutput placeholders for each row
    row_outputs <- lapply(out_used_params()$Parameter, function(param_name) {
      uiOutput(outputId = paste0("param_row_", param_name))
    })
    
    div(
      id = "scroll-container",
      style = "
      height: 500px;
      overflow-y: auto;
      overflow-x: hidden;
      border: 1px solid #ccc;
      padding: 10px;
      white-space: normal;
    ",
      tagList(header, tags$hr(), row_outputs)
    )
  })
  
  ## render param table rows -----
  observe({
    req(out_used_params())
    
    for (i in seq_len(nrow(out_used_params()))) {
      local({
        param <- out_used_params()[i, ]
        param_name <- param$Parameter
        output_id <- paste0("param_row_", param_name)
        override_id <- paste0("override_", param_name)
        
        output[[output_id]] <- renderUI({
          override_val <- param_overrides[[param_name]] %||% "None"
          status <- switch(
            override_val,
            "Keep" = "Kept (Override)",
            "Discard" = "Discarded (Override)",
            param$Status
          )
          
          bg_color <- switch(
            status,
            "Kept" = "lightgreen",
            "Kept (Override)" = "lightgreen",
            "Discarded (Override)" = "salmon",
            "Discarded (Start/End Period)" = "salmon",
            "Discarded (Shortened Period)" = "salmon",
            "Discarded (Low Variance)" = "salmon",
            "Discarded (Multicollinearity)" = "salmon",
            "white"
          )
          
          fluidRow(
            style = paste("padding: 5px; background-color:", bg_color),
            column(4, param_name),
            column(4, status),
            column(4,
                   radioGroupButtons(
                     inputId = override_id,
                     label = NULL,
                     choices = c("None", "Keep", "Discard"),
                     selected = override_val,
                     direction = "horizontal",
                     justified = TRUE,
                     size = "sm"
                   )
            )
          )
        })
      })
    }
  })
  
  ## track and store overrides ------
  observe({
    req(out_used_params())
    
    for (param in out_used_params()$Parameter) {
      btn_id <- paste0("override_", param)
      val <- input[[btn_id]]
      
      # skip if input value is missing or override is not yet defined
      if (!is.null(val) && !is.na(val)) {
        current_val <- param_overrides[[param]] %||% "None"
        if (!identical(val, current_val)) {
          isolate({
            param_overrides[[param]] <- val
          })
        }
      }
    }
  })
  
  ## reset button ------
  observeEvent(input$reset_overrides, {
    for (param in names(reactiveValuesToList(param_overrides))) {
      param_overrides[[param]] <- "None"
      updateRadioGroupButtons(session, inputId = paste0("override_", param), selected = "None")
    }
    run_ID_corr()
  })
  
  observeEvent(input$apply_overrides, {
    run_ID_corr()
  })
  
  ## download param table ----
  
  get_current_param_table <- function() {
    req(out_used_params())
    
    # Pull the original table
    base_table <- out_used_params()
    
    # Add override column
    base_table$Override <- sapply(base_table$Parameter, function(param) {
      param_overrides[[param]] %||% "None"
    })
    
    # Compute current effective status
    base_table$EffectiveStatus <- mapply(function(status, override) {
      if (override == "Keep") {
        "Kept (Override)"
      } else if (override == "Discard") {
        "Discarded (Override)"
      } else {
        status
      }
    }, base_table$Status, base_table$Override)
    base_table[, c("Parameter", "Status", "Override", "EffectiveStatus")]
  }
  
  output$downloadParamTable <- downloadHandler(
    filename = function(){
      paste0("param-table_", input$trial_matSelect, "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      param_out <- as_tibble(get_current_param_table())
      print(param_out)
      write.csv(param_out, file)
    }
  )
  
  
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
      
      if (is.null(out_id_corr_pheatmap())) {
        print("Heatmap object is NULL")}
      else {
        print("Plotting heatmap")
        plot(out_id_corr_pheatmap())
      }
    })
  
  ## trial comp heatmap / matrix downloads ----
  output$trial_downloadHeatmap <- downloadHandler(
    filename = function() {
      paste0("sim-heatmap-", input$trial_matSelect, "-", Sys.Date(), ".png")
    },
    content = function(file) {
      # Use the stored heatmap for the download
      png(file, width = 1400, height = 1000)
      grid::grid.draw(out_id_corr_pheatmap()$gtable)  # Draw the stored heatmap
      dev.off()
    }
  )
  
  output$downloadEnvMatrix <- downloadHandler(
    filename = function() {
      paste0("sim-matrix", input$trial_matSelect, "-", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(out_id_cor(), file)
    }
  )
  
  ## render dendrograms -----
  output$dendroPlot <- renderPlot({
    if (is.null(out_id_dend_obj())) {
      print("Dendrogram object is NULL")
    } else {
      
      dend <- out_id_dend_obj()
      #dend_labels <- nametag[as.character(nametag$ID) %in% labels(dend), ]
      #dend_labels <- dend_labels[match(labels(dend), as.character(dend_labels$ID)), ]
      #labels(dend) <- pull(dend_labels, tag)
      
      plot(dend, horiz = TRUE)
    }
  })
  
  observe({
    output$dendroPlotUI <- renderUI({
      plotOutput("dendroPlot", height = "400px", width = "100%")
    })
  })
  
  ## dendrogram downloads ------
  output$trial_downloadDendro <- downloadHandler(
    filename = function() {
      paste0("dendrogram-plot_", input$trial_matSelect, "_", Sys.Date(),".png")
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
      paste0("dendrogram-obj_", input$trial_matSelect, "_", Sys.Date(),".rds")
    },
    content = function(file) {
      write_rds(out_id_dend_obj(), file)
    }
  )
  

## function to create comparison report ----------

  make_vardates <- function(){
    
    seasoncorr_mx <- out_id_cor()
    IDs <- out_IDs()
    rownames(seasoncorr_mx) <- as.character(IDs)
    colnames(seasoncorr_mx) <- as.character(IDs)
    
    #load trails_x
    trials_x <- filter(trial_info, ID %in% IDs) %>%
      mutate(PlantingDOY = yday(PlantingDate_Sim), 
             PD_mday = format(PlantingDate_Sim, "%m/%d"))
    
    #create site distances matrix
    sitedist_mx <- distm(trials_x[, c("Longitude","Latitude")],fun = distHaversine)
    rownames(sitedist_mx) <- pull(trials_x, ID)
    colnames(sitedist_mx) <- pull(trials_x, ID)
    
    yearly <- out_scfinal_dt() %>% rownames_to_column("ID")
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
    
    #consistency between years for [location + planting date] comparisons, based on dates
    vardates <- long_comp %>% group_by(ID.x, ID.y, Site.x, Site.y, PD_mday.x, PD_mday.y) %>%
      summarize(var(season_corr), mean(season_corr),
                "sitedist" = mean(sitedist), 
                "planting_offset" = mean(planting_offset),
                "latitude.x" = mean(Latitude.x),
                "latitude.y" = mean(Latitude.y),
                "lat_offset" = mean(abs(latitude.y- latitude.x)),
                mean(season_diff))
    names(vardates) <- c("ID.x","ID.y","Site.x","Site.y","Planting_Date.x","Planting_Date.y",
                         "Variance_of_Seasonal_Corr","Mean_Seasonal_Corr","Distance_(m)","Planting_Offset_(days)",
                         "Latitude.x","Latitude.y","Latitude_Diff","Mean_Diff_in_Season_Duration_(days)")
    return(vardates)
  }
  

## download comp report -------
  output$download_vardates <- downloadHandler(
    filename = function() {
      paste0("trial-comp_", input$trial_matSelect, "_", Sys.Date(),".csv")
    },
    content = function(file) {
      vardates <- make_vardates()
      print("made vardates")
      write_csv(vardates, file)
    }
  )
  
  
  
  # TT / Precip Charts ----
  
  ## say current GDD values ----
output$current_GDD_settings <- renderText({
  paste0("The current base temp is ",base_temp()," C. The current upper limit temp is ", max_temp()," C.")
})
  
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
    dbtw_sites <- filter(filtmet(), Site %in% input$selectedSites) %>% 
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
        labs(x = "Day of Year", y = "Daily Mean Accumulated Precipitation (mm)", 
             title = "Typical Accumulated Precipitation at a Site by Day of Year") +
        theme_minimal() +
        theme(text = element_text(size = 15))
    } else if (input$comparisonType == "tt_doy") {
      p <- ggplot(dbtw_sites)  + 
        aes(x = day, y = acc_tt, colour = Site) +
        geom_line() +
        scale_color_hue(direction = 1) +
        labs(x = "Day of Year", y = "Daily Mean Accumulated Thermal Time (GDD)",
             title = "Typical Accumulated Thermal Time at a Site by Day of Year") +
        theme_minimal()+
        theme(text = element_text(size = 15))
    } else if (input$comparisonType == "precip_das") {
      p <- ggplot(sdbtw_sites) + 
        aes(x = day, y = acc_precip, colour = Site) +
        geom_line() +
        scale_color_hue(direction = 1) +
        labs(x = "Days after Sowing", y = "Daily Mean Accumulated Precipitation (mm)",
             title = "Typical Accumulated Precipitation at a Site by Days after Sowing") +
        theme_minimal()+
        theme(text = element_text(size = 15))
    } else if (input$comparisonType == "tt_das") {
      p <- ggplot(sdbtw_sites) + 
        aes(x = day, y = acc_tt, colour = Site) +
        geom_line() +
        scale_color_hue(direction = 1) +
        labs(x = "Days after Sowing", y = "Daily Mean Accumulated Thermal Time (GDD)",
             title = "Typical Accumulated Thermal Time at a Site by Days after Sowing") +
        theme_minimal()+
        theme(text = element_text(size = 15))
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
    wthn_sites <- filter(filtmet(), Site %in% selected_sites) %>% 
      ungroup() %>% group_by(Site, year) %>% 
      summarize(acc_precip = sum(rain), acc_tt = sum(tt)) 
    
    means <- wthn_sites %>% group_by(Site) %>%
      summarise(mean_acc_precip = mean(acc_precip, na.rm = TRUE),
                mean_acc_tt = mean(acc_tt, na.rm = TRUE))
    
    p <- ggplot(wthn_sites, aes(x = acc_precip, y = acc_tt)) +
      geom_vline(data = means, aes(xintercept = mean_acc_precip), color = "black", linetype = "dashed") +
      geom_hline(data = means, aes(yintercept = mean_acc_tt), color = "black", linetype = "dashed") +
      geom_label(label = wthn_sites$year, size = 5, 
                 aes(color = year)) +
      labs(x = "Acc. Precipitation (mm)", y = "Acc. Thermal Time (GDD)",
           title = "Total Thermal Time and Precipitation within the Last Ten Seasons") +
      facet_wrap(vars(Site)) +
      theme_minimal() +
      theme(legend.position = "none",
            text = element_text(size = 15))
    
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
    wthn_sites <- filter(filtmet(), Site %in% selected_sites) %>%
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
      geom_label(aes(label = Site), size = 5) +
      theme_minimal() +
      labs(x = "Acc. Precipitation (mm)", y = "Acc. Thermal Time (GDD)", 
           title = "Ten Year Site Averages for a Typical Growing Season") +
      theme(legend.position = "none",
            text = element_text(size = 15))
    
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