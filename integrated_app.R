library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyBS)
library(shinyjs)
library(DT)
library(pheatmap)
library(daymetr)
library(data.table)
library(RColorBrewer)
library(pheatmap)
library(janitor)
library(tidyverse)
library(esquisse)
library(zip)
library(here)
library(future)
library(promises)
library(apsimx)
library(chirps)
library(nasapower)
library(soilDB)
library(spData)
library(tools)
library(parallel)


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
    sidebarMenu(
      menuItem(
        "Upload and Analyze",
        tabName = "analysis",
        icon = icon("upload")
      )
    ),
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
    tabItems(
      tabItem(
        tabName = "analysis",
        fluidPage(
          h3("UPDATING"),
          textOutput("met_count"),
          textOutput("soil_count"),
          textOutput("sim_count"),
          textOutput("out_count"),
          br(),
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
              )),
            selectInput(
              "weatherAquis",
              "Select Weather Aquisition:",
              choices = c(
                "DAYMET" = "DAYMET",
                "CHIRPS" = "CHIRPS",
                "NASAPOWER" = "NASAPOWER"
              )),
            selectInput(
              "soilAquis",
              "Select Soil Aquisition:",
              choices = c(
                "SSURGO" = "SSURGO",
                "ISRIC" = "ISRIC"
              ))
          ),
          box(
            background = "green",
            actionButton("runAnalysis", "Run Analysis", icon = icon("play")),
          ),
          box(
            h3("Download Results"),
            downloadButton("downloadData", "Download Results")
          )
        
      )),
      tabItem(tabName = "results",
              fluidPage(
                fluidRow(
                  column(width = 5,
                         h3("Dataset Descriptions"),
                         p(
                           strong("trials_x:"),
                           " aligns with the input file; contains sim parameters, outcomes, and identifying information."
                         ),
                         p(
                           strong("daily_charact_x:"),
                           " the combined total output of the APSIM simulations; contains the recorded values of the reporting variables for each day of each simulation."
                         ),
                         p(
                           strong("charact_x:"),
                           " the seasonal profile; contains environmental and biological parameters summarized by developmental period."
                         ),
                         p(
                           strong("final_x:"),
                           " joins trials_x and charact_x; contains the full outputs of the seasonal characterization engine in wide format. The naming convention of period-specific parameters is “Variable_Period”, e.g., “Rain_5” is the mean rainfall within the fifth period of development."
                         )),
                  column(
                    width = 7,
                    box(
                      width = 12,
                      h3("Boxplot"),
                      uiOutput("fileSelectPlotUI"),
                      uiOutput("varSelectUI"),
                      plotOutput("boxplot"),
                      downloadButton("downloadBoxplot", "Download Boxplot")
                    )
                  ))
                  ,
                
                div(
                  selectInput(
                    "fileToView",
                    label = h3("View Result Files"),
                    choices = c(
                      "trials_x.csv",
                      "daily_charact_x.csv",
                      "charact_x.csv",
                      "final_x.csv"
                    )
                  )
                ),
                DTOutput("viewData")
              )),
      tabItem(
        tabName = "heatmap",
        fluidPage(
          p(
            "This heatmap visualizes the means of the selected variable by site and genetic group.
              Use the dropdown menus to select the variable and genetic group for analysis."
          ),
          uiOutput("season_heatByUI"),
          uiOutput("season_varHeatmapUI"),
          uiOutput("season_matSelectUI"),
          uiOutput("season_heatmapPlotUI"),
          downloadButton("season_downloadHeatmap", "Download Heatmap")
        )
      ),
      tabItem(
        tabName = "trial_comp",
        fluidPage(
          p(
            "This heatmap visualizes the means of the selected variable by site and genetic group.
              Use the dropdown menus to select the variable and genetic group for analysis."
          ),
          uiOutput("trial_matSelectUI"),
          uiOutput("comp_heatmapPlotUI"),
          downloadButton("trial_downloadHeatmap", "Download Heatmap"),
          plotOutput("dendroPlot"),
          downloadButton("trial_downloadDendro", "Download Dendrogram")
        )
      ),
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
                  ))
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
  
  #codes_dir <- here()
  codes_dir <- "C:/Users/cmg3/Documents/GitHub/SCT"
  input_dir <- paste0(codes_dir,"/input")
  unlink(input_dir,recursive = T) ; dir.create(input_dir)
  output_dir <- paste0(codes_dir,"/apsimx_output")
  setwd(output_dir)
  results_dir <- paste0(output_dir,"/output")

  # Reactive values for storing the analysis state and the selected variable
  #analysisDone <- reactiveVal(FALSE)
  analysisInProgress <- reactiveVal(FALSE)
  analysisDone <- reactiveVal(TRUE)
  
  
  season_heatmap_plot <- reactiveVal(NULL)
  comp_heatmap_plot <- reactiveVal(NULL)
  selectedVariable <- reactiveVal()
  
  #create color palette for heatmaps
  pal_f <- colorRampPalette(brewer.pal(9,"RdYlBu")) #creates a continuous palette
  palette <- rev(pal_f(50)[1:50])
  print("palette")
  
  #select template model ------
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
    updateSiteSelectionUI()
  })
  

# file upload ---- 
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
    updateSiteSelectionUI()
  })
  
# set parameters -------
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
  
# run analysis ----
  observeEvent(input$runAnalysis, {
    req(input$fileUpload, input$modelChoice, !analysisInProgress())
    
    analysisInProgress(TRUE)
    analysisDone(FALSE)
    
    input <- read_csv(list.files(input_dir, pattern = ".csv", full.names = TRUE))
    parms <- tibble(mat_handling = mat_handling(), 
                    weather_aquis = weather_aquis(), 
                    soil_aquis = soil_aquis())
    write_csv(parms, paste0(codes_dir,"/apsimx_output/parameters.csv"))
    setwd(output_dir)
    
    #run the script
    system2("Rscript", args = paste0(codes_dir, "/apsimx.R"), wait = FALSE)
    # This code runs AFTER source() completes
    analysisDone(TRUE)
    analysisInProgress(FALSE)
    count_files()
      
    print("finished analysis")
  })

  #live folder updates ----------
  met_count <- reactiveVal(0)
  soil_count <- reactiveVal(0)
  sim_count <- reactiveVal(0)
  out_count <- reactiveVal(0)
  
  soil_dir <- paste0(codes_dir,"/apsimx_output/soils")
  met_dir <- paste0(codes_dir,"/apsimx_output/met")
  apsim_dir <- paste0(codes_dir,"/apsimx_output/apsim")
  
  count_files <- function(){
    count_list <- list.files(met_dir, pattern = ".met", recursive = FALSE)
    met_count(length(count_list))
    
    count_list <- list.files(soil_dir, pattern = ".soils", recursive = FALSE)
    soil_count(length(count_list))
    
    count_list <- list.files(apsim_dir, pattern = ".apsimx", recursive = TRUE)
    sim_count(length(count_list))
    
    count_list <- list.files(apsim_dir, pattern = ".db", recursive = TRUE)
    out_count(length(count_list))
  }
  
  
  nloc <- reactiveVal(1)
  ntrials <- reactiveVal(1)
  
  # Live folder counting (triggered when analysisInProgress is TRUE)
  observe({
    req(analysisInProgress())  # Ensure that the analysis is in progress
    count_files()
    invalidateLater(5000)
  })
  
  output$met_count <- renderText({
    #count <- met_count()
    paste(met_count()," .met files ()")
  })
  
  output$soil_count <- renderText({
    count <- soil_count()
    paste(count," soil profiles ()")
  })
  
  output$sim_count <- renderText({
    count <- sim_count()
    paste(count," apsimx files generated ()")
  })
  
  output$out_count <- renderText({
    count <- out_count()
    paste(count," apsimx files finished ()")
  })
  
  
  # disable run analysis if analysis is currently in progress ---- 
  observe({
    if (analysisInProgress()) {
      shinyjs::disable("runAnalysis")
    } else {
      shinyjs::enable("runAnalysis")
    }
  })
  
# immediately after analysis ----
  observe({
    req(analysisDone())
    
    tryCatch({
      source(paste0(codes_dir,"/trial_visualization.R"))
    }, error = function(e) {
      # Handle the error here
      cat("An error occurred while sourcing the file:", e$message, "\n")
    })
    print("visuals")
    
    count_files()
    
    trials_x <- read.csv(paste0(results_dir, "/trials_x.csv"))
    
  })
  
#enable rest of the menu ----
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
  
#updateSiteSelectionUI ----
  updateSiteSelectionUI <- function() {
    req(analysisDone())
    sites <- sort(unique(trials_x$Site)) 
    output$siteSelectionUI <- renderUI({
      fluidRow(
        column(width = 12,
               actionButton("selectAllSites", "Select All"),
               actionButton("unselectAllSites", "Unselect All")
        ),
        column(width = 12,
               checkboxGroupInput("selectedSites", "Select Sites", choices = sites, selected = sites[1:2])
        )
      )
    })
  }

  #updateSiteSelectionFacetedUI ----
  updateSiteSelectionFacetedUI <- function() {
    req(analysisDone())
    sites <- sort(unique(trials_x$Site))
    output$siteSelectionUI_faceted <- renderUI({
      fluidRow(
        column(width = 12,
               actionButton("selectAllSites_faceted", "Select All"),
               actionButton("unselectAllSites_faceted", "Unselect All")
        ),
        column(width = 12,
               checkboxGroupInput("selectedSites_faceted", "Select Sites", choices = sites, selected = sites[1:2])
        )
      )
    })
  }

  # update SiteSelectionBetweenUI -----
  updateSiteSelectionBetweenUI <- function() {
    req(analysisDone())
    sites <- sort(unique(trials_x$Site))
    output$siteSelectionUI_between <- renderUI({
      fluidRow(
        column(width = 12,
               actionButton("selectAllSites_between", "Select All"),
               actionButton("unselectAllSites_between", "Unselect All")
        ),
        column(width = 12,
               checkboxGroupInput("selectedSites_between", "Select Sites", choices = sites, selected = sites[1:2])
        )
      )
    })
  }

# selectAllSites and unselectAllSites ----  
  observeEvent(input$selectAllSites, {
    sites <- sort(unique(trials_x$Site))
    updateCheckboxGroupInput(session, "selectedSites", selected = sites)
  })
  
  observeEvent(input$unselectAllSites, {
    updateCheckboxGroupInput(session, "selectedSites", selected = character(0))
  })
  
  # selectAllSites_faceted and unselectAllSites_faceted ----  
  
  observeEvent(input$selectAllSites_faceted, {
    sites <- sort(unique(trials_x$Site))
    updateCheckboxGroupInput(session, "selectedSites_faceted", selected = sites)
  })
  
  observeEvent(input$unselectAllSites_faceted, {
    updateCheckboxGroupInput(session, "selectedSites_faceted", selected = character(0))
  })
  
  # selectAllSites_between and unselectAllSites_between ----  
  observeEvent(input$selectAllSites_between, {
    sites <- sort(unique(trials_x$Site))
    updateCheckboxGroupInput(session, "selectedSites_between", selected = sites)
  })
  
  observeEvent(input$unselectAllSites_between, {
    updateCheckboxGroupInput(session, "selectedSites_between", selected = character(0))
  })  
  

# View Results & Boxplot ----
  ## viewData / view data in tables below boxplot ----  
  output$viewData <- renderDT({
    req(analysisDone())
    
    file_path <- paste0(results_dir, "/", input$fileToView)
    
    if (file.exists(file_path)) {
      data <- read.csv(file_path)
      rdata <- mutate(data, across(where(is.numeric), ~ round(.x, 4)))
      datatable(rdata, escape = FALSE, extensions = 'Buttons', 
                options = list(
                      dom = 'Bfrtip',
                      buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                      scrollX = TRUE
      ))
    } else {
      print("File not found: ", file_to_view)
      return(NULL)
    }
  })
  
  ## fileSelectPlotUI / select file for boxplot ----
  output$fileSelectPlotUI <- renderUI({
    req(analysisDone())
    files <- c("trials_x.csv", "daily_charact_x.csv", "charact_x.csv", "final_x.csv")
    selectInput("fileSelectPlot", "Select File to Plot", choices = files, selected = "charact_x.csv")
  })
  
  ## varSelect_boxplot ----
  output$varSelectUI <- renderUI({
    req(analysisDone())
    file_path <- paste0(results_dir, "/", input$fileSelectPlot)
    if (file.exists(file_path)) {
      data <- read.csv(file_path)
      selectInput("varSelect_boxplot", "Select Variable", choices = names(data)[-1], selected = "Rain")
    }
  })
  
  observeEvent(input$varSelect_boxplot, {
    selectedVariable(input$varSelect_boxplot)
  }, ignoreInit = TRUE)
  
  ## store the generated boxplot for download ----
  boxplot_data <- reactiveVal()
  
  output$boxplot <- renderPlot({
    req(analysisDone(), selectedVariable())
    file_path <- paste0(results_dir, "/", input$fileSelectPlot)
    
    if (file.exists(file_path)) {
      data <- read.csv(file_path)
      
      if (!selected_file %in% c("trials_x.csv","final_x.csv")) {
        trials_x <- read.csv(paste0(results_dir, "/trials_x.csv"))
        data <- left_join(data, trials_x[, c("ID", "Site")], by = "ID")
      }
      
      data$Site <- as.factor(data$Site)  # Ensure Site is treated as a factor
      
      selected_var <- selectedVariable()
      
      # Check if selected_var is in the column names of data
      if(selected_var %in% names(data)) {
        # Create the box plot
        p <- ggplot(data, aes(x = Site, y = .data[[selected_var]], fill = Site)) +
          geom_boxplot() +  # Use geom_boxplot to create a box plot
          labs(x = "Site", y = selected_var) +
          theme(axis.text.x = element_text(angle = 45, hjust = 1),
                legend.position = "none")
        
        boxplot_data(p)  # Store the plot in a reactive value
        print(p)  # Render the plot
      } else {
        print(paste("Error: Variable", selected_var, "not found in data frame"))
      }
    } else {
      print(paste("Error: File", selected_file, "does not exist"))
    }
  })
  
  ## download handler for the boxplot ----
  output$downloadBoxplot <- downloadHandler(
    filename = function() {
      paste0("boxplot-", input$varSelect_boxplot, "-", Sys.Date(), ".png")
    },
    content = function(file) {
      png(file, width = 800, height = 600)
      print(boxplot_data())  # Print the stored plot
      dev.off()
    }
  )
  
  ## disable download results button if no analysis. ----
  
  observe({
    if (analysisDone()) {
      shinyjs::enable("downloadData")
    } else {
      shinyjs::disable("downloadData")
    }
    updateSiteSelectionUI()
    updateSiteSelectionBetweenUI()
    updateSiteSelectionFacetedUI()
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

  ## select file to download ----
  output$fileSelectUI <- renderUI({
    req(analysisDone())
    files <- list.files(results_dir, full.names = FALSE)
    selectInput("fileSelect", "Select File to Download", choices = files)
  })
  
# Seasonal Heatmaps ----  
  ## select maturity for heatmap -----
  output$season_matSelectUI <- renderUI({
    req(analysisDone())
    gen_choices <- unique(trials_x$Mat)
    selectInput(inputId = "season_matSelect", label = "Select Maturity for Heatmap", choices = gen_choices, selected = gen_choices[1])
  })
  
  ## select maturity for heatmap -----
  output$season_heatByUI <- renderUI({
    req(analysisDone())
    gen_choices <- c("By Trial","By Site")
    selectInput(inputId = "season_heatBy", label = "By Site or by Trial?", choices = gen_choices, selected = gen_choices[1])
  })
  
  ## store heatmap for download ----
  output$season_varHeatmapUI <- renderUI({
    req(analysisDone())
    varchoice <- charact_x %>% ungroup() %>% select(where(is.numeric) & !c(ID, Period)) %>% names()
    selectInput("season_heatmapSelect", "Select Variable for Heatmap", choices = varchoice)
  })
  
  ## more heatmap rendering ----
  observe({
    output$season_heatmapPlotUI <- renderUI({
      updateSiteSelectionFacetedUI()
      graphics.off()
      req(input$season_heatmapSelect)  # Ensure there's a selected value
      plotOutput("season_heatmapPlot", height = "600px", width = "90%")
    })
  })

  ## render heatmap ----
  output$season_heatmapPlot <- renderPlot(
    {
      req(input$season_heatmapSelect)  # Ensure a variable is selected
      req(input$season_heatBy)
      req(analysisDone())
      matsel <- input$season_matSelect 
      var <- input$season_heatmapSelect
      heatby <- input$season_heatBy
      
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
                   color = palette,
                   breaks=c(var_mat[1,1]-2,var_mat[1,1]-1,var_mat[1,1]+1,var_mat[1,1]+2),
                   fontsize = 16, 
                   display_numbers = round(var_mat, 2), 
                   number_color = "grey10", 
                   number_format = "%.2f", 
                   legend = F,
                   cluster_cols = F,
                   cluster_rows = T,
                   main = paste0(paste1,var,paste2,matsel,")"))
        } else {
          heatmap <- pheatmap(var_mat,angle_col = 0,
                    fontsize = 16, 
                    color = palette,
                    display_numbers = round(var_mat, 2), 
                    number_color = "grey10", 
                    scale = "column",
                    number_format = "%.2f", 
                    legend = F,
                    cluster_cols = F,
                    cluster_rows = T,
                    main = paste0(paste1,var,paste2,matsel,")"))
        }
        
        season_heatmap_plot(heatmap)
      #list(var_mat = var_mat, var = var)
      
    })

  ## heatmap download handler ----
  output$season_downloadHeatmap <- downloadHandler(
    filename = function() {
      paste0("season-heatmap-", input$season_heatmapSelect, "-", Sys.Date(), ".png")
    },
    content = function(file) {
      # Use the stored heatmap for the download
      png(file, width = 1400, height = 1000)
      grid::grid.draw(season_heatmap_plot()$gtable)  # Draw the stored heatmap
      dev.off()
    }
  )

  # Trial Comparisons ----  
  ## select maturity for comparisons -----
  output$trial_matSelectUI <- renderUI({
    req(analysisDone())
    gen_choices <- unique(trials_x$Mat)
    selectInput(inputId = "trial_matSelect", label = "Select Maturity for Heatmap", choices = gen_choices, selected = gen_choices[1])
  })

  ## function to generate trial comparisons ------
  ID_corr <- function(matsel, final_x, charact_x) {
    
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
    badp <- c(min(period_durs$Period), badp, max(period_durs$Period)) #remove first and last periods and bad periods
    
    #get names of variables to use for comparison
    varn <- charact_x %>% ungroup() %>% select(where(is.numeric) & 
            !c(ID, Period, Period_Start_DOY, Duration, Period_End_DOY)) %>% names()
    
    final_dt <- filter(final_x, Mat == matsel) %>% 
      select(ID, starts_with(varn)) %>% 
      
      #grab only numeric variables (no dates)
      select(where(is.numeric)) %>%  
      
      #remove constant parameters
      remove_constant(na.rm = TRUE)  
    
    #remove trials where no data was collected
    final_dt <- remove_empty(final_dt, which = c("rows"), cutoff = 0.9)
    
    #remove parameters that intersect with discarded periods
    final_dt <- select(final_dt, !ends_with(paste0("_",badp)))
    
    #remove parameters with near zero variance
    nzv_check <- sapply(final_dt, function(x){var(x, na.rm = TRUE)})
    nzv <- names(nzv_check)[nzv_check < 1e-6]
    final_dt <- select(final_dt, !any_of(nzv))
    
    #remove parameters which are autocorrelated, based on the full runs 
    final_full <- filter(final_dt, ID %in% full_run_IDs) %>% column_to_rownames("ID") #subset the data to successful runs
    var_cor <- cor(final_full)
    correlated <- caret::findCorrelation(var_cor, cutoff = 0.95, names = T)
    final_dt <- select(final_dt, !any_of(correlated)) #remove autocorrelated variables
    
    #plot removed autocorrelated variables
    row_annotation <- data.frame(
      Autocorrelation = ifelse(rownames(var_cor) %in% correlated, " Will Be Removed", " Not Removed"))
    rownames(row_annotation) <- rownames(var_cor)
    p1 <- pheatmap(var_cor, annotation_row = row_annotation, cex = 0.75, 
                   annotation_colors = list(
                     Autocorrelation = c(" Will Be Removed" = "red", " Not Removed" = "black")), 
                   color = palette, breaks = seq(from = -1, to = 1, length.out = 50))
    
    #scale the final parameters used for comparison
    scfinal_dt <- final_dt %>%
      column_to_rownames("ID") %>%
      scale() %>% as.data.frame() #scale variables
    
    #list of IDs
    id_list <- final_dt$ID
    
    #plot heatmap of correlation of final parameters
    var_cor2 <- cor(scfinal_dt, use = "pairwise.complete.obs")
    
    if (nrow(var_cor2) >= 2){
    p2 <- pheatmap(var_cor2, main = paste("Parameter Correlations for Mat", matsel),
                   color = palette, breaks = seq(from = -1, to = 1, length.out = 50))
    } else {
    p2 <- pheatmap(var_cor2, main = paste("Parameter Correlations for Mat", matsel),
                     color = palette, breaks = seq(from = -1, to = 1, length.out = 50),
                     cluster_cols = F, cluster_rows = F)
    }
    
    #plot heatmap of correlation of trials by those parameters
    id_cor <- cor(t(scfinal_dt), use = "pairwise.complete.obs")
    
    if (nrow(id_cor) >= 2){
    p3 <- pheatmap(id_cor, main = paste("Seasonal Correlations (Maturity: ", matsel, ")"),
                   labels_row = nametag[id_list,]$tag, cex = 1,
                   color = palette, breaks = seq(from = -1, to = 1, length.out = 50),
                   angle_col = 0)
    
    #dendrograms
    pdend <- as.dendrogram(p3$tree_row)
    
    } else {
      p3 <- pheatmap(id_cor, main = paste("Seasonal Correlations (Maturity: ", matsel, ")"),
                     labels_row = nametag[id_list,]$tag, cex = 1,
                     color = palette, breaks = seq(from = -1, to = 1, length.out = 50),
                     cluster_cols = F, cluster_rows = F, angle_col = 0)
      pdend <- NULL
    }
    
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
  
  out_IDs <- reactiveVal(NULL)
  out_nametag <- reactiveVal(NULL)
  out_used_params <- reactiveVal(NULL)
  out_final_dt <- reactiveVal(NULL)
  out_scfinal_dt <- reactiveVal(NULL)
  out_autocorr_pheatmap <- reactiveVal(NULL)
  out_used_params_corr_pheatmap <- reactiveVal(NULL)
  out_id_corr_pheatmap <- reactiveVal(NULL)
  out_id_dend_obj <- reactiveVal(NULL)

  ## select maturity, run analyses -----
  
  observeEvent(input$trial_matSelect, {
    req(analysisDone())
    req(input$trial_matSelect)
    matsel <- input$trial_matSelect 
    outs <- ID_corr(matsel, final_x = final_x, charact_x = charact_x)
    
    out_IDs(outs$IDs)
    out_nametag(outs$nametag)
    out_used_params(outs$used_params)
    out_final_dt(outs$final_dt)
    out_scfinal_dt(outs$scfinal_dt)
    out_autocorr_pheatmap(outs$autocorrpheatmap)
    out_used_params_corr_pheatmap(outs$used_params_corr_pheatmap)
    out_id_corr_pheatmap(outs$id_corr_pheatmap)
    out_id_dend_obj(outs$id_dend_obj)
    
  })
  
  ## more trial comp heatmap rendering ----
  observe({
    output$comp_heatmapPlotUI <- renderUI({
      updateSiteSelectionFacetedUI()
      graphics.off()
      plotOutput("comp_heatmapPlot", height = "600px", width = "90%")
    })
  })
  
  ## render trial comp heatmap ----
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
  
  ## heatmap download handler ----
  output$trial_downloadHeatmap <- downloadHandler(
    filename = function() {
      paste0("trial-heatmap-", input$trial_matSelect, "-", Sys.Date(), ".png")
    },
    content = function(file) {
      # Use the stored heatmap for the download
      png(file, width = 1400, height = 1000)
      grid::grid.draw(out_id_corr_pheatmap()$gtable)  # Draw the stored heatmap
      dev.off()
    }
  )
  
  ##render dendrograms -----
  output$dendroPlot <- renderPlot({
    p <- plot(out_id_dend_obj(), horiz = TRUE)
    print(p)
  })
  
  output$trial_downloadDendro <- downloadHandler(
    filename = function() {
      paste0("trial-dendrogram-", input$trial_matSelect, "-", Sys.Date(), ".png")
    },
    content = function(file) {
      # Use the stored heatmap for the download
      png(file, width = 1400, height = 1000)
      grid::grid.draw(out_id_corr_pheatmap()$gtable)  # Draw the stored heatmap
      dev.off()
    }
  )
  
  
  
# Reactive to generate TT/Precip data ----
  accumulatedData <- reactive({
    req(input$selectedSites)
    print(input$selectedSites)
    bigmet <- filter(bigmet, Site %in% input$selectedSites)
    filtmet <- bigmet %>% left_join(mean_startend) %>% filter(day >= first_doy & day <= final_doy)
    dbtw_sites <- filtmet %>% group_by(Site, year) %>% 
      mutate(acc_precip = cumsum(rain), acc_tt = cumsum(tt)) %>%
      ungroup() %>% group_by(Site, day) %>% 
      summarize(acc_precip = mean(acc_precip, na.rm = T), acc_tt = mean(acc_tt, na.rm = T))
    dbtw_sites
  })
  
  
# TT/Precip1 (comparison) ---- 
  ## store the generated daily TT/Precip plot for download ----
  comparison_plot_data <- reactiveVal()
  
  output$comparisonPlot <- renderPlot({
    req(accumulatedData(), input$comparisonType)
    data <- accumulatedData()
    
    sdbtw_sites <- data %>% mutate(day = day - min(day) + 1)
    
    if (input$comparisonType == "precip_doy") {
      p <- ggplot(data)  + 
        aes(x = day, y = acc_precip, colour = Site) +
        geom_line() +
        scale_color_hue(direction = 1) +
        labs(x = "Day of Year", y = "Accumulated Precipitation (mm)") +
        theme_minimal()
    } else if (input$comparisonType == "tt_doy") {
      p <- ggplot(data)  + 
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
  
  ## download handler for the daily TT/Precip plot ----
  output$downloadComparisonPlot <- downloadHandler(
    filename = function() {
      paste0("comparison_plot-", input$comparisonType, "-", Sys.Date(), ".png")
    },
    content = function(file) {
      png(file, width = 1400, height = 1000)
      print(comparison_plot_data())  # Print the stored plot
      dev.off()
    }
  )
  
# TT/Precip2 (facted) ---- 
  ## store the generated TT/Precip 2 plot (faceted) for download ----
  faceted_comparison_plot_data <- reactiveVal()
  
  output$facetedComparisonPlot <- renderPlot({
    req(input$selectedSites_faceted)
    selected_sites <- input$selectedSites_faceted
    
    plot_dt <- wthn_sites %>% filter(Site %in% selected_sites)
    means <- plot_dt %>% group_by(Site) %>%
      summarise(mean_acc_precip = mean(acc_precip, na.rm = TRUE),
                mean_acc_tt = mean(acc_tt, na.rm = TRUE))
    
    p <- ggplot(plot_dt, aes(x = acc_precip, y = acc_tt)) +
      geom_vline(data = means, aes(xintercept = mean_acc_precip), color = "black", linetype = "dashed") +
      geom_hline(data = means, aes(yintercept = mean_acc_tt), color = "black", linetype = "dashed") +
      geom_label(label = plot_dt$year, size = 3, 
                 aes(color = year)) +
      labs(x = "Acc. Precipitation (mm)", y = "Acc. Thermal Time") +
      facet_wrap(vars(Site)) +
      theme_minimal() +
      theme(legend.position = "none")
    
    faceted_comparison_plot_data(p)  # Store the plot in a reactive value
    print(p)  # Render the plot
  })
  
  ## download handler for TT/Precip 2 plot ----
  output$downloadFacetedComparisonPlot <- downloadHandler(
    filename = function() {
      paste0("faceted_comparison_plot-", Sys.Date(), ".png")
    },
    content = function(file) {
      png(file, width = 1200, height = 800)
      print(faceted_comparison_plot_data())  # Print the stored plot
      dev.off()
    }
  )  
  
# TT/Precip3 (between) ---- 
  ## store the generated TT/Precip 3 plot (between) for download ----
  between_sites_plot_data <- reactiveVal()
  
  output$plotBetweenSites <- renderPlot({
    req(analysisDone())
    selected_sites <- input$selectedSites_between
    
    plot_dtt <- wthn_sites %>% 
      filter(Site %in% selected_sites) %>% 
      group_by(Site) %>%
      summarize(acc_precip = mean(acc_precip, na.rm = TRUE),
                acc_tt = mean(acc_tt, na.rm = TRUE))
    
    p <- ggplot(plot_dtt) +
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
  
  ## download handler for TT/Precip 3 plot -----
  output$downloadBetweenSitesPlot <- downloadHandler(
    filename = function() {
      paste0("between_sites_plot-", Sys.Date(), ".png")
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