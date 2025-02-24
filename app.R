library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyBS)
library(shinyjs)
library(DT)
library(readr)
library(dplyr)
library(pheatmap)
library(apsimx)
library(tidyverse)
library(daymetr)
library(data.table)
library(RColorBrewer)
library(pheatmap)
library(janitor)
library(tidyverse)
library(esquisse)
library(tidyr)
library(zip)
library(here)


# Define UI ----
ui <- dashboardPage(
  dashboardHeader(
    title = tags$div(
      style = "display: flex; align-items: center;",
      tags$img(src = "aces.png", height = "40px", style = "margin-right: 10px;"), # Adjust the height as needed
      tags$span("SCE", style = "font-size: 30px; font-weight: bold;")
    ),
    titleWidth = 300 # Adjust this width based on your needs
  ),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Description", tabName = "description", icon = icon("info")),
      menuItem("Upload and Analyze", tabName = "analysis", icon = icon("upload")),
      menuItem("View Results", tabName = "results", icon = icon("image")),
      menuItem("View Seasonal Heatmap", tabName = "heatmap", icon = icon("fire")),
      menuItem("Typical TT/Precip Accumulation", tabName = "daily_between_sites", icon = icon("chart-line")),
      menuItem("Site Yearly TT/Precip Totals", tabName = "faceted_comparison", icon = icon("chart-area")),
      menuItem("Ten Year Site TT/Precip Means", tabName = "between_sites", icon = icon("chart-bar"))
    ),
    width = 300
  ),
  dashboardBody(
    shinyjs::useShinyjs(),
    tags$head(
      tags$style(HTML("
        body, .content-wrapper, .box-body, .main-sidebar, .sidebar-menu, .content {
          font-size: 18px; /* Increase the font size here */
        }
        h1, h2, h3, h4, h5, h6 {
          font-size: 1.25em; /* Adjust the headings' size proportionally */
        }
        .sidebar-menu li a {
          font-size: 18px; /* Adjust sidebar menu font size */
        }
        .large-text-label .control-label {
          font-size: 24px; /* Adjust the size as needed */
          font-weight: bold;
        }
      ")),
      tags$script(HTML("
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
      "))
    ),
    tabItems(
      tabItem(tabName = "description",
              fluidPage(
                h2("Seasonal Characterization Engine"),
                p("Built in R and Shiny using the apsimr package."),
                p("Purpose: To characterize the growing season at one or more sites according to the crop's response to environmental conditions at those sites."),
                h3("Seasonal Characterization Engine can be used to:"),
                tags$ul(
                  tags$li("Understand environment in terms of the conditions / stressors present at specific stages of the crop's development."),
                  tags$li("Compare seasonal conditions between sites and how those conditions have changed over time."),
                  tags$li("Predict crop phenology and performance from a cultivar's maturity, planting date, and location.")
                ),
                h4("Instructions"),
                p("You can use Upload and Analysis page to upload your input data and run analysis, and download result"),
                p("You can use view result page to view boxplot of each column of final dataset"),
                p("You can use visualizations pages to view aggregated plots and download them"),
                h5("Contact"),
                p("Feel free to reach out if you have any questions, suggestions, or contributions!"),
                p("Sam Shi, mshi17@illinois.edu "),
                p("Catherine Gilbert, cmg3@illinois.edu"),
                p("github page: https://github.com/CatherineGilbert/APSIMX_SeasonalCharacterization")
                
                
              )
      ),
      tabItem(tabName = "analysis",
              fluidPage(
                selectInput("cropType", "Select Crop Type", choices = c("Maize" = "Maize", "Soy" = "Soy"), selected = "Maize"),
                fileInput("fileUpload", "Upload Input File", accept = c(".csv")),
                actionButton("runAnalysis", "Run Analysis", icon = icon("play")),
                downloadButton("downloadData", "Download Results"),
                br(),
                
                h3("Dataset Descriptions"),
                p(strong("trials_x:"), " aligns with the input file and contains sim parameters, identifying information, and values which would be inappropriate to summarize by period. 
"),
                p(strong("daily_charact_x:"), " is the combined total output of the APSIM simulations and contains the recorded values of the reporting variables for each day of each simulation. This data is available to the user if they wish to work with the raw outputs of the tool.
"),
                p(strong("charact_x:"), " contains parameters specific to each developmental period. These are the environmental and biological parameters summarized by period, and the parameters which describe the periods themselves, such as starting and end date. This data is in long format by ID and period. 
"),
                p(strong("final_x:"), " joins the contents of trials_x and charact_x, and contains the full outputs of the seasonal characterization engine in wide format. The naming convention of period-specific parameters is “Variable_Period”, e.g., “Rain_5” is the mean rainfall within the fifth period of development. 
"),
                fluidRow(
                  column(12,
                         progressBar(id = "progressBar", value = 0, display_pct = TRUE)
                  )
                )
              )
      ),
      tabItem(tabName = "results",
              fluidPage(
                div(
                  class = "large-text-label control-label",  
                  tags$label("Boxplot")  
                ),
                uiOutput("fileSelectPlotUI"),
                uiOutput("varSelectUI"),
                plotOutput("boxplot"),
                downloadButton("downloadBoxplot", "Download Boxplot"),  
                div(
                  class = "large-text-label",
                  selectInput("fileToView", "View Result Files", choices = c("trials_x.csv", "daily_charact_x.csv", "charact_x.csv", "final_x.csv"))
                ),
                DTOutput("viewData")
              )
      ),
      tabItem(tabName = "heatmap",
              fluidPage(
                p("This heatmap visualizes the means of the selected variable by site and genetic group. 
              Use the dropdown menus to select the variable and genetic group for analysis."),
                uiOutput("varHeatmapUI"),
                uiOutput("matSelectUI"),
                uiOutput("heatmapPlotUI"),  # Use uiOutput to render the heatmap plot
                downloadButton("downloadHeatmap", "Download Heatmap") 
              )
      ),
      tabItem(tabName = "daily_between_sites",
              fluidPage(p("This section allows you to compare daily accumulated precipitation and thermal time between different sites. Select the comparison type and sites for analysis."),
                        fluidRow(
                          column(width = 10,  # Adjust the width as needed
                                 selectInput("comparisonType", "Select Comparison Type", choices = c(
                                   "Acc. Precip. (Day of Year)" = "precip_doy",
                                   "Acc. Precip. (Days after Sowing)" = "precip_das",
                                   "Acc. Thermal Time (Day of Year)" = "tt_doy",
                                   "Acc. Thermal Time (Days after Sowing)" = "tt_das"
                                 )),
                                 plotOutput("comparisonPlot"),
                                 downloadButton("downloadComparisonPlot", "Download Plot")
                          ),
                          column(width = 3,  # Adjust the width as needed
                                 uiOutput("siteSelectionUI")
                          )
                        )
              )
      ),
      tabItem(tabName = "faceted_comparison",
              fluidPage(
                p("This section provides a faceted comparison of accumulated precipitation and thermal time for different sites. Select the sites to visualize the comparison."),
                fluidRow(
                  column(width = 3,
                         uiOutput("siteSelectionUI_faceted")
                  ),
                  column(width = 9,
                         plotOutput("facetedComparisonPlot"),
                         downloadButton("downloadFacetedComparisonPlot", "Download Plot")
                  )
                )
              )
      ),
      tabItem(tabName = "between_sites",
              fluidPage(
                p("This section visualizes the 10-year site averages for a typical growing season, comparing accumulated precipitation and thermal time between selected sites."),
                fluidRow(
                  column(width = 3,
                         uiOutput("siteSelectionUI_between")
                  ),
                  column(width = 9,
                         plotOutput("plotBetweenSites"),
                         downloadButton("downloadBetweenSitesPlot", "Download Plot")
                  )
                )
              )
      )
    )
  )
)

# Define server logic ----
server <- function(input, output, session) {
  
  # Path to the scripts and results
  #codesPath <- here()
  codesPath <- "C:/Users/cmg3/Documents/GitHub/SCT"
  setwd(paste0(codesPath,"/apsimx_output"))
  resultFolderPath <- paste0(codesPath,"/apsimx_output/output")
  
  # Reactive values for storing the analysis state and the selected variable
  #analysisDone <- reactiveVal(FALSE)
  analysisDone <- reactiveVal(TRUE)
  analysisInProgress <- reactiveVal(FALSE)
  
  heatmap_plot <- reactiveVal(NULL)
  
# things to do immediately after analysis finishes ----
  observe({
    req(analysisDone())
    tryCatch({
      source(paste0(codesPath,"/visualization.R"))
    }, error = function(e) {
      # Handle the error here
      cat("An error occurred while sourcing the file:", e$message, "\n")
    })
  })
 
  
# disable run analysis if analysis is currently in progress ---- 
  observe({
    if (analysisInProgress()) {
      shinyjs::disable("runAnalysis")
    } else {
      shinyjs::enable("runAnalysis")
    }
  })
  
  # for read log file for analysis progress
  progress <- reactiveVal(0)
  
  observe({
    invalidateLater(5000, session)
    setwd(paste0(codesPath,"/apsimx_output"))
    if (file.exists("progress.log")) {
      log_contents <- readLines("progress.log")
      # Update progress based on the log contents
      total_steps <- 16  # Define the total number of steps in the log
      current_step <- length(log_contents)  # Update based on the number of log entries
      progress_value <- round(current_step / total_steps * 100)
      progress(progress_value)
      updateProgressBar(session, id = "progressBar", value = progress_value)
    }
  })
  
  output$progressBar <- renderUI({
    progressBar(id = "progressBar", value = progress(), display_pct = TRUE)
  })
  
  cropType <- reactiveVal("Maize")
  observeEvent(input$cropType, {
    cropType(input$cropType)
  })
  
  selectedVariable <- reactiveVal()


# file upload ---- 
  observeEvent(input$fileUpload, {
    if (!dir.exists(resultFolderPath)) {
      dir.create(resultFolderPath, recursive = TRUE)
    }
    tryCatch({
      file.copy(input$fileUpload$datapath, paste0(resultFolderPath, "/input.csv"), overwrite = TRUE)
      if (file.exists(paste0(resultFolderPath, "/input.csv"))) {
        cat("File copy successful\n")
      } else {
        cat("File copy failed\n")
      }
    }, error = function(e) {
      cat("An error occurred during file copy: ", e$message, "\n")
    })
    updateSiteSelectionUI()
  })
  
  
# run analysis ----
  observeEvent(input$runAnalysis, {
  
    req(input$fileUpload)
    analysisInProgress(TRUE)
    
    input <- read_csv(paste0(resultFolderPath, "/input.csv"))
    
    setwd(paste0(codesPath,"/apsimx_output"))
    
    file.create("progress.log")
    
    crop <- cropType()
    writeLines(crop, paste0(codesPath, "/selected_crop.txt"))
    
    source(paste0(codesPath,"/apsimx.R"))
# 
#     # Start, set up trials_df 
#     assign("trials_df", trials_df, envir = .GlobalEnv)
#     assign("locs_df", locs_df, envir = .GlobalEnv)
#     assign("soil_profile_list", soil_profile_list, envir = .GlobalEnv)
#     assign("daily_output", daily_output, envir = .GlobalEnv)
#     assign("yields", yields, envir = .GlobalEnv)
#     assign("res", res, envir = .GlobalEnv)
#     assign("trials_x", trials_x, envir = .GlobalEnv)
#     assign("charact_x", charact_x, envir = .GlobalEnv)
#     assign("daily_charact_x", daily_charact_x, envir = .GlobalEnv)
#     assign("bigmet", bigmet, envir = .GlobalEnv)
#     
    analysisDone(TRUE)
    analysisInProgress(FALSE)
    
    updateSiteSelectionUI()
    updateSiteSelectionFacetdUI()
    updateSiteSelectionBetweenUI()
  })

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

  #updateSiteSelectionFacetdUI ----
  updateSiteSelectionFacetdUI <- function() {
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
    updateSiteSelectionUI()
    updateSiteSelectionBetweenUI()
    updateSiteSelectionFacetdUI()
    
    file_to_view <- input$fileToView
    file_path <- paste0(resultFolderPath, "/", file_to_view)
    
    if (file.exists(file_path)) {
      data <- read.csv(file_path)
      # Round all numeric columns to 2 decimal places
      data <- data %>% mutate(across(where(is.numeric), round, 2))
      # Debugging print statements
      #print(dim(data))  # Print the dimensions of the data
      datatable(data, extensions = 'Buttons', options = list(
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
        scrollX = TRUE
      ), escape = FALSE)
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
    selected_file <- input$fileSelectPlot  # Use the selected file
    file_path <- paste0(resultFolderPath, "/", selected_file)
    
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
    selected_file <- input$fileSelectPlot  # Use the selected file
    file_path <- paste0(resultFolderPath, "/", selected_file)
    
    if (file.exists(file_path)) {
      data <- read.csv(file_path)
      
      if (!selected_file %in% c("trials_x.csv","final_x.csv")) {
        trials_x <- read.csv(paste0(resultFolderPath, "/trials_x.csv"))
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
  })
  
  ## download results ----
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0("results_", Sys.Date(), ".zip")  # Name the zip file
    },
    content = function(file) {
      # Create a temporary directory to store the files
      temp_dir <- tempdir()
      files <- list.files(resultFolderPath, full.names = TRUE)
      
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
    files <- list.files(resultFolderPath, full.names = FALSE)
    selectInput("fileSelect", "Select File to Download", choices = files)
  })
  
# Heatmaps ----  
  ## select maturity for heatmap -----
  output$matSelectUI <- renderUI({
    req(analysisDone())
    gen_choices <- unique(trials_x$Mat)
    selectInput(inputId = "matSelect", label = "Select Maturity for Heatmap", choices = gen_choices, selected = gen_choices[1])
  })
  
  ## store heatmap for download ----
  output$varHeatmapUI <- renderUI({
    req(analysisDone())
    charact_x_path <- paste0(resultFolderPath, "/charact_x.csv")
    if(file.exists(charact_x_path)) {
      charact_x <- read.csv(charact_x_path)
      varchoice <- charact_x %>% ungroup() %>% select(where(is.numeric) & !c(ID, Period)) %>% names()
      #print(varchoice)
      selectInput("heatmapSelect", "Select Variable for Heatmap", choices = varchoice)
      
    }
  })
  
  ## more heatmap rendering ----
  observe({
    output$heatmapPlotUI <- renderUI({
      updateSiteSelectionFacetdUI()
      graphics.off()
      req(input$heatmapSelect)  # Ensure there's a selected value
      plotOutput("heatmapPlot", height = "600px", width = "90%")
      
    })
  })

  ## render heatmap ----
  output$heatmapPlot <- renderPlot(
    {
      req(input$heatmapSelect)  # Ensure a variable is selected
      req(analysisDone())
      matsel <- input$matSelect 
      var <- input$heatmapSelect
      
      # Logic to prepare the heatmap matrix
      final_x_path <- paste0(resultFolderPath, "/final_x.csv")
      final_x <- read_csv(final_x_path)
      
      #set palette
      pal_f <- colorRampPalette(brewer.pal(9,"RdYlBu")) #creates a continuous palette
      palette <- rev(pal_f(50)[2:50])
      
      if (file.exists(final_x_path)) {
        var <- input$heatmapSelect
        var_mat <- final_x %>% filter(Mat == matsel) %>% select(ID, Site, starts_with(var)) %>% select(-ID) %>%
          group_by(Site) %>% summarize(across(where(is.numeric), function(x){mean(x,na.rm=T)})) %>%
          column_to_rownames("Site") %>%
          remove_empty(which = "rows") %>%
          as.matrix()
        number_names <- sub(".*_(\\d+)$", "\\1", colnames(var_mat))
        colnames(var_mat) <- number_names
        sorted_colnames <- as.character(sort(as.numeric(colnames(var_mat))))
        var_mat <- var_mat[, sorted_colnames]
        
        var_mat[is.nan(var_mat)] <- NA
        var_vals <- c(var_mat)[!is.na(c(var_mat))]
        
        #print(head(var_mat))
        #print(dim(var_mat))
        
        if (all(var_vals == var_vals[1])){  #check if matrix is constant
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
                   main = paste0("Means of ",var," by Site (Maturity: ",matsel,")"))
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
                            main = paste0("Means of ",var," by Site (Maturity: ",matsel,")"))
        }
        
        heatmap_plot(heatmap)
        
        
      } else {
        plot(NULL, main = "Data not available")
      }
      list(var_mat = var_mat, var = var)
      
    })

  ## heatmap download handler ----
  output$downloadHeatmap <- downloadHandler(
    filename = function() {
      paste0("heatmap-", input$heatmapSelect, "-", Sys.Date(), ".png")
    },
    content = function(file) {
      # Use the stored heatmap for the download
      png(file, width = 1400, height = 1000)
      grid::grid.draw(heatmap_plot()$gtable)  # Draw the stored heatmap
      dev.off()
    }
  )
  
# Reactive to generate TT/Precip data ----
  accumulatedData <- reactive({
    req(input$selectedSites)
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
      png(file, width = 800, height = 600)
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
      png(file, width = 800, height = 600)
      print(between_sites_plot_data())  # Print the stored plot
      dev.off()
    }
  )

}

# Run the app ----
shinyApp(ui = ui, server = server)