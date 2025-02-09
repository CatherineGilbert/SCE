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
library(ggplot2)

# NOTE: General style guideline recommends using |> pipe instead of %>%
# NOTE: This project uses the file system a LOT. Can I reduce the reliance?
# NOTE: Can I make it so more of these variables are reused instead of load file
# TODO: try to come up with shorter variable names

# Hardcoded Page Elements ----
header_height <- "40px" # Adjust the height as needed
title_width <- 300 # Adjust this width based on your needs
sidebar_width <- 300 # NOTE: Is this the same as title_width for a reason?

# TODO: consider "cogs" icon for "analysis"
# TODO: consider "chart-pie" for "results"
# TODO: consider "cloud-showers-heavy" for "daily"
# TODO: Consider making the last four sub menu items from View Results.
sidebar_content <- list(
  menuItem("Description", tabName = "description", icon = icon("info")),
  menuItem("Upload & Analyze", tabName = "analysis", icon = icon("upload")),
  menuItem("View Results", tabName = "results", icon = icon("image")),
  menuItem("Seasonal Heatmap", tabName = "heatmap", icon = icon("fire")),
  menuItem("Daily Accumulation", tabName = "daily", icon = icon("chart-line")),
  menuItem(
    "Annual Totals",
    tabName = "faceted_comparison",
    icon = icon("chart-area")
  ),
  menuItem(
    "Ten Year Site Means",
    tabName = "between_sites",
    icon = icon("chart-bar")
  )
)

tab_content <- list(
  description = tabItem(
    tabName = "description",
    fluidPage(
      h2("Seasonal Characterization Engine"),
      p("Built in R and Shiny using the apsimr package."),
      p("Purpose: Characterize the growing season at one or more sites 
        according to the crop's response to environemntal conditions."),
      h3("Seasonal Characterization Engine can be used to:"),
      tags$ul(
        tags$li("Understand the environment in terms of the conditions and 
                stressors present at each stage of the crop's development."),
        tags$li("Compare seasonal conditions between sites and how those
                conditions have changed over time."),
        tags$li("Predict crop phenology and performance from a cultivar's 
                maturity, planting date, and location.")
      ),
      h4("Tool Walkthrough"),
      p("Use the \"Upload & Analyze\" page to upload and analyze your input data
        and download the results."),
      p("See the \"View Results\" page for a boxplot of
        each column of your results"),
      p("See the additional visualization pages for aggregated plots."),
      h5("Contact"),
      p("Please reach out with any questions, suggestions, or contributions!"),
      p("Sam Shi, mshi17@illinois.edu"),
      p("Catherine Gilbert, cmg3@illinois.edu"),
      p("github page:
        https://github.com/CatherineGilbert/APSIMX_SeasonalCharacterization")
    )
  ),

  analysis = tabItem(
    tabName = "analysis",
    fluidPage(
      selectInput(
        "cropType",
        "Select Crop",
        choices = c("Maize" = "Maize", "Soy" = "Soy")
      ),
      fileInput("fileUpload", "Upload Input File", accept = c(".csv")),
      actionButton("runAnalysis", "Run Analysis", icon = icon("play")),
      downloadButton("downloadData", "Download Results"),
      br(),
      h3("Dataset Descriptions"),
      p(
        strong("trials_x:"),
        " aligns with the input file and contains sim parameters, identifying 
        information, and values that should not be summarized by period."
      ),
      p(
        strong("daily_charact_x:"),
        " is the combined total output of the APSIM simulations and contains
        the recorded values of the reporting variables for each day of each 
        simulation. This data is available to the user if they wish to work 
        with the raw outputs of the tool."
      ),
      p(
        strong("charact_x:"),
        " contains parameters specific to each developmental period. These are 
        the environmental and biological parameters summarized by period, and 
        the parameters which describe the periods themselves, such as starting 
        and end date. This data is in long format by ID and period."
      ),
      p(
        strong("final_x:"),
        " joins the contents of trials_x and charact_x, and contains the full 
        outputs of the seasonal characterization engine in wide format. 
        The naming convention of period-specific parameters is 
        “Variable_Period”, e.g., “Rain_5” is the mean rainfall within the fifth 
        period of development."
      ),
      fluidRow(column(
        12,
        progressBar(id = "progressBar", value = 0, display_pct = TRUE)
      ))
    )
  ),

  results = tabItem(
    tabName = "results",
    fluidPage(
      div(class = "large-text-label control-label", tags$label("Boxplot")),
      uiOutput("fileSelectPlotUI"),
      uiOutput("varSelectUI"),
      plotOutput("boxplot"),
      downloadButton("downloadBoxplot", "Download Boxplot"),
      div(
        class = "large-text-label",
        selectInput(
          "fileToView",
          "View Result Files",
          choices = c(
            "trials_x.csv",
            "daily_charact_x.csv",
            "charact_x.csv",
            "final_x.csv"
          )
        )
      ),
      DTOutput("viewData")
    )
  ),

  heatmap = tabItem(
    tabName = "heatmap",
    fluidPage(
      p("This heatmap visualizes means of the selected variable by site and 
      genetic group. Use the dropdown menus to choose a variable and 
      genetic group for display."),
      uiOutput("varHeatmapUI"),
      uiOutput("genSelectUI"),
      uiOutput("heatmapPlotUI"),  # Use uiOutput to render the heatmap plot
      downloadButton("downloadHeatmap", "Download Heatmap")
    )
  ),

  daily = tabItem(
    tabName = "daily",
    fluidPage(
      p("This section allows you to compare daily accumulated precipitation and 
        thermal time between different sites. Select the comparison type and 
        sites for analysis."),
      fluidRow(
        column(
          width = 10,  # Adjust the width as needed
          selectInput("comparisonType", "Select Comparison Type", choices = c(
            "Acc. Precip. (Day of Year)" = "precip_doy",
            "Acc. Precip. (Days after Sowing)" = "precip_das",
            "Acc. Thermal Time (Day of Year)" = "tt_doy",
            "Acc. Thermal Time (Days after Sowing)" = "tt_das"
          )),
          plotOutput("comparisonPlot"),
          downloadButton("downloadComparisonPlot", "Download Plot")
        ),
        column(width = 3, uiOutput("siteSelectionUI"))
      )
    )
  ),

  faceted = tabItem(
    tabName = "faceted_comparison",
    fluidPage(
      p("This section provides a faceted comparison of accumulated 
        precipitation and thermal time for different sites. Select the sites to 
        visualize the comparison."),
      fluidRow(
        column(width = 3, uiOutput("siteSelectionUI_faceted")),
        column(
          width = 9,
          plotOutput("facetedComparisonPlot"),
          downloadButton("downloadFacetedComparisonPlot", "Download Plot")
        )
      )
    )
  ),

  between_sites = tabItem(
    tabName = "between_sites",
    fluidPage(
      p("This section visualizes the 10-year site averages for a typical 
        growing season, comparing accumulated precipitation and thermal time 
        between selected sites."),
      fluidRow(
        column(width = 3, uiOutput("siteSelectionUI_between")),
        column(
          width = 9,
          plotOutput("plotBetweenSites"),
          downloadButton("downloadBetweenSitesPlot", "Download Plot")
        )
      )
    )
  )
)

# Define UI ----
ui <- dashboardPage(
  dashboardHeader(
    title = tags$div(
      style = "display: flex; align-items: center;",
      tags$img(
        src = "aces.png",
        height = header_height,
        style = "margin-right: 10px"
      ),
      tags$span("SCE", style = "font-size: 30px; font-weight: bold;")
    ),
    titleWidth = title_width
  ),
  dashboardSidebar(sidebarMenu(sidebar_content), width = sidebar_width),
  dashboardBody(
    shinyjs::useShinyjs(),
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
      tags$script(src = "custom.js")
    ),
    tabItems(
      tab_content$description,
      tab_content$analysis,
      tab_content$results,
      tab_content$heatmap,
      tab_content$daily,
      tab_content$faceted,
      tab_content$between_sites
    )
  )
)

# Define server logic ----

# TODO: First of all, consider making this it's own R script.
# TODO: Stop using all of the setwd, they are really sketchy
server <- function(input, output, session) {
  # TODO: Seriously fix how paths are handled
  # Path to the scripts and results
  # Should this be: codes_path <- here() # FIXME
  codes_path <- "C:/Users/cmg3/Documents/GitHub/SCT"
  setwd(paste0(codes_path, "/apsimx_output"))
  result_folder_path <- paste0(codes_path, "/apsimx_output/output")

  # Reactive values for storing the analysis state and the selected variable
  analysis_finshed <- reactiveVal(TRUE)
  analysis_in_progress <- reactiveVal(FALSE)
  heatmap_plot <- reactiveVal(NULL)

  # things to do immediately after analysis finishes ----
  observe({
    req(analysis_finshed())
    tryCatch({
      source(paste0(codes_path, "/visualization.R"))
    }, error = function(e) {
      cat("An error occurred while sourcing the file:", e$message, "\n")
    })
    trials_x <- read_csv(paste0(result_folder_path, "/trials_x.csv"))
  })

  # disable run analysis if analysis is currently in progress ----
  observe({
    if (analysis_in_progress()) {
      shinyjs::disable("runAnalysis")
    } else {
      shinyjs::enable("runAnalysis")
    }
  })

  # Update progress bar based on log file ----
  # TODO: figure out what the updateProgressBar and progressBar functions are
  progress <- reactiveVal(0)

  observe({
    invalidateLater(5000, session)
    setwd(paste0(codes_path, "/apsimx_output"))
    if (file.exists("progress.log")) { # Update progress based on log contents
      log_contents <- readLines("progress.log")
      total_steps <- 16  # Define the total number of steps in the log
      current_step <- length(log_contents)
      progress_value <- round(current_step / total_steps * 100)
      progress(progress_value)
      updateProgressBar(session, id = "progressBar", value = progress_value)
    }
  })

  output$progressBar <- renderUI({
    progressBar(id = "progressBar", value = progress(), display_pct = TRUE)
  })

  # TODO: Why are these variables defined here? ----
  selected_variable <- reactiveVal()
  trials_df <- reactiveVal()

  # When a file is uploaded, copy it to `result_folder_path/input.csv` ----
  observeEvent(input$fileUpload, {
    if (!dir.exists(result_folder_path)) {
      dir.create(result_folder_path, recursive = TRUE)
    }
    tryCatch({
      file.copy(
        input$fileUpload$datapath,
        paste0(result_folder_path, "/input.csv"),
        overwrite = TRUE
      )
      if (file.exists(paste0(result_folder_path, "/input.csv"))) {
        cat("File copy successful\n")
      } else {
        cat("File copy failed\n")
      }
    }, error = function(e) {
      cat("An error occurred during file copy: ", e$message, "\n")
    })
    update_site_selection_ui()
  })

  # When runAnalysis button clicked ----
  observeEvent(input$runAnalysis, {
    req(input$fileUpload)
    analysis_finshed(FALSE)
    analysis_in_progress(TRUE)

    input <- read_csv(paste0(result_folder_path, "/input.csv"))
    setwd(paste0(codes_path, "/apsimx_output"))

    file.create("progress.log")

    crop <- input$cropType
    # NOTE: This appears to be in SCT/ rather than SCT/apsimx_output,
    # even though setwd was supposed to move us there
    writeLines(crop, paste0(codes_path, "/selected_crop.txt"))

    # TODO: Is this an appropriate place to call source()?
    source(paste0(codes_path, "/apsimx.R"))

    # Start, set up trials_df
    # NOTE: These are setting .GlobalEnv. Why, and can I use for path stuff?
    # NOTE: pretty  sure these are in apsimx.R; how can they be recognized?
    assign("trials_df", trials_df, envir = .GlobalEnv)
    assign("locs_df", locs_df, envir = .GlobalEnv)
    assign("soil_profile_list", soil_profile_list, envir = .GlobalEnv)
    assign("daily_output", daily_output, envir = .GlobalEnv)
    assign("yields", yields, envir = .GlobalEnv)
    assign("res", res, envir = .GlobalEnv)
    assign("trials_x", trials_x, envir = .GlobalEnv)
    assign("charact_x", charact_x, envir = .GlobalEnv)
    assign("daily_charact_x", daily_charact_x, envir = .GlobalEnv)
    assign("bigmet", bigmet, envir = .GlobalEnv)

    analysis_finshed(TRUE)
    analysis_in_progress(FALSE)

    # TODO: These all seem like helpers that could be moved into R/
    update_site_selection_ui()
    update_site_selection_faceted_ui()
    update_site_selection_between_ui()
  })

  # Site Selection Functionality ----

  update_site_selection_ui <- function() {
    # NOTE: can this be put in a helper if it relies on analysis_finished()?
    req(analysis_finshed())
    trials_df <- read_csv(paste0(result_folder_path, "/trials_x.csv"))
    print("337") # NOTE: Why are we printing these numbers?
    sites <- sort(unique(trials_df$Site))
    output$siteSelectionUI <- renderUI({
      fluidRow(
        column(
          width = 12,
          actionButton("selectAllSites", "Select All"),
          actionButton("unselectAllSites", "Unselect All")
        ),
        column(
          width = 12,
          checkboxGroupInput(
            "selectedSites",
            "Select Sites",
            choices = sites,
            selected = sites[1:2]
          )
        )
      )
    })
  }

  update_site_selection_faceted_ui <- function() {
    req(analysis_finshed())
    # TODO: Decide on keeping read_csv or assigning trials_x for both update fns
    # Should trials_df <- read_csv(paste0(resultFolderPath, "/trials_x.csv"))
    # NOTE: WHY print("357")
    # FIXME: why this load trials_x, but update_site_selection_ui loads file?
    trials_df <- trials_x
    sites <- sort(unique(trials_df$Site))
    output$siteSelectionUI_faceted <- renderUI({
      fluidRow(
        column(
          width = 12,
          actionButton("selectAllSites_faceted", "Select All"),
          actionButton("unselectAllSites_faceted", "Unselect All")
        ),
        column(
          width = 12,
          checkboxGroupInput(
            "selectedSites_faceted",
            "Select Sites",
            choices = sites,
            selected = sites[1:2]
          )
        )
      )
    })
  }

  observeEvent(input$selectAllSites, {
    # TODO: do we have to read_csv or could I just ignore trials_df?
    trials_df <- read_csv(paste0(result_folder_path, "/trials_x.csv"))
    print("374")
    sites <- sort(unique(trials_df$Site))
    updateCheckboxGroupInput(session, "selectedSites", selected = sites)
  })

  observeEvent(input$unselectAllSites, {
    updateCheckboxGroupInput(session, "selectedSites", selected = character(0))
  })

  observeEvent(input$selectAllSites_faceted, {
    trials_df <- read_csv(paste0(result_folder_path, "/trials_x.csv"))
    print("384")
    sites <- sort(unique(trials_df$Site))
    updateCheckboxGroupInput(session, "selectedSites_faceted", selected = sites)
  })

  observeEvent(input$unselectAllSites_faceted, {
    updateCheckboxGroupInput(
      session,
      "selectedSites_faceted",
      selected = character(0)
    )
  })

  # Reactive: Filter meteorological data based on selected sites (bigmet)
  filtered_met_data <- reactive({
    req(input$selectedSites)
    current_year <- as.numeric(substr(Sys.time(), 1, 4)) - 1
    bigmet <- data.frame()
    # For each trial loc, get the last 9 years of met data and put in bigmet
    for (s in 1:max(trials_x$ID_Loc)) {
      lil_met <- read_apsim_met(paste0("met/loc_", s, ".met"), verbose = F) %>%
        as_tibble() %>%
        filter(year >= current_year - 9, year <= current_year) %>%
        mutate(ID_Loc = s)
      bigmet <- bind_rows(bigmet, lil_met)
    }
    # Select distinct (site, loc id), then self join.
    # Group by site, loc, year, day, and make tt column:
    # which is a modified avg. temp, then ungroup
    # TODO: shorten explanation, and maybe simplify below expression.
    bigmet <- trials_x %>%
      select(Site, ID_Loc) %>%
      distinct() %>%
      left_join(bigmet) %>%
      group_by(Site, ID_Loc, year, day) %>%
      # NOTE: Why ` - 0`?
      # NOTE: max(..., 0) would only do anything if both maxt and mint are < 0C
      mutate(tt = max((min(maxt, 34) + max(mint, 0)) / 2 - 0, 0)) %>%
      ungroup()

    # NOTE: this could save time in above pipe, but may be bad since self join
    bigmet <- filter(bigmet, Site %in% input$selectedSites)
    bigmet
    # NOTE: remember this is recalculated every time the site selection changes
    #   and therefore should be as efficient as possible.
  })

  # Reactive: Generate filtered and accumulated data (dbtw_sites)
  acc_data <- reactive({
    req(filtered_met_data())
    # NOTE: WHY print("448")
    startend <- select(
      trials_x, Site, Year, ID, Mat, PlantingDate_Sim, HarvestDate_Sim
    ) %>% mutate(
      first_doy = yday(PlantingDate_Sim),
      until_final = as.numeric(HarvestDate_Sim - PlantingDate_Sim),
      final_doy = first_doy + until_final
    ) # done this way because final_doy can go over 365

    mean_startend <- group_by(startend, Site) %>%
      summarize(
        first_doy = mean(first_doy, na.rm = TRUE),
        final_doy = mean(final_doy, na.rm = TRUE)
      )

    filtmet <- filtered_met_data() %>%
      left_join(mean_startend) %>%
      filter(day >= first_doy & day <= final_doy)

    dbtw_sites <- filtmet %>%
      group_by(Site, year) %>%
      mutate(acc_precip = cumsum(rain), acc_tt = cumsum(tt)) %>%
      ungroup() %>%
      group_by(Site, day) %>%
      summarize(
        acc_precip = mean(acc_precip, na.rm = TRUE),
        acc_tt = mean(acc_tt, na.rm = TRUE)
      )

    dbtw_sites
  })

  # Store the generated daily TT/Precip plot for download ----
  comparison_plot_data <- reactiveVal()

  output$comparisonPlot <- renderPlot({
    req(acc_data(), input$comparisonType)
    data <- acc_data()

    plot <- ggplot(data) # TODO: library(ggplot)?

    if (input$comparisonType == "precip_doy") {
      plot <- plot +
        aes(x = day, y = acc_precip, colour = Site) +
        geom_line() +
        scale_color_hue(direction = 1) +
        labs(x = "Day of Year", y = "Accumulated Precipitation (mm)") +
        theme_minimal()
    } else if (input$comparisonType == "tt_doy") {
      plot <- plot +
        aes(x = day, y = acc_tt, colour = Site) +
        geom_line() +
        scale_color_hue(direction = 1) +
        labs(x = "Day of Year", y = "Accumulated Thermal Time") +
        theme_minimal()
    } else if (input$comparisonType == "precip_das") {
      sdbtw_sites <- data %>% mutate(day = day - min(day) + 1)
      plot <- ggplot(sdbtw_sites) +
        aes(x = day, y = acc_precip, colour = Site) +
        geom_line() +
        scale_color_hue(direction = 1) +
        labs(x = "Days after Sowing", y = "Accumulated Precipitation (mm)") +
        theme_minimal()
    } else if (input$comparisonType == "tt_das") {
      sdbtw_sites <- data %>% mutate(day = day - min(day) + 1)
      plot <- ggplot(sdbtw_sites) +
        aes(x = day, y = acc_tt, colour = Site) +
        geom_line() +
        scale_color_hue(direction = 1) +
        labs(x = "Days after Sowing", y = "Accumulated Thermal Time") +
        theme_minimal()
    }

    comparison_plot_data(plot)  # Store the plot in a reactive value
    print(plot)  # Render the plot
  })

  # Download handler for the daily TT/Precip plot ----
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

  # Store the generated TT/Precip 2 plot for download ----
  faceted_comparison_plot_data <- reactiveVal()

  output$facetedComparisonPlot <- renderPlot({
    req(input$selectedSites_faceted)
    selected_sites <- input$selectedSites_faceted

    plot_dt <- wthn_sites %>% filter(Site %in% selected_sites)
    means <- plot_dt %>% group_by(Site) %>%
      summarise(mean_acc_precip = mean(acc_precip, na.rm = TRUE),
                mean_acc_tt = mean(acc_tt, na.rm = TRUE))

    plot <- ggplot(plot_dt, aes(x = acc_precip, y = acc_tt)) +
      facet_wrap(vars(Site)) +
      geom_vline(
        data = means,
        aes(xintercept = mean_acc_precip),
        color = "black",
        linetype = "dashed"
      ) +
      geom_hline(
        data = means,
        aes(yintercept = mean_acc_tt),
        color = "black", linetype = "dashed"
      ) +
      geom_label(aes(label = plot_dt$year), size = 3) +
      labs(x = "Acc. Precipitation (mm)", y = "Acc. Thermal Time") +
      theme_minimal() +
      theme(legend.position = "none")

    faceted_comparison_plot_data(plot)  # Store the plot in a reactive value
    print(plot)  # Render the plot
  })

  # Download handler for TT/Precip 2 plot ----
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

  # Selecting a file to view and displaying it ----
  # TODO: find renderDT and datatable functions (and potentially add library())
  output$viewData <- renderDT({
    req(analysis_finshed()) # TODO: Should input$fileToView be added?
    update_site_selection_ui()
    update_site_selection_between_ui()

    file_to_view <- input$fileToView
    file_path <- paste0(result_folder_path, "/", file_to_view)

    if (file.exists(file_path)) {
      data <- read.csv(file_path)
      # Round all numeric columns to 2 decimal places
      data <- data %>% mutate(across(where(is.numeric), round, 2))
      datatable(data, extensions = "Buttons", options = list(
        dom = "Bfrtip",
        buttons = c("copy", "csv", "excel", "pdf", "print"),
        scrollX = TRUE
      ), escape = FALSE)
    } else {
      print("File not found: ", file_to_view)
      return(NULL)
    }
  })

  # TODO: Add a list of these files (as well as descriptions) to reuse
  # These are  used in the tab pages, here, and few other places.
  #first select file for boxplot
  output$fileSelectPlotUI <- renderUI({
    req(analysis_finshed())
    files <- c(
      "trials_x.csv",
      "daily_charact_x.csv",
      "charact_x.csv",
      "final_x.csv"
    )
    selectInput(
      "fileSelectPlot",
      "Select File to Plot",
      choices = files,
      selected = "charact_x.csv"
    )
  })

  #second select var for UI
  output$varSelectUI <- renderUI({
    req(analysis_finshed()) # TODO: should input$fileSelectPlot be included?
    selected_file <- input$fileSelectPlot  # Use the selected file
    file_path <- paste0(result_folder_path, "/", selected_file)

    if (file.exists(file_path)) {
      data <- read.csv(file_path)
      selectInput(
        "varSelect_boxplot",
        "Select Variable",
        choices = names(data)[-1],
        selected = "Rain"
      )
    }
  })

  # Selecting and rendering a boxplot for a given variable ----
  observeEvent(input$varSelect_boxplot, {
    selected_variable(input$varSelect_boxplot)
  }, ignoreInit = TRUE)

  # Store the generated boxplot for download
  boxplot_data <- reactiveVal()

  output$boxplot <- renderPlot({
    req(analysis_finshed(), selected_variable())
    selected_file <- input$fileSelectPlot  # Use the selected file
    file_path <- paste0(result_folder_path, "/", selected_file)

    if (file.exists(file_path)) {
      data <- read.csv(file_path)

      # If another file is selected, join data with ID and Site from trials_x
      if (!selected_file %in% c("trials_x.csv", "final_x.csv")) {
        trials_x <- read.csv(paste0(result_folder_path, "/trials_x.csv"))
        data <- left_join(data, trials_x[, c("ID", "Site")], by = "ID")
      }
      data$Site <- as.factor(data$Site)  # Ensure Site is treated as a factor

      # TODO: it it necessary to define a variable? Just use selected_variable()
      selected_var <- selected_variable()

      # Check if selected_var is in the column names of data
      if (selected_var %in% names(data)) {
        # Create the box plot
        # TODO: is .data[[selected_var]] still used for this? better way?
        p <- ggplot(data, aes(
          x = Site,
          y = .data[[selected_var]],
          fill = Site
        )) +
          geom_boxplot() +
          labs(x = "Site", y = selected_var) +
          theme(
            axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "none"
          )

        boxplot_data(p)  # Store the plot in a reactive value
        print(p)  # Render the plot
      } else {
        # TODO: try: "Error:", selected_var, "is not a column of", selected_file
        print(paste("Error: Variable", selected_var, "not found in data frame"))
      }
    } else {
      print(paste("Error: File", selected_file, "does not exist"))
    }
  })

  # Download handler for the boxplot
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

  # TODO: It feels like there is a better place for this.
  # Disable download button if analysis is not finished.
  observe({
    if (analysis_finshed()) {
      shinyjs::enable("downloadData")
    } else {
      shinyjs::disable("downloadData")
    }
  })

  output$downloadData <- downloadHandler(
    filename = function() {
      paste0("results_", Sys.Date(), ".zip")  # Name the zip file
    },
    content = function(file) {
      # Create a temporary directory to store the files
      temp_dir <- tempdir()
      files <- list.files(result_folder_path, full.names = TRUE)

      # Copy the selected files to the temporary directory
      file_paths <- file.path(temp_dir, basename(files))
      file.copy(files, file_paths)

      # Create a zip file from the files in the temporary directory
      zip::zipr(file, files = file_paths)
    }
  )

  output$fileSelectUI <- renderUI({
    req(analysis_finshed())
    files <- list.files(result_folder_path, full.names = FALSE)
    selectInput("fileSelect", "Select File to Download", choices = files)
  })

  output$genSelectUI <- renderUI({
    req(analysis_finshed())
    gen_choices <- unique(trials_x$Mat)
    selectInput(
      inputId = "genSelect",
      label = "Select Maturity for Heatmap",
      choices = gen_choices,
      selected = gen_choices[1]
    )
  })

  output$varHeatmapUI <- renderUI({
    req(analysis_finshed())
    charact_x_path <- paste0(result_folder_path, "/charact_x.csv")
    if (file.exists(charact_x_path)) {
      charact_x <- read.csv(charact_x_path)
      varchoice <- charact_x %>%
        ungroup() %>%
        select(where(is.numeric) & !c(ID, Period)) %>%
        names()

      selectInput(
        "heatmapSelect",
        "Select Variable for Heatmap",
        choices = varchoice
      )
    }
  })

  observe({
    output$heatmapPlotUI <- renderUI({
      update_site_selection_faceted_ui()
      graphics.off()
      req(input$heatmapSelect)  # Ensure there's a selected value
      plotOutput("heatmapPlot", height = "600px", width = "90%")
    })
  })

  output$heatmapPlot <- renderPlot({
    req(input$heatmapSelect, analysis_finshed()) # Ensure a variable is selected
    matsel <- input$genSelect # NOTE: matsel bad name (matrix vs maturity)
    var <- input$heatmapSelect

    # Logic to prepare the heatmap matrix
    final_x_path <- paste0(result_folder_path, "/final_x.csv")
    final_x <- read_csv(final_x_path)

    #set palette
    pal_f <- colorRampPalette(brewer.pal(9, "RdYlBu")) # Continuous palette
    palette <- rev(pal_f(50)[2:50])

    if (file.exists(final_x_path)) {
      var <- input$heatmapSelect
      var_mat <- final_x %>%
        filter(Mat == matsel) %>%
        select(ID, Site, starts_with(var)) %>%
        select(-ID) %>%
        group_by(Site) %>%
        summarize(across(where(is.numeric), function(x) {
          mean(x, na.rm = TRUE)
        })) %>%
        column_to_rownames("Site") %>%
        remove_empty(which = "rows") %>%
        as.matrix()

      number_names <- sub(".*_(\\d+)$", "\\1", colnames(var_mat))
      colnames(var_mat) <- number_names
      # TODO: Could skip creating sorted_colnames variable
      sorted_colnames <- as.character(sort(as.numeric(colnames(var_mat))))
      var_mat <- var_mat[, sorted_colnames]

      var_mat[is.nan(var_mat)] <- NA # NOTE: What does this do?
      var_vals <- c(var_mat)[!is.na(c(var_mat))]

      # TODO: The only difference: breaks (top) and scale (bottom)
      # TODO: Find the pheatmap function
      if (all(var_vals == var_vals[1])) { # Check if matrix is constant
        heatmap <- pheatmap(
          var_mat,
          angle_col = 0,
          color = palette,
          breaks = c( # ONLY TOP
            var_mat[1, 1] - 2,
            var_mat[1, 1] - 1,
            var_mat[1, 1] + 1,
            var_mat[1, 1] + 2
          ),
          fontsize = 16,
          display_numbers = round(var_mat, 2),
          number_color = "grey10",
          number_format = "%.2f",
          legend = FALSE,
          cluster_cols = FALSE,
          cluster_rows = TRUE,
          main = paste0("Means of ", var, " by Site (Maturity: ", matsel, ")")
        )
      } else {
        heatmap <- pheatmap(
          var_mat,
          angle_col = 0,
          fontsize = 16,
          color = palette,
          display_numbers = round(var_mat, 2),
          number_color = "grey10",
          scale = "column", # ONLY BOTTOM
          number_format = "%.2f",
          legend = FALSE,
          cluster_cols = FALSE,
          cluster_rows = TRUE,
          main = paste0("Means of ", var, " by Site (Maturity: ", matsel, ")")
        )
      }
      heatmap_plot(heatmap)

    } else {
      plot(NULL, main = "Data not available")
    }
    list(var_mat = var_mat, var = var)
  })

  # Heatmap download handler
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

  # Update TT/Precip 3 UI : SiteSelectionBetweenUI -----
  # TODO: this probably belongs with the other update functions (in a helper)
  update_site_selection_between_ui <- function() {
    req(analysis_finshed())
    trials_df <- trials_x
    sites <- sort(unique(trials_df$Site))
    output$siteSelectionUI_between <- renderUI({
      fluidRow(
        column(
          width = 12,
          actionButton("selectAllSites_between", "Select All"),
          actionButton("unselectAllSites_between", "Unselect All")
        ),
        column(
          width = 12,
          checkboxGroupInput(
            "selectedSites_between",
            "Select Sites",
            choices = sites,
            selected = sites[1:2]
          )
        )
      )
    })
  }

  # Update TT/Precip 3 UI: ----
  # TODO: This could prob go with similar ones above
  observeEvent(input$selectAllSites_between, {
    trials_df <- read_csv(paste0(result_folder_path, "/trials_x.csv"))
    print("806") # NOTE: WHY??
    sites <- sort(unique(trials_df$Site))
    updateCheckboxGroupInput(session, "selectedSites_between", selected = sites)
  })

  observeEvent(input$unselectAllSites_between, {
    updateCheckboxGroupInput(
      session,
      "selectedSites_between",
      selected = character(0)
    )
  })

  # Store the generated TT/Precip 3 plot for download ----
  between_sites_plot_data <- reactiveVal()

  output$plotBetweenSites <- renderPlot({
    req(analysis_finshed())
    selected_sites <- input$selectedSites_between

    plot_dtt <- wthn_sites %>%
      filter(Site %in% selected_sites) %>%
      group_by(Site) %>%
      summarize(acc_precip = mean(acc_precip, na.rm = TRUE),
                acc_tt = mean(acc_tt, na.rm = TRUE))

    plot <- ggplot(plot_dtt) +
      aes(x = acc_precip, y = acc_tt) +
      geom_vline(
        aes(xintercept = mean(acc_precip)),
        color = "black",
        linetype = "dashed"
      ) +
      geom_hline(
        aes(yintercept = mean(acc_tt)),
        color = "black",
        linetype = "dashed"
      ) +
      geom_label(aes(label = Site), size = 3) +
      theme_minimal() +
      labs(
        x = "Acc. Precipitation (mm)",
        y = "Acc. Thermal Time",
        title = "10 Year Site Averages for a Typical Growing Season"
      ) +
      theme(legend.position = "none")

    between_sites_plot_data(plot)  # Store the plot in a reactive value
    print(plot)  # Render the plot
  })

  # Download handler for TT/Precip 3 plot -----
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

  output$selectSite <- renderUI({
    selectInput("selectSite", "Select Site", choices = unique(trials_x$Site))
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)
