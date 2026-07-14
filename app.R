if (!require("pacman")) install.packages("pacman")
pkg <- c("shiny", "shinydashboard", "shinycssloaders", "shinyWidgets", "shinyBS", 
"shinyjs", "DT", "geosphere", "pheatmap", "apsimx", "daymetr", "data.table", 
"RColorBrewer", "janitor", "zip", "here", "future", "promises", "viridisLite", 
"dendextend", "scales", "grid", "leaflet", "apsimx", "tidyverse", 
"daymetr", "chirps", "nasapower", "soilDB", "spData", "tools", "parallel",
"beepr", "plotly", "bigstatsr")
p_load(char = pkg)

plan(multisession, workers = 2)
options(dplyr.summarise.inform = FALSE)

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
      tags$span("SCE", style = "font-size: 20px; font-weight: bold;")
    ),
    titleWidth = 300
  ),
  
  ## dashboardSidebar ----
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      menuItem("About", tabName = "info", icon = icon("circle-info")),
      menuItem("Upload and Analyze", tabName = "analysis", icon = icon("upload")),
      menuSubItem("Build Gridded Input File", tabName = "build_input", icon = icon("globe"))
    ),
    hidden(
      div(id = "sidebar_menu",
          sidebarMenu(
            menuItem("View Results",            tabName = "results",          icon = icon("table-list")),
            menuItem("Configure Pheno Periods",       tabName = "config_periods",   icon = icon("sliders")),
            menuItem("View Map",                tabName = "view_map",         icon = icon("map")),
            menuItem("View Seasonal Heatmaps",  tabName = "heatmap",          icon = icon("fire")),
            menuItem("View Trial Similarities", tabName = "trial_comp",       icon = icon("seedling")),
            menuItem("View Timelines",   tabName = "timelines",            icon = icon("chart-gantt"))
          )
      )
    ),
    hidden(
      div(id = "ttpp_sidebar_menu",
          sidebarMenu(
            menuItem("Thermal Time / Precipitation", icon = icon("cloud-sun-rain"),
               menuSubItem("Modify GDD Equation", tabName = "gdd_equation", icon = icon("calculator")),
               menuSubItem("Typical Site TT/Precip Accumulation", tabName = "daily_between_sites", icon = icon("chart-line")),
               menuSubItem("Site Yearly TT/Precip Totals", tabName = "faceted_comparison", icon = icon("chart-area")),
               menuSubItem("Ten Year Site TT/Precip Means", tabName = "between_sites", icon = icon("chart-bar"))
            )
          )
      )
    ),
    hidden(
      div(id = "sidebar_spinner",
          tags$div(style = "padding: 20px; text-align: left;",
                   icon("spinner", class = "fa-spin"),
                   "Loading ..."
          )
      )
    )
  ),
  
  
  ## dashbordBody CSS----
  dashboardBody(
    useShinyjs(),
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
    ### info page UI ----
    tabItems(
      tabItem(tabName = "info",
        fluidPage(
          h2("Seasonal Characterization Engine"),
          p(em("Created by Catherine Gilbert, German Mandrini, Sheila Scheffel Pereira, and Sam Shi.")),
          br(),
          p("The", tags$b("Seasonal Characterization Engine (SCE)"), " describes growing environment in terms of the crop's 
            development. Using APSIM, a procedural crop simulation program, the tool simulates the growth 
            of a crop according to specified 'trial' conditions and returns seasonal profiles consisting 
            of environmental variables aligned with crop phenology. This tool can be used to more accurately
            assess and compare the growing conditions experienced by crops, and to generate seasonal covariates 
            which can be used in later crop modeling."),
          fluidRow(
            column(
              width = 12, offset = 0,
            tags$a("Go to GitHub Repo", href = "https://github.com/CatherineGilbert/SCE", target = "_blank", class = "btn btn-primary",),
            tags$a("Open Documentation", href = "SCE_Documentation.html", target = "_blank", class = "btn btn-primary",),
            downloadButton("download_ex", "Download Example Files", class = "btn btn-primary", style = "color: white;")
            )
            ),
          p(""),
          br(),
          box(
            width = 12,
          p(tags$b("Citation:")),
          p("Gilbert, C., Mandrini, G., Ersoz, E., & Martin, N. (2026). The seasonal characterization engine, an application for describing environment from the perspective of crop development. SoftwareX, 33, 102477. https://doi.org/10.1016/j.softx.2025.102477

")
          )
        )      
      ),
    ### upload + analysis page UI ------
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

                  div(style = "margin-top: -20px; margin-left: 20px;",
                      checkboxInput("useExampleInput", "Use example file?", value = FALSE),
                      uiOutput("exampleOptions")
                  ),


                  fileInput(
                    "templateUpload",
                    label = tagList(
                      "Select Template Crop Model:",
                      shiny::span(icon("info-circle"), id = "tip_tempmodel")
                    ), 
                    accept = c(".apsimx")
                  ),
                  
                  div(style = "margin-top: -20px; margin-left: 20px;",
                      checkboxInput("useExampleTemplate", "Use example file?", value = FALSE),
                      uiOutput("exampleTemplate")
                  ),
                  
                  selectInput(
                    "matType",
                    label = tagList(
                      "Select Maturity Handling:",
                      shiny::span(icon("info-circle"), id = "tip_mat_hndl")
                    ), 
                    choices = c(
                      "Soybean RM" = "Soy",
                      "Maize RM" = "Maize",
                      "Use given APSIM cultivars" = "Direct"
                    )
                  ),
                  selectInput(
                    "weatherAquis",
                    "Select Weather Acquisition:",
                    choices = c(
                      "NASAPOWER" = "NASAPOWER",
                      "DAYMET (USA only)" = "DAYMET",
                      "CHIRPS" = "CHIRPS"
                    )
                  ),
                  selectInput(
                    "soilAquis",
                    "Select Soil Acquisition:",
                    choices = c("ISRIC" = "ISRIC",
                                "SSURGO (USA only)" = "SSURGO", 
                                "SLGA (AUS only)" = "SLGA", 
                                "World Modeler" = "World Modeler")
                  ),
                  checkboxInput("no_trim", 
                              tagList(
                                "Advanced: Do Not Trim Sim Outputs", 
                                shiny::span(icon("info-circle"), id = "tip_no_trim")
                              ), 
                              value = FALSE),
                ),
                box(
                  background = "green",
                  div(
                    style = "display: flex; align-items: center; gap: 10px;",
                    actionButton("runAnalysis", "Run Analysis", icon = icon("play")),
                    tags$i(
                      id = "runSpinner",
                      class = "fa fa-spinner fa-spin",
                      style = "display:none; font-size: 24px; color: white;"
                    )
                  )
                ),
                box(
                  h3("Progress"),
                  verbatimTextOutput("progressLog")
                ),
                box(
                  h3("Download Results (.zip)"),
                  downloadButton("downloadData", "Download Results")
                )
              ),
              ),
              bsTooltip("tip_input", "A trial dataset with the columns Site, Planting, Genetics, Latitude, and Longitude. Example input data is available in project files; see documentation for more information about formatting.", "right", options = list(container = "body")),
              bsTooltip("tip_tempmodel", "The template model provided here-- its crop module, reporting variables, and management controls-- will be used as the basis for all trial simulations.", "right", options = list(container = "body")),
              bsTooltip("tip_mat_hndl", "How the Genetics column of the input should be translated into the generic cultivars that APSIM uses to define the crop phenology. `Soybean RM` and `Maize RM` may be used with the template models provided. See documentation for details.", "right", options = list(container = "body")),
              bsTooltip("tip_no_trim", "By default, the daily simulation records are trimmed to the duration of the crop's development, plus an optional buffer. Selecting this option keeps the full simulation records, including empty time. WARNING: This will increase output file size.", "right", options = list(container = "body"))
              ),
    ###build gridded input file UI -----------
    tabItem(tabName = "build_input",
            fluidPage(
              h3("Build a Gridded Input File"),
              fluidRow(
                column(width = 6,
                       box(width = 12,
                           h4("Geographic Range"),
                           p("Enter the coordinates of two opposite corners of the rectangular area to cover."),
                           fluidRow(
                             column(width = 6,
                                    numericInput("cornerA_lat",  "Corner A Latitude (WGS84):",  value = 40.0, min = -90,  max = 90,  step = 0.1)
                             ),
                             column(width = 6,
                                    numericInput("cornerA_long",  "Corner A Longitude (WGS84):",  value = -88.0, min = -180,  max = 180,  step = 0.1)
                             )
                           ),
                           fluidRow(
                             column(width = 6,
                                    numericInput("cornerB_lat", "Corner B Latitude (WGS84):", value = 42.0, min = -90, max = 90, step = 0.1)
                             ),
                             column(width = 6,
                                    numericInput("cornerB_long", "Corner B Longitude (WGS84):", value = -90.0, min = -180, max = 180, step = 0.1)
                             )
                           ),
                           numericInput("grid_spacing_km", "Grid Spacing (km):", value = 50, min = 1, max = 1000, step = 1),
                           uiOutput("totallocs_UI")
                       ),
                       box(width = 12,
                           h4("Genetics"),
                           p("Enter comma-separated genetic maturity values to test (e.g. 3.5, 4.0, 4.5)."),
                           textInput("genetics_input", "Genetic Maturities:", value = "3.5, 4.0, 4.5"),
                           uiOutput("totalgenetics_UI")
                       )
                       
                ),
                column(width = 6,
                       box(width = 12,
                           h4("Planting Dates"),
                           p("Enter the range of planting dates to test within each year."),
                           fluidRow(
                             column(
                               width = 6,
                               dateInput(
                                 "planting_start",
                                 "First Planting Date:",
                                 value = as.Date("2000-05-01"),
                                 format = "MM dd",
                                 startview = "month",
                                 min = as.Date("2000-01-01"),
                                 max = as.Date("2000-12-31")
                               )
                             ),
                             column(
                               width = 6,
                               dateInput(
                                 "planting_end",
                                 "Last Planting Date:",
                                 value = as.Date("2000-06-01"),
                                 format = "MM dd",
                                 startview = "month",
                                 min = as.Date("2000-01-01"),
                                 max = as.Date("2000-12-31")
                               )
                             )
                           ),
                           numericInput(
                             "planting_step_days",
                             "Planting Date Interval (days):",
                             value = 14,
                             min = 1,
                             max = 180,
                             step = 1
                           ),
                           p("Enter the range of years to simulate."),
                           fluidRow(
                             column(width = 6,
                                    numericInput("year_start", "First Year:", value = 2015, min = 1900, max = 2100, step = 1)
                             ),
                             column(width = 6,
                                    numericInput("year_end", "Last Year:", value = 2020, min = 1900, max = 2100, step = 1)
                             )
                           ),
                           uiOutput("totalyears_UI"),
                           uiOutput("totaldates_UI")
                       ),
                       box(width = 12, 
                           h4("Summary"),
                           uiOutput("total_trialsUI"),
                           br(),
                           downloadButton("download_grid_input", "Download Input File")
                       )
                )
              ),
              br(),
              h4("Preview"),
              DTOutput("grid_input_preview")
            )
    ),
    ###config periods UI -----
    tabItem(tabName = "config_periods",
            fluidPage(
              h3("Configure Phenological Periods"),
              p(
                "Here you can rename each APSIM phenological period and optionally merge",
                "any set of periods into a single combined period.",
                "Changes take effect when you click 'Apply Configuration'."
              ),
              p(
                "Type a custom label in the 'Custom Name' column. Leave a cell blank to 
                keep the default APSIM phase name."
              ),
              p(
                tags$b("Merging:"),
                " Assign the same 'Merge Group' number to all periods you want to combine.",
                " Periods with unique or blank merge group values are kept as-is.",
                " Accumulation variables (AccRain, AccTT, AccEmTT, Duration)",
                " are", tags$em("summed"), "within a merge group; all other variables are",
                tags$em("averaged"), "."
              ),
              
              # ── Action buttons ──────────────────────────────────────────────────────
              fluidRow(
                column(width = 12,
                       div(style = "display: flex; gap: 10px; margin-bottom: 15px;",
                           actionButton("apply_period_config",  "Apply Configuration",
                                        icon = icon("check"),
                                        style = "font-weight: bold; background-color: #4CAF50;
                                color: white; border: none;"),
                           actionButton("reset_period_config",  "Reset to Defaults",
                                        icon = icon("rotate-left"))
                       )
                )
              ),
              
              # ── Per-period configuration table ──────────────────────────────────────
              # Rendered dynamically once analysis is complete
              uiOutput("period_config_tableUI"),
              
              # ── Live preview of the resulting period key ─────────────────────────────
              br(),
              h4("Preview: Resulting Period Key"),
              p(em("This reflects your pending configuration before Apply is clicked
          only the first time; after clicking Apply it shows the active state.")),
              DTOutput("period_config_preview")
            )
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
                  column(width = 4,
                         div(class = "left-panel", 
                             h3("Dataset Descriptions"),
                             p(
                               tags$br(),
                               tags$strong("trial_info:"),
                               " aligns with the input file; contains sim parameters, outcomes, and identifying information.", 
                               tags$br(),tags$br(),
                               tags$strong("daily_sim_outputs:"),
                               " the combined total output of the APSIM simulations; contains the daily records of each reporting variable.", 
                               tags$br(),tags$br(),
                               tags$strong("seasonal_data:"),
                               " the seasonal profile; contains environmental and physiological variables summarized by developmental period.", 
                               tags$br(),tags$br(),
                               tags$strong("final_x:"),
                               " joins trial_info and seasonal_data; contains the full outputs of the SCE in wide format. The naming convention of the seasonal covariates is 'Variable_Period', e.g., 'Rain_5' is the mean rainfall within the fifth period of development.",
                               tags$br(),tags$br(),
                               tags$strong("period_key:"),
                               "  table showing which APSIM phase each Period maps to."
                             )     
                         )
                  ),
                  column(width = 8,
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
                         downloadButton("downloadBoxplot", "Download Boxplot (.png)")
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
      ### map view UI  -----
    tabItem(tabName = "view_map",
            fluidPage(
              withSpinner(leafletOutput("map", height = 800), type = 4)
            )),
      ### heatmap UI ----
      tabItem(
        tabName = "heatmap",
        fluidPage(
          h3("Seasonal Heatmaps"),
          p("This section allows you to create a heatmap to visually compare values of a seasonal variable 
            across the periods of the crop's development between different trials or sites."),
          p("Use the dropdown menus to select which maturity group to view, the variable to inspect,
            and whether this comparison should be made between trials or sites. If trials are chosen, 
            the cell values will simply be the recorded values of that variable for that combination of period 
            and trial. If sites are chosen this will be the means of the same values for each site."
          ),
          p("The X axis of the plot is the developmental period and the Y axis of the plot is the trial/site. 
            Trials are labeled in the format '[Trial ID]: [Site Name] [Date Planted]'. [Genetic Maturity] is appended 
            if you are viewing all maturities at once. Trials/sites are ordered on the Y axis according to hierarchical
            clustering of the chosen variable, as indicated by the dendrogram on the plot's left margin. 
            Cells are colored by value, red/high to blue/low. The coloring is scaled relative to the other
            values recorded in that period for the trials/sites compared (column-wise).
          "),
          fluidRow(
            column(width = 2,
                   uiOutput("season_matSelectUI")),
            column(width = 2,
                   uiOutput("season_varHeatmapUI")),
            column(width = 3,
                   selectInput(inputId = "season_heatBy", label = "By Trial or by Site?", choices = c("By Trial","By Site"), selected = "By Trial")
            ),
            column(width = 2,
                   numericInput(
                     inputId = "season_cex",
                     label = "Adjust Label Size",
                     value = 16,        
                     min = 0,          
                     max = 50,          
                     step = 1        
                   ))
            ,
            column(width = 2,
                   numericInput(
                     inputId = "season_h",
                     label = "Height (px)",
                     value = 600,        
                     min = 0,          
                     max = 10000,          
                     step = 100        
                   ))
          ),
          uiOutput("season_heatmapPlotUI"),
          downloadButton("season_downloadHeatmap", "Download Seasonal Heatmap (.png)"),
          downloadButton("season_downloadMatrix", "Download Seasonal Matrix (.csv)")
        )
      ),
      ### timeline plot ----
      tabItem(
      tabName = "timelines",
      fluidPage(
        h3("Timeline Plot"),
        fluidRow(
          column(width = 3,
            uiOutput("timeline_yearSelectUI")
          ),
          column(width = 3,
            numericInput(
              inputId = "timeline_cex",
              label = "Adjust Label Size",
              value = 16,        
              min = 0,          
              max = 50,          
              step = 1        
            )
          ),
          column(width = 3,
            numericInput(
              inputId = "timeline_h",
              label = "Height (px)",
              value = 800,        
              min = 0,          
              max = 10000,          
              step = 100        
            )
          ),
        ),
        fluidRow(
          column(
            width = 3,
            box(
              width = 12,
              checkboxGroupInput(
                "selected_labels",
                "Developmental Stages",
                choices = NULL
              )
            )
          ),
          column(
            width = 9,
            plotOutput("timeline_plot", height = "auto"),
            downloadButton( "download_timeline_plot","Download Developmental Timeline (.png)")
          )
        )
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
            ,
            column(width = 3,
                   numericInput(
                     inputId = "trial_h",
                     label = "Height (px)",
                     value = 800,        
                     min = 0,          
                     max = 10000,          
                     step = 100        
                   ))
          ),
          withSpinner(uiOutput("comp_heatmapPlotUI"), type = 4),
          downloadButton(
            "trial_downloadHeatmap",
            "Download Heatmap of Seasonal Correlations (.png)"
          ),
          downloadButton("downloadEnvMatrix", "Download Seasonal Correlation Matrix (.csv)"),
          downloadButton("download_vardates", tagList(
            "Download Seasonal Analogues Report (.csv)", 
            shiny::span(icon("info-circle"), id = "tip_report")),
            ),
          br(),
          p("
            Above is a heatmap showing a similarity matrix created from the correlation of trial seasonal profiles. 
            The matrix is symmetrical: the columns are labeled below with the trial ID, and the rows are labeled to the 
            right in the format '[Trial ID:] [Site Name] [Date Planted]', followed by '[Genetic Maturity]' 
            if more than one maturity is being viewed at once. 
          "),
          h3("Dendrogram of Seasonal Similarities"),
          fluidRow(
            column(width = 3, 
                   numericInput(inputId = "k_val", label = "Cluster #", val = 3, min = 1, max = 1000, step = 1)
                   ),
            column(width = 3,
                   numericInput(
                     inputId = "dendro_cex",
                     label = "Adjust Label Size",
                     value = 1,        
                     min = 0,          
                     max = 2,          
                     step = 0.1        
                   ))
            ,
            column(width = 3,
                   numericInput(
                     inputId = "dendro_h",
                     label = "Height (px)",
                     value = 400,        
                     min = 0,          
                     max = 10000,          
                     step = 100        
                   ))
          ),
          withSpinner(uiOutput("dendroPlotUI"), type = 4),
          downloadButton("trial_downloadDendro", "Download Dendrogram Plot (.png)"),
          downloadButton("downloadDendroObj", "Download Dendrogram Object (.rds)"),
          p("
            The dendrogram above is the same dendrogram at the sides of the heatmap plot, but pulled out for easier viewing. 
          "),
          h3("Seasonal Covariate Controls"),
          p("  
            Using the controls below, you can change the decision criteria for including or excluding seasonal
            covariates from the similarity calculations."),
          p("  
            Below and to the right is a scrollable table of all of the 
            seasonal covariates available for the similarity analysis. The first column, 'Seasonal Covariate', gives the name of
            the covariate, and the second column, 'Status', gives whether or not it was included in the analysis and 
            on what criteria. The last column, 'Override', gives you the option to override whatever other criteria you set and 
            forcibly include or exclude the seasonal covariate from the analysis. 
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
                                  "Min variance within SC:", 
                                  shiny::span(icon("info-circle"), id = "tip_nzv_chk")
                                ), 
                                value = 1e-10, min = 1e-10, max = 1, step = 0.0001),
                   
                   numericInput("empty_chk", 
                                label = tagList(
                                  "Min trial data completeness:", 
                                  shiny::span(icon("info-circle"), id = "tip_empty_chk")
                                ), 
                                value = 1e-10, min = 1e-10, max = 1, step = 0.01),
                   
                   numericInput("var_chk", 
                                label = tagList(
                                  "Max SC correlation:", 
                                  shiny::span(icon("info-circle"), id = "tip_var_chk")
                                ), 
                                value = 1, min = 0, max = 1, step = 0.01),
                   
                   downloadButton("downloadParamTable", "Download SC Selections Table (.csv)")
            ),
            column(width = 8, 
                   div(style = "display: flex; gap: 10px; margin-bottom: 15px;",
                       actionButton("apply_overrides",  "Apply Overrides",
                                    icon = icon("check"),
                                    style = "font-weight: bold; background-color: #4CAF50;
                                color: white; border: none;"),
                       actionButton("reset_overrides",  "Reset to Defaults",
                                    icon = icon("rotate-left"))
                   ),
                   div(
                     id = "scroll-container",
                     withSpinner(uiOutput("customParamTableUI"), type = 4)
                   )
                  )
          )
        ),
        bsTooltip("tip_report", "Report comparing seasonal conditions for different site and planting date combinations. Gives the similarity and stability of that similarity over the simulated years. See documentation for specifics.", "below", options = list(container = "body")),
        bsTooltip("tip_exclude_startend", "Remove seasonal covariates outside the strict duration of crop development. Drops seasonal covariates associated with the first and last phenological periods, which contain the periods before sowing and after harvest.", "right", options = list(container = "body")),
        bsTooltip("tip_min_dur", "Drop seasonal covariates associated with periods that have a mean duration shorter than this value (in days). Useful for removing shortened periods (such as those a day or less in length) which may be part of the APSIM model definition but may not be relevant to the seasonal profile.", "right", options = list(container = "body")),
        bsTooltip("tip_nzv_chk", "Drop seasonal covariates with a variance lower than this value. Used to remove variables with near-zero variance, which are likely uninformative.", "right", options = list(container = "body")),
        bsTooltip("tip_empty_chk", "Drop trials with too much missing data (less than this proportion of their seasonal data is available). In the case that a simulation fails or is cut short, this can be used to remove suspicious trial data.", "right", options = list(container = "body")),
        bsTooltip("tip_var_chk", "Drop highly correlated seasonal covariates. For highly correlated pairs, the variable with the largest mean absolute correlation is removed until all pair-wise correlations in the matrix are below this value.", "right", options = list(container = "body"))
      ),
      ### TT / precip UIs ----
      tabItem(tabName = "gdd_equation",
              fluidPage(
                h2("Modify GDD Equation"),
                p(
                  "Modify the parameters of the GDD equation used to calculate Thermal Time for the charts within this tab."
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
                h2("Typical TT/Precip Accumulation"),
                p(
                  "This section allows you to compare the accumulation of precipitation and thermal 
                  time during the typical growing season at each site. The timespan of a site's \"typical\" 
                  growing season is estimated using the mean planting and harvest dates of the previous trial 
                  simulations. Thermal time and precipitation values are taken from the last ten years of weather records at that site. 
                  Select the variable to view and sites to compare for the visualization."
                ),
                fluidRow(
                  column(width = 3, 
                    selectInput(
                      "comparisonType",
                      "Select Comparison Type",
                      choices = c(
                        "Acc. Precip. (Date)" = "precip_date",
                        "Acc. Precip. (Days after Sowing)" = "precip_das",
                        "Acc. Thermal Time (Date)" = "tt_date",
                        "Acc. Thermal Time (Days after Sowing)" = "tt_das"
                      )
                  )),
                  column(width = 3,
                         numericInput(
                           inputId = "ttp1_h",
                           label = "Height (px)",
                           value = 600,        
                           min = 0,          
                           max = 10000,          
                           step = 100        
                         ))
                ),
                fluidRow(
                  column(width = 3,  
                         uiOutput("siteSelectionUI")),
                  column(
                    width = 9,
                    plotOutput("comparisonPlot", height = "auto"),
                    downloadButton("downloadComparisonPlot", "Download Plot (.png)")
                  )
                )
              )),
      tabItem(tabName = "faceted_comparison",
              fluidPage(
                h2("Site Yearly TT/Precip Totals"),
                p(
                  "This section shows, for each site, the total accumulated precipitation and thermal time during 
                  each year's typical growing season. The dashed lines on the graph represent the 
                  mean total thermal time or precipitation at that site over the last ten years. 
                  Select the sites to compare for the visualization."
                ),
                fluidRow(
                  column(width = 3, 
                    numericInput(
                      inputId = "ttpr2_cex",
                      label = "Adjust Label Size",
                      value = 5,        
                      min = 0,          
                      max = 50,          
                      step = 1        
                    )
                  ),
                  column(width = 3,
                         numericInput(
                           inputId = "ttp2_h",
                           label = "Height (px)",
                           value = 600,        
                           min = 0,          
                           max = 10000,          
                           step = 100        
                         ))
                ),
                fluidRow(
                  column(width = 3,
                         uiOutput("siteSelectionUI_faceted")),
                  column(
                    width = 9,
                    plotOutput("facetedComparisonPlot", height = "auto"),
                    downloadButton("downloadFacetedComparisonPlot", "Download Plot (.png)")
                  )
                )
              )),
      tabItem(tabName = "between_sites",
              fluidPage(
                h2("Ten Year Site TT/Precip Means"),
                p(
                  "This figure shows the 10-year averages of accumulated thermal time and precipitation 
                  for a typical growing season at each site. The dashed horizontal line represents the 
                  mean total thermal time for all selected sites, while the dashed vertical line represents 
                  the mean total precipitation for all selected sites. Select the sites to compare for the visualization."
                ),
                fluidRow(
                  column(width = 3, 
                    numericInput(
                      inputId = "ttpr3_cex",
                      label = "Adjust Label Size",
                      value = 5,        
                      min = 0,          
                      max = 50,          
                      step = 1        
                    )),
                  column(width = 3,
                         numericInput(
                           inputId = "ttp3_h",
                           label = "Height (px)",
                           value = 600,        
                           min = 0,          
                           max = 10000,          
                           step = 100        
                         )
                  )
                ),
                fluidRow(
                  column(width = 3,
                         uiOutput("siteSelectionUI_between")),
                  column(
                    width = 9,
                    plotOutput("plotBetweenSites", height = "auto"),
                    downloadButton("downloadBetweenSitesPlot", "Download Plot (.png)")
                  )
                )
              ))
    )
  )
)

# Define server logic ----
server <- function(input, output, session) {
  
  #if (basename(getwd()) == "output_files") {setwd("..")}  #prevents a directory bug when the app is reloaded in some environments.  yes i know it's bad practice. 
  codes_dir <- getwd()
  input_dir <- paste0(codes_dir,"/input")
  unlink(input_dir,recursive = T) ; dir.create(input_dir)
  
  # Reactive values for storing the analysis state and the selected variable
  if (
    all(
      file.exists(
        file.path(paste0(codes_dir,"/output_files/results/", c("final_x.csv","seasonal_data.csv","trial_info.csv","period_key.csv")))
      )
    )
  ){
    analysisDone <- reactiveVal(TRUE)
  } else {
    analysisDone <- reactiveVal(FALSE)
  }
  analysisInProgress <- reactiveVal(FALSE)
  analysisFailed <- reactiveVal(FALSE)
  
  output_dir <- paste0(codes_dir,"/output_files")
  if(!dir.exists(output_dir)) {dir.create(output_dir)}
  setwd(output_dir)
  results_dir <- paste0(output_dir,"/results")
  
  #create color palette for heatmaps
  pal_f <- colorRampPalette(brewer.pal(9,"RdYlBu")) #creates a continuous palette
  palette <- rev(pal_f(50)[1:50])
  
  seasonal_data <- reactiveVal(NULL)
  final_x       <- reactiveVal(NULL)
  period_key    <- reactiveVal(NULL)
  raw_period_key <- reactiveVal(NULL)
  
 # Front Page / Analysis ----
  
  ## download examples ------
  output$download_ex <- downloadHandler(
    filename = function() {
      "SCE_examples.zip"
    },
    content = function(file) {
      files_to_zip <- c(paste0(codes_dir,"example_input_files/soy_example_input.csv"), 
                        paste0(codes_dir,"example_input_files/maize_example_input.csv"),
                        paste0(codes_dir,"template_models/Soy_Template.apsimx"),
                        paste0(codes_dir,"template_models/Maize_Template.apsimx")
                        )
      
      # Create a temporary directory and copy files there
      tmp_dir <- tempdir()
      file_paths <- file.path(tmp_dir, files_to_zip)
      file.copy(files_to_zip, file_paths, overwrite = TRUE)
      
      # Create ZIP
      zip(zipfile = file, files = file_paths, flags = "-j")  # -j = junk the paths
    },
    contentType = "application/zip"
  )
  
  
  ## UI for example files --------
  ## Disable/enable fileInput 
  observeEvent(input$useExampleInput, {
    if (input$useExampleInput) {
      shinyjs::disable("fileUpload")
    } else {
      shinyjs::enable("fileUpload")
    }
  })
  
  ## Conditionally show radio buttons for example inputs
  output$exampleOptions <- renderUI({
    if (input$useExampleInput) {
      wellPanel(
        style = "padding: 0px;",
        div(style = "padding: 10px; margin-bottom: -15px;",
            radioButtons("exampleInput",NULL,
                         choices = c("Soybean", "Maize"),
                         inline = TRUE)
        )
      )
    }
  })
  
    ## Disable/enable template upload
  observeEvent(input$useExampleTemplate, {
    if (input$useExampleTemplate) {
      shinyjs::disable("templateUpload")
    } else {
      shinyjs::enable("templateUpload")
    }
  })
  
  ## Conditionally show radio buttons for example template models
  output$exampleTemplate <- renderUI({
    if (input$useExampleTemplate) {
      wellPanel(
        style = "padding: 0px;",
        div(style = "padding: 10px; margin-bottom: -15px;",
          radioButtons("exampleTemplate",NULL,
                       choices = c("Soybean", "Maize"),
                       inline = TRUE)
        )
      )
    }
  })
  
  ## set parameters -------
  weather_aquis <- reactiveVal("NASAPOWER")
  soil_aquis <- reactiveVal("SSURGO")
  mat_handling <- reactiveVal("Soy")
  no_trim <- reactiveVal("FALSE")
  
  for (par in c("matType","soilAquis","weatherAquis","no_trim")) {
    local({
      p <- par
      rv <- switch(p, matType=mat_handling, soilAquis=soil_aquis,
                   weatherAquis=weather_aquis, no_trim=no_trim)
      observeEvent(input[[p]], rv(input[[p]]))
    })
  }
  
  ## set progress counters -------
  nloc <- reactiveVal(0)
  ntrials <- reactiveVal(0)
  met_count <- reactiveVal(0)
  soil_count <- reactiveVal(0)
  sim_count <- reactiveVal(0)
  out_count <- reactiveVal(0)
  valid_count <- reactiveVal(0)
  prog_error <- reactiveVal(NA)
  prog_m <- reactiveVal("Using pre-loaded analysis results ...")
  
  ## run the analysis ----
  
  observeEvent(input$runAnalysis, {
    #check if analysis already in progress. if so, don't disrupt
    if (analysisInProgress()) {
      cat("Analysis already in progress.\n")
      return()
    } 
    
    prog_m(c(prog_m(), "Starting ..."))

    analysisInProgress(TRUE)
    analysisDone(FALSE)
    analysisFailed(FALSE)
    
    shinyjs::show("runSpinner")
    shinyjs::hide("sidebar_menu")
    shinyjs::hide("ttpp_sidebar_menu")
    
    #clear existing files
    unlink(paste0(output_dir,"/met"),recursive = T) ; dir.create(paste0(output_dir,"/met"))
    unlink(paste0(output_dir,"/soils"),recursive = T) ; dir.create(paste0(output_dir,"/soils"))
    unlink(paste0(output_dir,"/apsim"),recursive = T) ; dir.create(paste0(output_dir,"/apsim"))
    unlink(paste0(output_dir,"/results"),recursive = T) ; dir.create(paste0(output_dir,"/results"))
    
    # reset counters for the progress update
    prog_error(NA)
    prog_m(NULL)
    met_count(0)
    soil_count(0)
    sim_count(0)
    out_count(0)
    valid_count(0)
    ntrials(0)
    nloc(0)
    
    #check if input chosen
    if ((!input$useExampleInput & !is.null(input$fileUpload)) | 
        (input$useExampleInput & !is.null(input$exampleInput))
    ){

    } else {
      cat("No trial input choice detected.\n")
      analysisFailed(TRUE) ; analysisInProgress(FALSE)
      prog_error(c("No trial input choice detected."))
      return()
    }
    
    #check if template chosen
    if ((!input$useExampleTemplate & !is.null(input$templateUpload)) | 
        (input$useExampleTemplate & !is.null(input$exampleTemplate))
    ){

    } else {
      cat("No template model choice detected.\n")
      analysisFailed(TRUE) ; analysisInProgress(FALSE)
      prog_error(c("No template model choice detected."))
      return()
    }
    
    #set parameters
    parms <- tibble(mat_handling = mat_handling(), 
                    weather_aquis = weather_aquis(), 
                    soil_aquis = soil_aquis(),
                    no_trim = no_trim())
    write_csv(parms, paste0(codes_dir,"/output_files/parameters.csv"))
    
    ### upload template model -----
    
    prog_m(c(prog_m(), "Copying template model ..."))
    
      file.remove(list.files(input_dir, pattern = ".apsimx", full.names = TRUE))
      if(input$useExampleTemplate == TRUE){
        if (input$exampleTemplate == "Soybean") {
          send_tmp_path <- paste0(input_dir, "/Soy_Template.apsimx")
          source_tmp_path <- paste0(codes_dir,"/template_models/Soy_Template.apsimx")
        } else if (input$exampleTemplate == "Maize") {
          send_tmp_path <- paste0(input_dir, "/Maize_Template.apsimx")
          source_tmp_path <- paste0(codes_dir,"/template_models/Maize_Template.apsimx")
        }
      } else if (!is.null(input$templateUpload)) {
        send_tmp_path <- paste0(input_dir, "/", input$templateUpload$name)
        source_tmp_path <- input$templateUpload$datapath
      } else {
        cat("No template model detected.")
        analysisFailed(TRUE) ; analysisInProgress(FALSE)
        prog_error(c("No template model detected."))
        return()
      }
      file.copy(source_tmp_path, send_tmp_path, overwrite = TRUE)
      if (file.exists(send_tmp_path)) {
        cat("Template model copy successful\n")
      } else {
        cat("Template model copy failed\n")
        analysisFailed(TRUE) ; analysisInProgress(FALSE)
        prog_error(c("Template model copy failed."))
        return()
      }
    
    ### upload trial .csv -----

      prog_m(c(prog_m(), "Reading input file ..."))
      
        file.remove(list.files(input_dir, pattern = ".csv", full.names = TRUE))
        if(input$useExampleInput == TRUE){
          if (input$exampleInput == "Soybean") {
            send_tmp_path <- paste0(input_dir, "/soy_example_input.csv")
            source_tmp_path <- paste0(codes_dir,"/example_input_files/soy_example_input.csv")
          } else if (input$exampleInput == "Maize") {
            send_tmp_path <- paste0(input_dir, "/maize_example_input.csv")
            source_tmp_path <- paste0(codes_dir,"/example_input_files/maize_example_input.csv")
          }
        } else if (!is.null(input$fileUpload)) {
          send_tmp_path <- paste0(input_dir, "/", input$fileUpload$name)
          source_tmp_path <- input$fileUpload$datapath
        } else {
          cat("No input file detected.")
          analysisFailed(TRUE) ; analysisInProgress(FALSE)
          prog_error(c("No input file detected."))
          return()
        }
        file.copy(source_tmp_path, send_tmp_path, overwrite = TRUE)
        if (file.exists(send_tmp_path)) {
          cat("Input file copy successful.")
        } else {
          cat("Input file copy failed\n")
          analysisFailed(TRUE) ; analysisInProgress(FALSE)
          prog_error(c("Input file copy failed."))
          return()
        }
        
    ### call analysis script  -----
      
      #get the input and set associated progress counters
      input <- read_csv(list.files(input_dir, pattern = ".csv", full.names = TRUE), show_col_types = FALSE)
      nloc(nrow(distinct(select(input, Latitude, Longitude))))
      ntrials(nrow(input))
      
      prog_m(c(prog_m(), "Getting trial parameters ..."))
      
      future(seed = TRUE, {
        cat("Running analysis ...")
        source(paste0(codes_dir,"/apsimx.R"))  # Run the APSIMX analysis
      }) %>% then(function() {
        cat("Analysis finished.\n")
        analysisInProgress(FALSE)
        analysisDone(TRUE)  
        count_files()
      }) %>% 
        catch(function(err) {  # Catch analysis errors 
        cat("Error in analysis:", err$message, "\n")
        analysisInProgress(FALSE)
        analysisFailed(TRUE)
      })
      
      observe({
        req(analysisInProgress())  # Only count while analysis is running
        if (!analysisInProgress()) return()
        count_files()
        invalidateLater(500, session) 
      })
  })
  
  ## live folder updates ----------
  soil_dir <- paste0(codes_dir,"/output_files/soils")
  met_dir <- paste0(codes_dir,"/output_files/met")
  apsim_dir <- paste0(codes_dir,"/output_files/apsim")
  
  # Function to count files in each directory
  count_files <- function() {
    if (!(nloc() >= 1 & ntrials() >= 1)) {return()}
    if (met_count() != nloc()) {
      met_count(length(list.files(met_dir, pattern = "\\.met$", recursive = FALSE))) 
    } 
    if (soil_count() != nloc()) {
      soil_count(length(list.files(soil_dir, pattern = "\\.rds$", recursive = FALSE))) 
    } 
    if (sim_count() != ntrials()) {
      sim_count(length(list.files(apsim_dir, pattern = "\\.apsimx$", recursive = TRUE)))
    } 
    out_count(length(list.files(apsim_dir, pattern = "\\.db$", recursive = TRUE)))
    valid_count(length(list.files(apsim_dir, pattern = "\\.csv$", recursive = TRUE)))
  }
  
  output$progressLog <- renderText({
    req(analysisDone() || analysisInProgress() || analysisFailed())
    
    logs <- c()
    
    logs <- c(logs, prog_m())
    
    if (ntrials() >= 1) {
    
      if (met_count() > 0 | sim_count() > 0) {
        logs <- c(logs, sprintf("%d .met files collected (%.1f%%)", 
                                met_count(), 100 * met_count() / nloc()))
      }
      
      if (soil_count() > 0 | sim_count() > 0) {
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
    }
    
    #### error messages in the progress log ----------------
    if (analysisFailed()) {
      beep()
      logs <- c(logs, "\nERROR ///////////")
      if (!is.na(prog_error())) {logs <- c(logs, prog_error())}
      if (nloc() > 0 & ntrials() > 0){
        if (met_count() == 0){logs <- c(logs, ".met files could not be generated.")}
        if (soil_count() == 0){logs <- c(logs, ".soils files could not be generated.")}
        if (sim_count() == 0){logs <- c(logs, ".apsimx files could not be generated.")}
        if (out_count() == 0){logs <- c(logs, "No simulations ran successfully.")}
        if (met_count() != 0 & soil_count() != 0 & sim_count() != 0 & out_count() != 0) {
           logs <- c(logs, "Something went wrong while processing the results.")
         }
      }
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
    beep()
    shinyjs::hide("runSpinner")
    shinyjs::show("sidebar_spinner")
    
    trial_info <<- read_csv(paste0(results_dir, "/trial_info.csv"), show_col_types = FALSE)
    daily_sim_outputs <<- read_csv(paste0(results_dir, "/daily_sim_outputs.csv"), show_col_types = FALSE)
    
    seasonal_data(read_csv(paste0(results_dir, "/seasonal_data.csv"), show_col_types = FALSE))
    final_x(read_csv(paste0(results_dir, "/final_x.csv"), show_col_types = FALSE))
    period_key(read_csv(paste0(results_dir, "/period_key.csv"), show_col_types = FALSE))
    
    rebuilt_key <- daily_sim_outputs %>%
      ungroup() %>%
      select(PhaseName, Period) %>%
      distinct() %>%
      filter(!is.na(PhaseName)) %>%
      group_by(Period) %>%
      summarise(
        Label                   = first(PhaseName),
        `APSIM Phases Included` = paste(PhaseName, collapse = " & "),
        `Original Periods`      = paste(Period,    collapse = ", "),
        .groups = "drop"
      ) %>%
      mutate(Period = as.character(Period)) %>%
      select(Period, Label, `APSIM Phases Included`, `Original Periods`) %>%
      arrange(as.numeric(Period))
    raw_period_key(rebuilt_key)
    
        
    nametag <<- select(final_x(), ID, Site, PlantingDate_Sim, Mat) %>% 
      mutate(tag = paste0(ID,": ", Site, " ", PlantingDate_Sim),
             mtag = paste0(ID,": ", Site, " ", PlantingDate_Sim, " ", Mat))
    
    ### refresh progress counters again ----
    nloc(nrow(distinct(select(trial_info, Latitude, Longitude))))
    ntrials(nrow(trial_info))
    count_files()
    site_list(sort(unique(trial_info$Site)))
    
    req(analysisDone())
    base_temp(input$base_temp)
    max_temp(input$max_temp)
    ttpp <- ttpp_crunch() # get the daily tt/precip estimates with this slow function
    bigmet <<- ttpp$bigmet
    mean_startend <<- ttpp$mean_startend
    filtmet(ttpp$bigmet_gdd)
    
    ## show sidebar stuff ----
    shinyjs::hide("sidebar_spinner")
    shinyjs::show("ttpp_sidebar_menu")
    shinyjs::show("sidebar_menu")

  }) %>% bindEvent(analysisDone())
  
  ## crunch met data for TT/Precipitation analysis ------------
  ttpp_crunch <- function(){
    
    prev_year <- year(Sys.Date()) - 1
    id_locs <- unique(trial_info$ID_Loc)
    
    ### start and end of simulation as doy, going over 365 if wrapping over the new year -----
    mean_startend <- trial_info %>%
      transmute(Site, 
                first_doy = yday(PlantingDate_Sim),
                until_final =  as.numeric(as_date(HarvestDate_Sim) - as_date(PlantingDate_Sim)),
                final_doy = first_doy + until_final) %>% #done this way because final_doy can go over 365
      group_by(Site) %>%
      summarize(across(c(first_doy, final_doy), ~mean(.x, na.rm = TRUE)), .groups = "drop")
    
    ### get thermal time and precip for the last ten years of records ------
    met_list <- map(id_locs, function(s) {
      file_path <- paste0("./met/loc_", s, ".met")
      tryCatch({
        read_apsim_met(file_path, verbose = FALSE) %>%
          as_tibble() %>%
          filter(year >= prev_year - 9, year <= prev_year) %>%
          mutate(ID_Loc = s)
      }, error = function(e) NULL)
    })
    bigmet <- bind_rows(met_list)
    
    bigmet <- bigmet %>% left_join(distinct(trial_info, Site, ID_Loc), by = "ID_Loc", relationship = "many-to-many") 
    
    max_temp <- input$max_temp
    base_temp <- input$base_temp
    
    bigmet_gdd <- bigmet %>%
      mutate(tt = pmax((pmin(maxt, max_temp) + pmax(mint, base_temp)) / 2 - base_temp, 0)) %>%
      left_join(mean_startend, by = "Site") %>%
      filter(day >= first_doy & day <= final_doy)
    
    list(
      mean_startend = mean_startend,
      bigmet = bigmet,
      bigmet_gdd = bigmet_gdd
    )
  }

  ### recalculate GDD ------
  observeEvent({
    input$recalc_GDD
  }, {
    base_temp(input$base_temp)
    max_temp(input$max_temp)
    bigmet_gdd <- mutate(bigmet, tt = pmax((pmin(maxt,input$max_temp) + pmax(mint,input$base_temp))/2 - input$base_temp,0)) %>% ungroup()
    bigmet_gdd <- bigmet_gdd %>% left_join(mean_startend) %>% filter(day >= first_doy & day <= final_doy)
    filtmet(bigmet_gdd)
    print(head(filtmet()))
  })
  
  ## download results ----
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0("results_", Sys.Date(), ".zip")  # Name the zip file
    },
      content = function(file) {
           temp_dir <- tempdir()
        
           # Write configured versions so the download reflects any period changes
           write_csv(trial_info,                  file.path(temp_dir, "trial_info.csv"))
           write_csv(daily_sim_outputs,           file.path(temp_dir, "daily_sim_outputs.csv"))
           write_csv(seasonal_data(),  file.path(temp_dir, "seasonal_data.csv"))
           write_csv(final_x(),        file.path(temp_dir, "final_x.csv"))
           write_csv(period_key(),     file.path(temp_dir, "period_key.csv"))

           zip::zipr(file, files = file.path(temp_dir,
                                             c("trial_info.csv","daily_sim_outputs.csv",
                                               "seasonal_data.csv","final_x.csv","period_key.csv")))
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
  
  ## show and hide spinner ----
  observe({
    if (analysisInProgress()) {
      shinyjs::show("runSpinner")
    } else {
      shinyjs::hide("runSpinner")
    }
  })
  
  # Build Gridded Input File --------
  
  ## helper: generate grid points from two corners and spacing in km ----
  make_grid <- function(lat_a, lon_a, lat_b, lon_b, spacing_km) {
    lat_min <- min(lat_a, lat_b)
    lat_max <- max(lat_a, lat_b)
    lon_min <- min(lon_a, lon_b)
    lon_max <- max(lon_a, lon_b)
    
    # approximate degrees per km
    lat_step <- spacing_km / 110.574
    lon_step <- spacing_km / (111.320 * cos(mean(c(lat_min, lat_max)) * pi / 180))
    
    lats <- seq(lat_min, lat_max, by = lat_step)
    lons <- seq(lon_min, lon_max, by = lon_step)
    
    expand.grid(Latitude = round(lats, 4), Longitude = round(lons, 4))
  }
  
  ## helper: generate planting dates from range and interval ----
  make_planting_dates <- function(start_date, end_date,
                                  step_days,
                                  year_start, year_end) {
    
    start_fixed <- as.Date(paste0("2000-", format(as.Date(start_date), "%m-%d")))
    end_fixed <- as.Date(paste0("2000-", format(as.Date(end_date), "%m-%d")))
    base_dates <- seq(start_fixed, end_fixed, by = step_days)
    base_mmdd <- format(base_dates, "%m-%d")
    years <- year_start:year_end
    
    as.character(
      as.Date(
        outer(base_mmdd, years,
              function(md, y) paste0(y, "-", md))
      )
    )
  }
  
  ## helper: parse genetics input string ----
  parse_genetics <- function(genetics_str) {
    vals <- trimws(strsplit(genetics_str, ",")[[1]])
    vals <- vals[nchar(vals) > 0]
    vals
  }
  
  ## reactive: build the full grid input tibble ----
  grid_input_data <- reactive({
    req(input$cornerA_lat, input$cornerA_long,
        input$cornerB_lat, input$cornerB_long,
        input$grid_spacing_km,
        input$planting_start, input$planting_end, input$planting_step_days,
        input$year_start, input$year_end,
        input$genetics_input)
    
    validate(
      need(input$year_start <= input$year_end,    "First Year must be <= Last Year."),
      need(input$planting_start <= input$planting_end, "First Planting Date must be <= Last Planting Date."),
      need(nchar(trimws(input$genetics_input)) > 0, "Please enter at least one genetic maturity.")
    )
    
    grid    <- make_grid(input$cornerA_lat, input$cornerA_long,
                         input$cornerB_lat, input$cornerB_long,
                         input$grid_spacing_km)
    dates   <- make_planting_dates(input$planting_start, input$planting_end,
                                   input$planting_step_days,
                                   input$year_start, input$year_end)
    genetics <- parse_genetics(input$genetics_input)
    
    sites <- grid %>%
      mutate(Site = paste0("Lat", Latitude, "_Lon", Longitude))
    
    expand.grid(
      Site      = sites$Site,
      Planting  = dates,
      Genetics  = genetics,
      stringsAsFactors = FALSE
    ) %>%
      left_join(sites, by = "Site") %>%
      select(Site, Planting, Genetics, Latitude, Longitude)
  })
  
  ## summary UIs ----
  output$totallocs_UI <- renderUI({
    req(input$cornerA_lat, input$cornerA_long, input$cornerB_lat, input$cornerB_long, input$grid_spacing_km)
    grid <- make_grid(input$cornerA_lat, input$cornerA_long,
                      input$cornerB_lat, input$cornerB_long,
                      input$grid_spacing_km)
    p(em(paste0(nrow(grid), " grid locations")))
  })
  
  output$totaldates_UI <- renderUI({
    req(input$planting_start, input$planting_end, input$planting_step_days,
        input$year_start, input$year_end, input$year_start, input$year_end)
    dates <- make_planting_dates(input$planting_start, input$planting_end,
                                 input$planting_step_days,
                                 input$year_start, input$year_end)
    if (input$year_start > input$year_end) {
      p(em("First Year must be <= Last Year."), style = "color: red;") } 
    else {
      p(em(paste0(length(dates), " planting dates across ",
                  length(input$year_start:input$year_end), " years")))
    }
  })
  
  output$totalgenetics_UI <- renderUI({
    req(input$genetics_input)
    genetics <- parse_genetics(input$genetics_input)
    p(em(paste0(length(genetics), " genetic maturity value(s)")))
  })
  
  output$totalyears_UI <- renderUI({
    req()
    
  })
  
  output$total_trialsUI <- renderUI({
    tryCatch({
      n <- nrow(grid_input_data())
      p(strong(paste0("Total trials: ", n)))
    }, error = function(e) {
      p(em("Complete all fields to see total trial count."))
    })
  })
  
  ## preview table ----
  output$grid_input_preview <- renderDT({
    req(grid_input_data())
    datatable(
      grid_input_data(),
      rownames = FALSE,
      class    = "compact stripe",
      options  = list(scrollX = TRUE, paging = TRUE, searching = FALSE)
    )
  })
  
  ## download handler ----
  output$download_grid_input <- downloadHandler(
    filename = function() {
      paste0("grid_input_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write_csv(grid_input_data(), file)
    }
  )
  
  
  # Configure Periods ------
  
  ## helper: read current UI inputs into a config tibble ----
  read_period_config <- function(pk, input) {
    tibble(
      Period     = as.character(pk$Period),
      APSIMName  = pk$Label,
      CustomName = vapply(as.character(pk$Period), function(p) {
        v <- input[[paste0("pcfg_name_", p)]]
        if (is.null(v) || trimws(v) == "") pk$Label[pk$Period == p]
        else trimws(v)
      }, character(1)),
      MergeGroup = vapply(as.character(pk$Period), function(p) {
        v <- input[[paste0("pcfg_group_", p)]]
        if (is.null(v) || is.na(v)) as.character(p)
        else as.character(v)
      }, character(1))
    )
  }
  
  ## helper: build a default config from raw_period_key ----
  default_config <- function(pk) {
    tibble(
      Period     = as.character(pk$Period),
      APSIMName  = pk$Label,
      CustomName = pk$Label,
      MergeGroup = as.character(pk$Period)
    )
  }
  
  ## helper: build period_key table from a config tibble ----
  build_period_key <- function(new_config) {
    new_config %>%
      group_by(MergeGroup) %>%
      summarise(
        Label                   = first(CustomName),
        `APSIM Phases Included` = paste(APSIMName, collapse = " & "),
        `Original Periods`      = paste(Period,    collapse = ", "),
        .groups = "drop"
      ) %>%
      rename(Period = MergeGroup) %>%
      select(Period, Label, `APSIM Phases Included`, `Original Periods`) %>%
      arrange(as.numeric(Period))
  }
  
  ## helper: build seasonal_data, final_x, and period_key from daily_sim_outputs ----
  build_period_outputs <- function(daily_sim_outputs, new_config, trial_info) {
    
    RESERVE_VARS <- c("AccRain", "AccTT", "AccEmTT", "Duration", "Period_Start_Date", 
                      "Period_End_Date", "Period_Start_DOY", "Period_End_DOY", "Duration", "DOY", "Stage")
    
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
      mutate(Period = as.numeric(Period)) %>%
      arrange(ID, Period) 
    
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
  
  ## render the per-row configuration UI ----------------------------------------
  output$period_config_tableUI <- renderUI({
    req(analysisDone(), !is.null(raw_period_key()))
    
    pk <- raw_period_key()
    
    header <- fluidRow(
      column(2, strong("Original Period")),
      column(3, strong("APSIM Phase Name")),
      column(4, strong("Custom Label")),
      column(3, strong("Merge Group"))
    )
    
    rows <- lapply(seq_len(nrow(pk)), function(i) {
      p_val <- as.character(pk$Period[i])
      fluidRow(
        style = if (i %% 2 == 0) "background-color: #f9f9f9; padding: 4px 0;"
        else              "background-color: #ffffff; padding: 4px 0;",
        column(2, tags$div(style = "padding-top: 8px;", strong(paste("Period", p_val)))),
        column(3, tags$div(style = "padding-top: 8px;", pk$Label[i])),
        column(4,
               textInput(
                 inputId = paste0("pcfg_name_",  p_val),
                 label   = NULL,
                 value   = pk$Label[i],
                 width   = "100%"
               )
        ),
        column(3,
               numericInput(
                 inputId = paste0("pcfg_group_", p_val),
                 label   = NULL,
                 value   = as.numeric(p_val),
                 min     = 1,
                 step    = 1,
                 width   = "100%"
               )
        )
      )
    })
    
    div(
      style = "border: 1px solid #ddd; border-radius: 6px;
             padding: 10px; background: white;",
      tagList(header, tags$hr(style = "margin: 6px 0;"), rows)
    )
  })
  
  ## apply button ----
  observeEvent(input$apply_period_config, {
    req(!is.null(raw_period_key()))
    
    cfg <- read_period_config(raw_period_key(), input)
    out <- build_period_outputs(daily_sim_outputs, cfg, trial_info)
    seasonal_data(out$seasonal_data)
    final_x(out$final_x)
    period_key(out$period_key)
    
    tryCatch({
      write_csv(out$seasonal_data, file.path(results_dir, "seasonal_data.csv"))
      write_csv(out$final_x,       file.path(results_dir, "final_x.csv"))
      write_csv(out$period_key,    file.path(results_dir, "period_key.csv"))
      showNotification("Period configuration applied and files saved.",
                       type = "message", duration = 3)
    }, error = function(e) {
      showNotification(paste("Configuration applied but file write failed:", e$message),
                       type = "warning", duration = 6)
    })
  })
  
  ## reset button ----
  observeEvent(input$reset_period_config, {
    req(!is.null(raw_period_key()))
    
    pk  <- raw_period_key()
    def <- default_config(pk)
    
    for (p in as.character(pk$Period)) {
      updateTextInput(session,    paste0("pcfg_name_",  p), value = pk$Label[pk$Period == p])
      updateNumericInput(session, paste0("pcfg_group_", p), value = as.numeric(p))
    }
    
    out <- build_period_outputs(daily_sim_outputs, def, trial_info)
    seasonal_data(out$seasonal_data)
    final_x(out$final_x)
    period_key(out$period_key)
    
    tryCatch({
      write_csv(out$seasonal_data, file.path(results_dir, "seasonal_data.csv"))
      write_csv(out$final_x,       file.path(results_dir, "final_x.csv"))
      write_csv(out$period_key,    file.path(results_dir, "period_key.csv"))
      showNotification("Period configuration reset and files restored.",
                       type = "message", duration = 3)
    }, error = function(e) {
      showNotification(paste("Configuration reset but file write failed:", e$message),
                       type = "warning", duration = 6)
    })
  })
  
  ## live preview table ----
  output$period_config_preview <- renderDT({
    req(analysisDone(), !is.null(raw_period_key()))
    
    cfg <- read_period_config(raw_period_key(), input)
    
    datatable(
      build_period_key(cfg),
      rownames = FALSE,
      class    = "compact stripe",
      options  = list(paging = FALSE, searching = FALSE, scrollX = TRUE)
    )
  })
 
  
# View Results & Boxplot ----
  ## viewData / view data in tables below boxplot ----  
   output$viewData <- renderDT({
     req(analysisDone())
     data <- switch(input$fileToView,
                    "trial_info.csv"        = trial_info,
                    "daily_sim_outputs.csv" = daily_sim_outputs,
                    "seasonal_data.csv"     = seasonal_data(),
                    "final_x.csv"           = final_x(),
                    "period_key.csv"        = period_key())
     rdata <- mutate(data, across(where(is.numeric), ~ round(.x, 4)))
     datatable(rdata,
               escape  = FALSE,
               class   = "compact stripe",
               options = list(scrollX = TRUE))
   })
  
  
  selectedVariable <- reactiveVal()
  
  ## varSelect_boxplot ----
  output$varSelectUI <- renderUI({
     req(analysisDone(), input$fileSelectPlot)
     data <- switch(input$fileSelectPlot,
                    "trial_info.csv"        = trial_info,
                    "daily_sim_outputs.csv" = daily_sim_outputs,
                    "seasonal_data.csv"     = seasonal_data(),
                    "final_x.csv"           = final_x(),
                    "period_key.csv"        = period_key())
     selectInput("varSelect_boxplot", "Select Variable", choices = names(data)[-1])
   })
  
  
  observeEvent(input$varSelect_boxplot, {
    selectedVariable(input$varSelect_boxplot)
  }, ignoreInit = TRUE)
  
  ## store the generated boxplot for download ----
  boxplot_data <- reactiveVal()
  
   output$boxplot <- renderPlot({
     req(analysisDone(), selectedVariable())
     selected_file <- input$fileSelectPlot
     data <- switch(input$fileSelectPlot,
                    "trial_info.csv"        = trial_info,
                    "daily_sim_outputs.csv" = daily_sim_outputs,
                    "seasonal_data.csv"     = seasonal_data(),
                    "final_x.csv"           = final_x(),
                    "period_key.csv"        = period_key())
       
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
      print(boxplot_data()) 
      dev.off()
    }
  )
  
  
  # Seasonal Heatmaps ----  
  
  season_heatmap_plot <- reactiveVal(NULL)
  season_heatmap_matrix <- reactiveVal(NULL)
  
  ## select maturity for heatmap -----
  output$season_matSelectUI <- renderUI({
    req(analysisDone())
    gen_choices <- c(unique(trial_info$Mat), "ALL")
    selectInput(inputId = "season_matSelect", label = "Select Maturity", choices = gen_choices, selected = gen_choices[1])
  })
  
  ## select variable for heatmap ----
  output$season_varHeatmapUI <- renderUI({
    req(analysisDone())
    varchoice <- seasonal_data() %>% ungroup() %>%
      select(where(is.numeric)) %>%
      select(-any_of(c("ID", "Period"))) %>% names()
    selectInput("season_varSelect", "Select Variable", choices = varchoice)
  })
  
  ## generate heatmap ----
    
  create_heatmap <- function(){
    
      req(input$season_varSelect)  # Ensure a variable is selected
      req(input$season_heatBy)
      req(input$season_matSelect)
      matsel <- input$season_matSelect 
      var <- input$season_varSelect
      heatcex <- input$season_cex
      
      #create base matrix from final_x, filter to maturity selection if necessary
      if(matsel == "ALL"){var_mat <- final_x()} else {var_mat <- filter(final_x(), Mat == matsel)}
      
      #if by site, aggregate by site
      if (input$season_heatBy == "By Site") {
        var_mat <- select(var_mat, ID, Site, starts_with(var)) %>% select(-ID) %>%
          group_by(Site) %>% summarize(across(where(is.numeric), function(x){mean(x,na.rm=T)})) %>%
          column_to_rownames("Site") %>%
          remove_empty(which = "rows") %>%
          as.matrix()
        paste1 <- "Recorded Means of "
        paste2 <- " by Site (Maturity: "
        
      } else { #else use the trial records directly
        var_mat <- select(var_mat, ID, starts_with(var)) %>%
          group_by(ID) %>% summarize(across(where(is.numeric), function(x){mean(x,na.rm=T)}))
        
        if (matsel == "ALL") { #if maturity is set to "ALL", include maturity in labels
          var_mat <- left_join(var_mat, select(nametag, ID, mtag), by = join_by(ID)) %>% 
            select(-ID) %>% column_to_rownames("mtag") %>% as.matrix()
        } else {
          var_mat <- left_join(var_mat, select(nametag, ID, tag), by = join_by(ID)) %>% 
            select(-ID) %>% column_to_rownames("tag") %>% as.matrix()
        }
        paste1 <- "Recorded Values of "
        paste2 <- " by Trial (Maturity: "
      }
      
      # Convert NaNs to NAs
      var_mat[is.nan(var_mat)] <- NA
      
      # Keep the GroupLabel suffixes as axis labels instead of 1..N integers.
      # Extract them from the column names (format: Variable_GroupLabel).
      pk_labels <- period_key() %>%
        select(Period, Label) %>%
        mutate(Period = as.character(Period))
      
      col_periods <- sub("^[^_]+_", "", colnames(var_mat))
      col_labels  <- pk_labels$Label[match(col_periods, pk_labels$Period)]
      col_labels  <- ifelse(is.na(col_labels), col_periods, col_labels)
      colnames(var_mat) <- col_labels
      
      #print(var_mat)
      
      if (all(var_mat == var_mat[1,1], na.rm = T)){  #check if matrix is constant
        heatmap <- pheatmap(var_mat, angle_col = 45,
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
        heatmap <- pheatmap(var_mat,angle_col = 45,
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
                gp = gpar(fontsize = 20))
      popViewport()
      
      # Bottom "Developmental Periods" label (row 3, centered)
      pushViewport(viewport(layout.pos.row = 3, layout.pos.col = 2))
      grid.text("Developmental Periods",
                x = unit(0.5, "npc"),
                y = unit(0.5, "npc"),
                just = "center",
                gp = gpar(fontsize = 20))
      popViewport()
      
      popViewport() 
  }
  
  output$season_heatmapPlot <- renderPlot(create_heatmap())
  

  
  ## render heatmap ----
  output$season_heatmapPlotUI <- renderUI({
    graphics.off()
    req(input$season_varSelect)  # Ensure there's a selected value
    plotOutput("season_heatmapPlot", height = paste0(input$season_h,"px"), width = "100%")
  })
  
  ## heatmap download handler ----
  output$season_downloadHeatmap <- downloadHandler(
    filename = function() {
      paste0("season-heatmap_", input$season_matSelect, "_", 
             input$season_varSelect, "_", input$season_heatBy,"_",Sys.Date(), ".png")
    },
    content = function(file) {
      # Use the stored heatmap for the download
      png(file, width = 1400, height = input$season_h)
      
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
                 gp = gpar(fontsize = 20))
      popViewport()
      
      # Bottom "Developmental Periods" label (row 3, centered)
      pushViewport(viewport(layout.pos.row = 3, layout.pos.col = 2))
      grid.text("Developmental Periods",
                x = unit(0.5, "npc"),
                y = unit(0.5, "npc"),
                just = "center",
                gp = gpar(fontsize = 20))
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
  
  # View Map ----
  output$map <- renderLeaflet({
    req(analysisDone())
    locs_df <- select(trial_info, Site, Latitude, Longitude) %>% distinct()
    leaflet(locs_df) %>%
      addTiles() %>%
      addMarkers(
        lng = ~Longitude,
        lat = ~Latitude,
        label = ~Site
      )
  })
  
  # Trial Comparisons ----  
  # Trial Comparisons ----
  corr_data     <- reactiveVal(list())   # heavy stats output (no plotting)
  heatmap_state <- reactiveVal(list())   # p3 (pheatmap obj) + pdend
  corr_results  <- reactiveVal(list())
  
  pal_f <- colorRampPalette(brewer.pal(9,"RdYlBu"))
  palette <- rev(pal_f(50)[1:50])
  
  param_overrides <- reactiveValues()
  
  ## maturity selection UI ----
  output$trial_matSelectUI <- renderUI({
    req(analysisDone())
    gen_choices <- c(unique(trial_info$Mat),"ALL")
    current_val <- isolate(input$trial_matSelect)
    if (is.null(current_val) || !(current_val %in% gen_choices)) {
      current_val <- gen_choices[1]
    }
    selectInput(inputId = "trial_matSelect", label = "Select Maturity", choices = gen_choices, selected = current_val)
  })
  
  ## generate trial comp matrix ------
  compute_corr_data <- function(matsel, final_x, seasonal_data,
                                min_dur = 1, exclude_startend = TRUE,
                                nzv_chk = 1e-6, empty_chk = 0.9, var_chk = 0.9,
                                param_overrides = list()) {
    
    full_run_IDs <- select(final_x, ID, MaxStage) %>%
      filter(!is.na(MaxStage)) %>%
      filter(MaxStage == max(MaxStage)) %>% pull(ID)
    
    period_durs <- select(seasonal_data, ID, Period, Duration) %>% filter(ID %in% full_run_IDs) %>%
      group_by(Period) %>% summarise(Duration = mean(Duration))
    
    if (any(period_durs$Duration < min_dur)) {
      badp <- filter(period_durs, Duration < min_dur) %>% pull(Period)
    } else {
      badp <- NULL
    }
    
    varn <- seasonal_data %>% ungroup() %>%
      select(where(is.numeric) & !c(ID, Period, Period_Start_DOY, Duration, Period_End_DOY)) %>% names()
    
    if (matsel == "ALL") {
      final_dt <- final_x
    } else {
      final_dt <- filter(final_x, Mat == matsel)
    }
    
    final_dt <- select(final_dt, ID, starts_with(varn)) %>%
      select(where(is.numeric))
    
    full_varlist <- names(select(final_dt, -ID))
    
    final_dt <- remove_empty(final_dt, which = c("rows"), cutoff = empty_chk)
    final_dt_locked <- final_dt
    
    if (!is.null(badp)) {
      badp_vars <- names(final_dt)[!names(final_dt) %in% names(select(final_dt, !ends_with(paste0("_",badp))))]
      final_dt <- select(final_dt, !ends_with(paste0("_",badp)))
    } else {
      badp_vars <- c("")
    }
    
    if (exclude_startend) {
      startend_vars <- names(final_dt)[!names(final_dt) %in%
                                         names(select(final_dt, !ends_with(paste0("_",c(min(period_durs$Period), max(period_durs$Period))))))]
      final_dt <- select(final_dt, !ends_with(paste0("_",c(min(period_durs$Period), max(period_durs$Period)))))
    } else {
      startend_vars <- c("")
    }
    
    nzv_data <- sapply(final_dt, function(x){var(x, na.rm = TRUE)})
    nzv_vars <- names(nzv_data)[nzv_data < nzv_chk]
    nzv_vars <- nzv_vars[!is.na(nzv_vars)]
    final_dt <- select(final_dt, !any_of(nzv_vars))
    
    final_full <- filter(final_dt, ID %in% full_run_IDs) %>% column_to_rownames("ID")
    var_cor <- cor(final_full, use = "complete.obs")
    correlated_vars <- caret::findCorrelation(var_cor, cutoff = var_chk, names = T)
    final_dt <- select(final_dt, !any_of(correlated_vars))
    
    # === APPLY OVERRIDES ===
    override_kept     <- names(param_overrides)[param_overrides == "Keep"]
    override_discarded <- names(param_overrides)[param_overrides == "Discard"]
    
    for (p in override_kept) {
      if (!(p %in% names(final_dt)) && (p %in% names(final_dt_locked))) {
        final_dt[[p]] <- final_dt_locked[[p]]
      }
    }
    final_dt <- select(final_dt, -any_of(override_discarded))
    # === END OVERRIDES ===
    
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
    
    scfinal_dt <- final_dt %>%
      column_to_rownames("ID") %>%
      scale() %>% as.data.frame()
    
    id_list <- final_dt$ID
    
    print(scfinal_dt)
    
    id_cor <- cor(t(scfinal_dt), use = "pairwise.complete.obs")
    
    if (matsel == "ALL") {
      tagnames <- filter(nametag, ID %in% id_list) %>% pull(mtag)
    } else {
      tagnames <- filter(nametag, ID %in% id_list) %>% pull(tag)
    }
    
    # --- precompute clustering once here, so cex-only rebuilds in build_heatmap()
    hc <- NULL
    if (nrow(id_cor) > 2 && !any(is.na(id_cor))) {
      hc <- tryCatch(hclust(dist(id_cor), method = "complete"), error = function(e) NULL)
    }
    
    list(
      matsel      = matsel,
      IDs         = colnames(id_cor),
      nametag     = nametag,
      used_params = param_status,
      final_dt    = final_dt,
      scfinal_dt  = scfinal_dt,
      id_cor      = id_cor,
      tagnames    = tagnames,
      hc          = hc
    )
  }
  
  run_corr_data <- function() {
    req(analysisDone())
    req(input$trial_matSelect)
    tryCatch({
      new_data <- compute_corr_data(
        matsel          = input$trial_matSelect,
        final_x         = final_x(),
        seasonal_data   = seasonal_data(),
        min_dur         = input$min_dur,
        exclude_startend = input$exclude_startend,
        nzv_chk         = input$nzv_chk,
        empty_chk       = input$empty_chk,
        var_chk         = input$var_chk,
        param_overrides = reactiveValuesToList(param_overrides)
      )
      corr_data(new_data)
      
      corr_results(list(
        #matsel      = new_data$matsel,
        IDs         = new_data$IDs,
        #nametag     = new_data$nametag,
        used_params = new_data$used_params,
        #final_dt    = new_data$final_dt,
        #scfinal_dt  = new_data$scfinal_dt,
        id_cor      = new_data$id_cor
        #tagnames    = new_data$tagnames
      ))
    }, error = function(e) {
      message("Error when computing corr_data: ", e$message)
    })
  }
  
  observeEvent({
    input$trial_matSelect
    input$min_dur
    input$exclude_startend
    input$nzv_chk
    input$empty_chk
    input$var_chk
  }, {
    run_corr_data()
  })
  
  ## build trial comp heatmap plot -------
  
  build_heatmap <- function(data, trialcex) {
    req(data$id_cor)
    id_cor   <- data$id_cor
    tagnames <- data$tagnames
    matsel   <- data$matsel
    hc       <- data$hc   # precomputed clustering (NULL if too few rows / has NAs)
    
    if (nrow(id_cor) > 2 & !any(is.na(id_cor))) {

      cluster_arg <- if (!is.null(hc)) hc else TRUE
      
      if (nrow(id_cor) < 100) {
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
                       cluster_rows = cluster_arg,
                       cluster_cols = cluster_arg,
                       silent = T)
        
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
                       cluster_rows = cluster_arg,
                       cluster_cols = cluster_arg,
                       silent = T)
      }
      
      pdend <- if (!is.null(hc)) as.dendrogram(hc) else NULL
      
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
    
    list(p3 = p3, pdend = pdend)
  }
  
  observeEvent({
    corr_data()
    input$trial_cex
  }, {
    req(length(corr_data()) > 0)
    tryCatch({
      heatmap_state(build_heatmap(corr_data(), input$trial_cex))
    }, error = function(e) {
      message("Error when building heatmap: ", e$message)
    })
  })
  
  ## render trial comp heatmap ----
  observe({
    output$comp_heatmapPlotUI <- renderUI({
      graphics.off()
      plotOutput("comp_heatmapPlot", height = paste0(input$trial_h,"px"), width = "100%")
    })
  })
  
  output$comp_heatmapPlot <- renderPlot({
    req(analysisDone())
    p3 <- heatmap_state()$p3
    if (is.null(p3)) {
      print("Heatmap object is NULL")
    } else {
      plot(p3$gtable)
    }
  })
  
  ## trial comp heatmap / matrix downloads ----
  output$trial_downloadHeatmap <- downloadHandler(
    filename = function() {
      paste0("sim-heatmap-", input$trial_matSelect, "-", Sys.Date(), ".png")
    },
    content = function(file) {
      png(file, width = 1400, height = input$trial_h)
      grid::grid.draw(heatmap_state()$p3$gtable)
      dev.off()
    }
  )
  
  output$downloadEnvMatrix <- downloadHandler(
    filename = function() {
      paste0("sim-matrix", input$trial_matSelect, "-", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(corr_results()$id_cor, file)
    }
  )
  
  ## param table container ----
  output$customParamTableUI <- renderUI({
    req(corr_results()$used_params)
    
    row_outputs <- lapply(corr_results()$used_params$Parameter, function(param_name) {
      uiOutput(outputId = paste0("param_row_", param_name))
    })
    
    div(
      div(
        style = "border: 1px solid #ccc; border-bottom: none; padding: 10px; background: white;",
        fluidRow(
          column(4, strong("Seasonal Covariate")),
          column(4, strong("Status")),
          column(4, strong("Override"))
        ),
        tags$hr(style = "margin: 6px 0;")
      ),
      div(
        id = "scroll-container",
        style = "height:500px; overflow-y:auto; overflow-x:hidden;
             border:1px solid #ccc; padding:0px; white-space:normal;",
        row_outputs
      )
    )
    
  })
  
  ## render param table rows -----
  observe({
    req(corr_results()$used_params)
    
    for (i in seq_len(nrow(corr_results()$used_params))) {
      local({
        param <- corr_results()$used_params[i, ]
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
    req(corr_results()$used_params)
    
    for (param in corr_results()$used_params$Parameter) {
      btn_id <- paste0("override_", param)
      val <- input[[btn_id]]
      
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
    run_corr_data()
  })
  
  observeEvent(input$apply_overrides, {
    run_corr_data()
  })
  
  ## download param table ----
  
  get_current_param_table <- function() {
    req(corr_results()$used_params)
    
    base_table <- corr_results()$used_params
    
    base_table$Override <- sapply(base_table$Parameter, function(param) {
      param_overrides[[param]] %||% "None"
    })
    
    base_table$EffectiveStatus <- mapply(function(status, override) {
      if (override == "Keep") {
        "Kept (Override)"
      } else if (override == "Discard") {
        "Discarded (Override)"
      } else {
        status
      }
    }, base_table$Status, base_table$Override)
    base_table[, c("Seasonal Covariate", "Status", "Override", "EffectiveStatus")]
  }
  
  output$downloadParamTable <- downloadHandler(
    filename = function(){
      paste0("sc-selection-table_", input$trial_matSelect, "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      param_out <- as_tibble(get_current_param_table())
      write.csv(param_out, file)
    }
  )
  
  ## render dendrograms -----
  
  draw_dendrogram <- function(
    dend,
    nametag,
    trial_matSelect,
    k_val,
    dendro_cex
  ) {
    req(dend, dendro_cex)
    
    par(mar = c(5, 2, 2, 15))
    
    ## Label lookup
    dend_labels <- nametag[as.character(nametag$ID) %in% labels(dend), ]
    dend_labels <- dend_labels[
      match(labels(dend), as.character(dend_labels$ID)),
    ]
    
    if (trial_matSelect == "ALL") {
      labels(dend) <- dplyr::pull(dend_labels, mtag)
    } else {
      labels(dend) <- dplyr::pull(dend_labels, tag)
    }
    
    ## Styling
    dend_styled <- dend %>%
      dendextend::set("branches_k_color", k = k_val) %>%
      dendextend::set("labels_cex", dendro_cex)
    
    ## Draw
    plot(dend_styled, horiz = TRUE)
    
    dendextend::rect.dendrogram(
      dend_styled,
      k = k_val,
      horiz = TRUE,
      border = 8,
      lty = 5,
      lwd = 2
    )
    
    invisible(dend_styled)
  }
  
  output$dendroPlot <- renderPlot({
    dend <- heatmap_state()$pdend   
    
    if (is.null(dend)) {
      plot.new()
      text(0.5, 0.5, "Cannot generate dendrogam.")
      return()
    }
    
    draw_dendrogram(
      dend             = dend,
      nametag          = nametag,
      trial_matSelect  = input$trial_matSelect,
      k_val            = input$k_val,
      dendro_cex       = input$dendro_cex
    )
  })
  
  output$dendroPlotUI <- renderUI({
    plotOutput(
      "dendroPlot",
      height = paste0(input$dendro_h, "px"),
      width  = "100%"
    )
  })
  
  ## dendrogram downloads ------
  output$trial_downloadDendro <- downloadHandler(
    filename = function() {
      paste0(
        "dendrogram-plot_",
        input$trial_matSelect, "_",
        Sys.Date(), ".png"
      )
    },
    content = function(file) {
      
      png(
        file,
        width  = 1400,
        height = input$dendro_h
      )
      
      draw_dendrogram(
        dend             = heatmap_state()$pdend,  
        nametag          = nametag,
        trial_matSelect  = input$trial_matSelect,
        k_val            = input$k_val,
        dendro_cex       = input$dendro_cex
      )
      
      dev.off()
    }
  )
  
  output$downloadDendroObj <- downloadHandler(
    filename = function() {
      paste0("dendrogram-obj_", input$trial_matSelect, "_", Sys.Date(),".rds")
    },
    content = function(file) {
      placeholder <- heatmap_state()$pdend  
      write_rds(placeholder, file)
    }
  )

## function to create seasonal comparison report ----------

  make_vardates <- function(){
    
    seasoncorr_mx <- corr_results()$id_cor
    IDs <- corr_results()$IDs
    rownames(seasoncorr_mx) <- as.character(IDs)
    colnames(seasoncorr_mx) <- as.character(IDs)
    
    #load trails_x
    trials_x <- filter(trial_info(), ID %in% IDs) %>%
      mutate(PlantingDOY = yday(PlantingDate_Sim), 
             PD_mday = format(PlantingDate_Sim, "%m/%d"))
    
    #create site distances matrix
    sitedist_mx <- distm(trials_x[, c("Longitude","Latitude")],fun = distHaversine)
    rownames(sitedist_mx) <- pull(trials_x, ID)
    colnames(sitedist_mx) <- pull(trials_x, ID)
    
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
    long_comp <- filter(long_comp, ID_Loc.x >= ID_Loc.y)  #essentially taking the top diagonal of the ""comparison matrix""
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
      #print("made vardates")
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
  make_site_selector <- function(suffix) {
    renderUI({
      id_group   <- paste0("selectedSites", suffix)
      id_all     <- paste0("selectAllSites",   suffix)
      id_none    <- paste0("unselectAllSites", suffix)
      fluidRow(
        column(12, actionButton(id_all, "Select All"),
               actionButton(id_none, "Unselect All")),
        column(12, tags$label("Select Sites"),
               tags$div(style = "height:400px;overflow-y:auto;border:1px solid #ccc;padding:5px;",
                        checkboxGroupInput(id_group, NULL,
                                           choices  = site_list(),
                                           selected = site_list()[1:2])
               )
        )
      )
    })
  }
  
  output$siteSelectionUI         <- make_site_selector("")
  output$siteSelectionUI_faceted <- make_site_selector("_faceted")
  output$siteSelectionUI_between <- make_site_selector("_between")
  
  ### select all / unselect all ----  
  for (suffix in c("", "_faceted", "_between")) {
    local({
      sfx <- suffix
      observeEvent(input[[paste0("selectAllSites",   sfx)]], {
        updateCheckboxGroupInput(session, paste0("selectedSites", sfx), selected = site_list())
      })
      observeEvent(input[[paste0("unselectAllSites", sfx)]], {
        updateCheckboxGroupInput(session, paste0("selectedSites", sfx), selected = character(0))
      })
    })
  }
  
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
    dbtw_sites <- dbtw_sites %>% mutate(day = as.Date(day))
    
    is_precip <- grepl("^precip", input$comparisonType)
    is_das <- grepl("das$", input$comparisonType)
    
    data <- if (is_das) sdbtw_sites else dbtw_sites
    yvar <- if (is_precip) "acc_precip" else "acc_tt"
    
    xlab <- if (is_das) "Days after Sowing" else "Date"
    measure <- if (is_precip) "Precipitation" else "Thermal Time"
    units <- if (is_precip) "(mm)" else "(GDD)"
    
    p <- ggplot(
      data,
      aes(x = day, y = .data[[yvar]], colour = Site)
    ) +
      geom_line() +
      scale_color_hue(direction = 1) +
      {if(!is_das) scale_x_date(date_breaks = "2 weeks", date_labels = "%b %d")} +
      labs(
        x = xlab,
        y = paste("Daily Mean Accumulated", measure, units),
        title = paste("Typical Accumulated", measure, "at a Site by", xlab)
      ) +
      theme_minimal() +
      theme(text = element_text(size = 15))

    comparison_plot_data(p)  # Store the plot in a reactive value
    print(p)  # Render the plot
  },
  height = function() {
    input$ttp1_h
  })

  
  ### download handler for the daily TT/Precip plot ----
  output$downloadComparisonPlot <- downloadHandler(
    filename = function() {
      paste0("comparison-plot_", input$comparisonType, "_", Sys.Date(), ".png")
    },
    content = function(file) {
      png(file, width = 1400, height = input$ttp1_h)
      print(comparison_plot_data())  # Print the stored plot
      dev.off()
    }
  )
  
  ## TT/precip2 (faceted) ---- 
  ### store the generated TT/precip 2 plot (faceted) for download ----
  faceted_comparison_plot_data <- reactiveVal()
  
  output$facetedComparisonPlot <- renderPlot({
    req(input$selectedSites_faceted)
    ttpr2_cex <- input$ttpr2_cex
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
      geom_label(label = wthn_sites$year, size = ttpr2_cex, 
                 aes(color = year)) +
      labs(x = "Acc. Precipitation (mm)", y = "Acc. Thermal Time (GDD)",
           title = "Total Thermal Time and Precipitation within the Last Ten Seasons") +
      facet_wrap(vars(Site)) +
      theme_minimal() +
      theme(legend.position = "none",
            text = element_text(size = 15)) +
      scale_x_continuous(expand = expansion(mult = 0.1)) +
      scale_y_continuous(expand = expansion(mult = 0.1))
    
    faceted_comparison_plot_data(p)  # Store the plot in a reactive value
    print(p)  # Render the plot
  },
  height = function() {
    input$ttp2_h
  })
  
  ### download handler for TT/precip 2 plot ----
  output$downloadFacetedComparisonPlot <- downloadHandler(
    filename = function() {
      paste0("faceted-comparison-plot_", Sys.Date(), ".png")
    },
    content = function(file) {
      png(file, width = 1400, height = input$ttp2_h)
      print(faceted_comparison_plot_data())  # Print the stored plot
      dev.off()
    }
  )  
  
  ## TT/precip3 (between) ---- 
  ### store the generated TT/precip 3 plot (between) for download ----
  between_sites_plot_data <- reactiveVal()
  
  output$plotBetweenSites <- renderPlot({
    req(analysisDone())
    ttpr3_cex <- input$ttpr3_cex
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
      geom_label(aes(label = Site), size = ttpr3_cex) +
      theme_minimal() +
      labs(x = "Acc. Precipitation (mm)", y = "Acc. Thermal Time (GDD)", 
           title = "Ten Year Site Averages for a Typical Growing Season") +
      theme(legend.position = "none",
            text = element_text(size = 15)) +
      scale_x_continuous(expand = expansion(mult = 0.1)) +
      scale_y_continuous(expand = expansion(mult = 0.1))
    
    between_sites_plot_data(p)  # Store the plot in a reactive value
    print(p)  # Render the plot
  },
  height = function() {
    input$ttp3_h
  })
  
  ### download handler for TT/precip 3 plot -----
  output$downloadBetweenSitesPlot <- downloadHandler(
    filename = function() {
      paste0("between-sites-plot_", Sys.Date(), ".png")
    },
    content = function(file) {
      png(file, width = 1400, height = input$ttp3_h)
      print(between_sites_plot_data())  # Print the stored plot
      dev.off()
    }
  )

# Sheila's Plots -----------

## Timeline Plot -----------

  observe({
    req(analysisDone())
    req(period_key())

    pkey <- period_key() %>%
      mutate(Period = as.character(Period)) %>%
      select(Period, Label)
    
    updateCheckboxGroupInput(
      session,
      "selected_labels",
      choices = pkey$Label,
      selected = pkey$Label
    )
    
  })
  
### select year -------
  output$timeline_yearSelectUI <- renderUI({
    req(analysisDone())
    gen_choices <- c(sort(unique(trial_info$Year)), "ALL")
    selectInput(inputId = "timeline_yearSelect", label = "Select Year", 
                choices = gen_choices, selected = gen_choices[1])
  })

### process plot data ----
  stage_data <- reactive({
    req(analysisDone())
    
    if (input$timeline_yearSelect == "ALL") {
      rsl_p <- final_x()
    } else {
      rsl_p <- filter(final_x(), Year == input$timeline_yearSelect)
    }
    
    rsl_p <- rsl_p %>%
      mutate(
        Genetics = factor(Genetics),
        Planting_genetics = paste(PlantingDate_Sim, Genetics, sep = "_")
      ) %>%
      select(
        ID, Site, Latitude, Year,
        PlantingDate_Sim, Genetics, HarvestDate_Sim, Planting_genetics,
        starts_with("Period_Start_Date")
      ) %>%
      arrange(Latitude, Site, Year, PlantingDate_Sim, Genetics) %>%
      unique() %>%
      pivot_longer(
        cols = starts_with("Period_Start_Date"),
        names_to = "Period",
        values_to = "Date"
      ) %>%
      mutate(Period = gsub("Period_Start_Date_", "", Period)) %>%
      droplevels()
    
    pkey <- period_key() %>%
      mutate(Period = as.character(Period)) %>%
      select(Period, Label)
    
    left_join(rsl_p, pkey, by = "Period") %>%
      filter(Label %in% input$selected_labels, !is.na(Date)) %>%
      mutate(
        Date_plot = as.Date(paste0("2000-", format(Date, "%m-%d"))),
        PlantingDate_plot = as.Date(paste0("2000-", format(PlantingDate_Sim, "%m-%d"))),
        HarvestDate_plot  = as.Date(paste0("2000-", format(HarvestDate_Sim, "%m-%d")))
      )
  })
  
  ### render timeline plot -----
  timeline_plot_obj <- reactive({
    req(input$timeline_yearSelect)
    req(input$timeline_cex)
    
    rsl_p2 <- stage_data()
    req(nrow(rsl_p2) > 0)
    timeline_cex <- input$timeline_cex
    
    # --- Build a y-axis key: one row per unique ID, ordered by Latitude then Site then Year ---
    id_key <- rsl_p2 %>%
      select(ID, Site, Latitude, Year, Planting_genetics) %>%
      distinct() %>%
      arrange(Latitude, Site, Year, Planting_genetics) %>%
      mutate(y_pos = row_number())
    
    # Attach y positions back to the long data
    rsl_p2 <- rsl_p2 %>%
      left_join(id_key, by = c("ID", "Site", "Latitude", "Year", "Planting_genetics"))
    
    # --- Dashed separator lines between Sites ---
    # Place a line at the boundary between each pair of adjacent Sites (by latitude order)
    site_order <- id_key %>%
      select(Site, Latitude) %>%
      distinct() %>%
      arrange(Latitude)
    
    site_boundaries <- id_key %>%
      group_by(Site) %>%
      summarise(max_y = max(y_pos), .groups = "drop") %>%
      # Drop the last site — no line needed after the final band
      left_join(site_order, by = "Site") %>%
      arrange(Latitude) %>%
      slice(-n()) %>%
      mutate(line_y = max_y + 0.5)
    
    # --- Y-axis labels: one label per Site, positioned at the band midpoint ---
    site_labels <- id_key %>%
      group_by(Site, Latitude) %>%
      summarise(mid_y = mean(y_pos), .groups = "drop") %>%
      arrange(Latitude)
    
    # --- Color palette ---
    gen_levels <- levels(factor(rsl_p2$Genetics))
    n <- length(gen_levels)
    vir   <- viridisLite::viridis(n, option = "D")[1:floor(0.8 * n)]
    greys <- gray.colors(n - length(vir), start = 0.2, end = 0.6)
    cols  <- setNames(c(vir, greys), gen_levels)
    
    rsl_p2 <- rsl_p2 %>%
      mutate(
        Label = factor(Label, levels = unique(Label[order(Period)]))
      )
    
    ggplot(rsl_p2, aes(x = Date_plot, y = y_pos)) +
      # Season span bars
      geom_errorbarh(
        aes(xmin = PlantingDate_plot, xmax = HarvestDate_plot, color = Genetics),
        height = 0,
        position = position_dodge(width = 1)
      ) +
      # Stage points
      geom_point(
        aes(color = Genetics, shape = Label),
        size = timeline_cex / 4
      ) +
      # Dashed lines between Sites
      geom_hline(
        data = site_boundaries,
        aes(yintercept = line_y),
        linetype = "dashed",
        color = "grey40",
        linewidth = 0.4
      ) +
      # Site name labels on y axis
      scale_y_continuous(
        breaks = site_labels$mid_y,
        labels = site_labels$Site,
        expand = expansion(add = 0.5)
      ) +
      scale_x_date(date_breaks = "2 weeks", date_labels = "%b %d") +
      scale_color_manual(values = cols) +
      scale_shape_manual(values = 1:length(unique(rsl_p2$Label))) +
      theme_bigstatsr() +
      theme(
        axis.text.x  = element_text(angle = 45, vjust = 0.5, size = 0.8*timeline_cex),
        axis.ticks.y = element_blank(),
        axis.text.y = element_text(size = 0.8*timeline_cex), 
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank()
      ) +
      labs(
        x     = "",
        y     = "",
        title = "Developmental stages by planting time and maturity group",
        color = "Maturity group"
      )
  })
  
  output$timeline_plot <- renderPlot({
    timeline_plot_obj()
  },
  height = function() {
    input$timeline_h
  })
  
  ### download timeline plot ---------
  output$download_timeline_plot <- downloadHandler(
    filename = function() {
      paste0("development-timeline-plot", Sys.Date(), ".png")
    },
    content = function(file) {
      png(file, width = 1400, height = input$timeline_h)
      print(timeline_plot_obj())  
      dev.off()
    }
  )
  
}

# Run the app ----
shinyApp(ui = ui, server = server)