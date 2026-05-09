# =============================================================================
# SCE — "Configure Periods" feature additions
# =============================================================================
# This file contains TWO sections:
#
#  SECTION 1 — UI additions  (splice into your existing ui definition)
#  SECTION 2 — Server additions  (splice into your existing server function)
#
# Search for the SPLICE POINT comments to find exactly where each block goes.
# =============================================================================


# =============================================================================
# SECTION 1 — UI ADDITIONS
# =============================================================================

# ── SPLICE POINT A ────────────────────────────────────────────────────────────
# Location: inside the hidden div with id = "sidebar_menu", after the last
#           existing menuItem (i.e. after the "View Trial Similarities" item).
#
# Replace this existing block:
#
#   hidden(
#     div(id = "sidebar_menu",
#         sidebarMenu(
#           menuItem("View Results",           tabName = "results",    icon = icon("table-list")),
#           menuItem("View Map",               tabName = "view_map",   icon = icon("map")),
#           menuItem("View Seasonal Heatmaps", tabName = "heatmap",    icon = icon("fire")),
#           menuItem("View Trial Similarities",tabName = "trial_comp", icon = icon("seedling"))
#         )
#     )
#   ),
#
# With this (one new menuItem added at the end):

hidden(
  div(id = "sidebar_menu",
      sidebarMenu(
        menuItem("View Results",            tabName = "results",          icon = icon("table-list")),
        menuItem("View Map",                tabName = "view_map",         icon = icon("map")),
        menuItem("View Seasonal Heatmaps",  tabName = "heatmap",          icon = icon("fire")),
        menuItem("View Trial Similarities", tabName = "trial_comp",       icon = icon("seedling")),
        menuItem("Configure Periods",       tabName = "config_periods",   icon = icon("sliders"))
      )
  )
),
# ── END SPLICE POINT A ────────────────────────────────────────────────────────


# ── SPLICE POINT B ────────────────────────────────────────────────────────────
# Location: inside tabItems(...), after the closing brace of the last
#           tabItem (the "between_sites" tabItem), just before the closing
#           parenthesis of tabItems().
#
# Paste the entire tabItem below in that position:

tabItem(tabName = "config_periods",
        fluidPage(
          h3("Configure Phenological Periods"),
          p(
            "Here you can rename each APSIM phenological period and optionally merge",
            "any set of periods into a single combined period.",
            "Changes take effect when you click ", tags$b("Apply Configuration"), "."
          ),
          p(
            tags$b("Renaming:"),
            " Type a custom label in the 'Custom Name' column.",
            " Leave a cell blank to keep the default APSIM stage name."
          ),
          p(
            tags$b("Merging:"),
            " Assign the same 'Merge Group' number to all periods you want to combine.",
            " Periods with unique or blank merge group values are kept as-is.",
            " Accumulation variables (Rain, AccRain, ThermalTime, AccTT, AccEmTT, Duration)",
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
# ── END SPLICE POINT B ────────────────────────────────────────────────────────


# =============================================================================
# SECTION 2 — SERVER ADDITIONS
# =============================================================================
# Paste the entire block below inside your server function.
# A good location is immediately after the "## immediately after analysis ----"
# observe() block (the one that ends with `}) %>% bindEvent(analysisDone())`).
# =============================================================================


# ── Configure Periods — server ───────────────────────────────────────────────

## Reactive: raw period list, populated once analysis completes ---------------
raw_period_key <- reactiveVal(NULL)   # the original period_key from the sim

observe({
  req(analysisDone())
  raw_period_key(period_key)          # period_key loaded from results CSV
}) %>% bindEvent(analysisDone())


## Reactive: stores the *applied* configuration (a data.frame) ----------------
#  Columns: Period (factor), APSIMName, CustomName, MergeGroup
applied_config <- reactiveVal(NULL)

## Helper: build a default config from whatever period_key currently holds ----
default_config <- function(pk) {
  tibble(
    Period     = pk$Period,
    APSIMName  = pk$`APSIM StageName`,
    CustomName = pk$`APSIM StageName`,   # pre-fill with APSIM name
    MergeGroup = as.character(pk$Period) # each period in its own group by default
  )
}

## Seed the applied_config once analysis is done ------------------------------
observe({
  req(analysisDone(), !is.null(raw_period_key()))
  if (is.null(applied_config())) {
    applied_config(default_config(raw_period_key()))
  }
}) %>% bindEvent(raw_period_key())


## Render the per-row configuration UI ----------------------------------------
output$period_config_tableUI <- renderUI({
  req(analysisDone(), !is.null(raw_period_key()))
  
  pk     <- raw_period_key()
  config <- isolate(applied_config()) %||% default_config(pk)
  
  # Header row
  header <- fluidRow(
    column(2, strong("Period")),
    column(3, strong("APSIM Stage Name")),
    column(4, strong("Custom Name")),
    column(3, strong("Merge Group"))
  )
  
  rows <- lapply(seq_len(nrow(pk)), function(i) {
    p_val  <- as.character(pk$Period[i])
    cfg_i  <- config[config$Period == p_val, ]
    
    cur_name  <- if (nrow(cfg_i) == 1) cfg_i$CustomName  else pk$`APSIM StageName`[i]
    cur_group <- if (nrow(cfg_i) == 1) cfg_i$MergeGroup  else p_val
    
    fluidRow(
      style = if (i %% 2 == 0) "background-color: #f9f9f9; padding: 4px 0;"
      else              "background-color: #ffffff; padding: 4px 0;",
      column(2, tags$div(style = "padding-top: 8px;", strong(paste("Period", p_val)))),
      column(3, tags$div(style = "padding-top: 8px;", pk$`APSIM StageName`[i])),
      column(4,
             textInput(
               inputId = paste0("pcfg_name_",  p_val),
               label   = NULL,
               value   = cur_name,
               width   = "100%"
             )
      ),
      column(3,
             textInput(
               inputId = paste0("pcfg_group_", p_val),
               label   = NULL,
               value   = cur_group,
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


## Apply button — read inputs, compute merged outputs -------------------------
observeEvent(input$apply_period_config, {
  req(!is.null(raw_period_key()))
  
  pk <- raw_period_key()
  
  # ── 1. Collect user-entered names and merge groups ─────────────────────────
  new_config <- tibble(
    Period     = as.character(pk$Period),
    APSIMName  = pk$`APSIM StageName`,
    CustomName = vapply(as.character(pk$Period), function(p) {
      v <- input[[paste0("pcfg_name_",  p)]]
      if (is.null(v) || trimws(v) == "") pk$`APSIM StageName`[pk$Period == p]
      else trimws(v)
    }, character(1)),
    MergeGroup = vapply(as.character(pk$Period), function(p) {
      v <- input[[paste0("pcfg_group_", p)]]
      if (is.null(v) || trimws(v) == "") as.character(p)
      else trimws(v)
    }, character(1))
  )
  applied_config(new_config)
  
  # ── 2. Decide the label for each merge group ────────────────────────────────
  # Use the custom name of the *first* (lowest-Period) member of each group.
  group_labels <- new_config %>%
    group_by(MergeGroup) %>%
    slice(1) %>%
    ungroup() %>%
    select(MergeGroup, GroupLabel = CustomName)
  
  # ── 3. Variables that should be summed vs. meaned on merge ─────────────────
  SUM_VARS <- c("Rain", "AccRain", "ThermalTime", "AccTT", "AccEmTT", "Duration")
  
  # ── 4. Rebuild seasonal_data with merged periods ────────────────────────────
  # Join the merge-group mapping onto seasonal_data
  period_to_group <- select(new_config, Period, MergeGroup)
  
  sd_merged <- seasonal_data %>%
    mutate(Period = as.character(Period)) %>%
    left_join(period_to_group, by = "Period") %>%
    group_by(ID, MergeGroup) %>%
    summarise(
      across(
        where(is.numeric) & any_of(SUM_VARS),
        ~ sum(.x, na.rm = TRUE)
      ),
      across(
        where(is.numeric) & !any_of(SUM_VARS),
        ~ mean(.x, na.rm = TRUE)
      ),
      Period_Start_Date = min(Period_Start_Date, na.rm = TRUE),
      Period_End_Date   = max(Period_End_Date,   na.rm = TRUE),
      .groups = "drop"
    ) %>%
    left_join(group_labels, by = "MergeGroup") %>%
    rename(Period = MergeGroup) %>%
    relocate(ID, Period)
  
  # ── 5. Rebuild final_x with merged, renamed column suffixes ─────────────────
  # Column names in final_x use the pattern Variable_Period.
  # Re-pivot from sd_merged using GroupLabel as the suffix.
  sd_for_pivot <- sd_merged %>%
    select(-Period_Start_Date, -Period_End_Date) %>%   # dates not pivoted
    rename(PivotPeriod = Period)
  
  fx_merged <- pivot_wider(
    sd_for_pivot,
    names_from  = PivotPeriod,
    values_from = setdiff(names(sd_for_pivot), c("ID", "PivotPeriod"))
  ) %>%
    right_join(trial_info, ., by = "ID")
  
  # ── 6. Rebuild period_key display table ─────────────────────────────────────
  pk_merged <- new_config %>%
    group_by(MergeGroup) %>%
    summarise(
      CustomName    = first(CustomName),
      `APSIM Stages Included` = paste(APSIMName, collapse = " → "),
      `Original Periods`      = paste(Period,    collapse = ", "),
      .groups = "drop"
    ) %>%
    rename(
      `Period / Group` = MergeGroup,
      `Label`          = CustomName
    ) %>%
    select(`Period / Group`, Label, `APSIM Stages Included`, `Original Periods`)
  
  # ── 7. Push to reactive stores ───────────────────────────────────────────────
  configured_seasonal_data(sd_merged)
  configured_final_x(fx_merged)
  configured_period_key(pk_merged)
  
  showNotification("Period configuration applied.", type = "message", duration = 3)
})


## Reset button ----------------------------------------------------------------
observeEvent(input$reset_period_config, {
  req(!is.null(raw_period_key()))
  
  def <- default_config(raw_period_key())
  applied_config(def)
  
  # Reset text inputs in the UI
  pk <- raw_period_key()
  for (p in as.character(pk$Period)) {
    cfg_i <- def[def$Period == p, ]
    updateTextInput(session, paste0("pcfg_name_",  p), value = cfg_i$CustomName)
    updateTextInput(session, paste0("pcfg_group_", p), value = cfg_i$MergeGroup)
  }
  
  # Restore originals
  configured_seasonal_data(seasonal_data)
  configured_final_x(final_x)
  configured_period_key(
    raw_period_key() %>%
      rename(Label = `APSIM StageName`) %>%
      mutate(`Period / Group` = as.character(Period),
             `APSIM Stages Included` = Label,
             `Original Periods`      = as.character(Period)) %>%
      select(`Period / Group`, Label, `APSIM Stages Included`, `Original Periods`)
  )
  
  showNotification("Period configuration reset to defaults.", type = "message", duration = 3)
})


## Reactive stores for the configured (possibly merged) outputs ----------------
# These are what the rest of the app reads instead of the raw globals.
configured_seasonal_data <- reactiveVal(NULL)
configured_final_x       <- reactiveVal(NULL)
configured_period_key    <- reactiveVal(NULL)

# Seed them from the raw data once analysis completes (before any config applied)
observe({
  req(analysisDone())
  configured_seasonal_data(seasonal_data)
  configured_final_x(final_x)
  
  pk <- period_key %>%
    rename(Label = `APSIM StageName`) %>%
    mutate(`Period / Group`        = as.character(Period),
           `APSIM Stages Included` = Label,
           `Original Periods`      = as.character(Period)) %>%
    select(`Period / Group`, Label, `APSIM Stages Included`, `Original Periods`)
  configured_period_key(pk)
  
}) %>% bindEvent(analysisDone())


## Live preview table in the Configure Periods tab ----------------------------
output$period_config_preview <- renderDT({
  req(!is.null(configured_period_key()))
  datatable(
    configured_period_key(),
    rownames = FALSE,
    class    = "compact stripe",
    options  = list(paging = FALSE, searching = FALSE, scrollX = TRUE)
  )
})


# ── END Configure Periods — server ───────────────────────────────────────────


# =============================================================================
# SECTION 3 — REPLACEMENTS IN EXISTING SERVER CODE
# =============================================================================
# The blocks below show every place in your existing server where you currently
# reference  seasonal_data / final_x / period_key  directly.
# Replace each reference with the configured_* reactive equivalent.
# Each replacement is shown as a FIND → REPLACE pair.
# =============================================================================

# ─────────────────────────────────────────────────────────────────────────────
# REPLACEMENT 1 — viewData (results tab table)
# ─────────────────────────────────────────────────────────────────────────────
# FIND (inside output$viewData <- renderDT):
#
#     data <- switch(input$fileToView,
#                    "trial_info.csv"        = trial_info,
#                    "daily_sim_outputs.csv" = daily_sim_outputs,
#                    "seasonal_data.csv"     = seasonal_data,
#                    "final_x.csv"           = final_x,
#                    "period_key.csv"        = period_key)
#
# REPLACE WITH:

# output$viewData <- renderDT({
#   req(analysisDone())
#   data <- switch(input$fileToView,
#                  "trial_info.csv"        = trial_info,
#                  "daily_sim_outputs.csv" = daily_sim_outputs,
#                  "seasonal_data.csv"     = configured_seasonal_data(),
#                  "final_x.csv"           = configured_final_x(),
#                  "period_key.csv"        = configured_period_key())
#   rdata <- mutate(data, across(where(is.numeric), ~ round(.x, 4)))
#   datatable(rdata,
#             escape  = FALSE,
#             class   = "compact stripe",
#             options = list(scrollX = TRUE))
# })


# ─────────────────────────────────────────────────────────────────────────────
# REPLACEMENT 2 — varSelectUI (boxplot variable selector)
# ─────────────────────────────────────────────────────────────────────────────
# FIND (inside output$varSelectUI <- renderUI):
#
#     data <- switch(input$fileSelectPlot,
#                    "trial_info.csv"        = trial_info,
#                    "daily_sim_outputs.csv" = daily_sim_outputs,
#                    "seasonal_data.csv"     = seasonal_data,
#                    "final_x.csv"           = final_x,
#                    "period_key.csv"        = period_key)
#
# REPLACE WITH:

# output$varSelectUI <- renderUI({
#   req(analysisDone(), input$fileSelectPlot)
#   data <- switch(input$fileSelectPlot,
#                  "trial_info.csv"        = trial_info,
#                  "daily_sim_outputs.csv" = daily_sim_outputs,
#                  "seasonal_data.csv"     = configured_seasonal_data(),
#                  "final_x.csv"           = configured_final_x(),
#                  "period_key.csv"        = configured_period_key())
#   selectInput("varSelect_boxplot", "Select Variable", choices = names(data)[-1])
# })


# ─────────────────────────────────────────────────────────────────────────────
# REPLACEMENT 3 — boxplot renderPlot
# ─────────────────────────────────────────────────────────────────────────────
# FIND (inside output$boxplot <- renderPlot):
#
#     data <- switch(input$fileSelectPlot,
#                    "trial_info.csv"        = trial_info,
#                    "daily_sim_outputs.csv" = daily_sim_outputs,
#                    "seasonal_data.csv"     = seasonal_data,
#                    "final_x.csv"           = final_x,
#                    "period_key.csv"        = period_key)
#
# REPLACE WITH:

# output$boxplot <- renderPlot({
#   req(analysisDone(), selectedVariable())
#   selected_file <- input$fileSelectPlot
#   data <- switch(input$fileSelectPlot,
#                  "trial_info.csv"        = trial_info,
#                  "daily_sim_outputs.csv" = daily_sim_outputs,
#                  "seasonal_data.csv"     = configured_seasonal_data(),
#                  "final_x.csv"           = configured_final_x(),
#                  "period_key.csv"        = configured_period_key())
#   # ... rest of existing boxplot code unchanged ...
# })


# ─────────────────────────────────────────────────────────────────────────────
# REPLACEMENT 4 — season_varHeatmapUI (heatmap variable selector)
# ─────────────────────────────────────────────────────────────────────────────
# FIND (inside output$season_varHeatmapUI <- renderUI):
#
#     varchoice <- seasonal_data %>% ungroup() %>%
#       select(where(is.numeric) & !c(ID, Period)) %>% names()
#
# REPLACE WITH:
#
#     varchoice <- configured_seasonal_data() %>% ungroup() %>%
#       select(where(is.numeric) & !c(ID, Period)) %>% names()


# ─────────────────────────────────────────────────────────────────────────────
# REPLACEMENT 5 — create_heatmap() function  (seasonal heatmap)
# ─────────────────────────────────────────────────────────────────────────────
# The heatmap builds its matrix from final_x and uses integer column indices.
# After merging, column suffixes are GroupLabels, not integers, so the existing
# `colnames(var_mat) <- 1:ncol(var_mat)` line is replaced with the label names.
#
# FIND near the top of create_heatmap():
#
#     if(matsel == "ALL"){var_mat <- final_x} else {var_mat <- filter(final_x, Mat == matsel)}
#
# REPLACE WITH:
#
#     cfg_fx <- configured_final_x()
#     if(matsel == "ALL"){var_mat <- cfg_fx} else {var_mat <- filter(cfg_fx, Mat == matsel)}
#
# ──────────────────────────────────────────────────────────────────────────────
# FIND (a few lines later in create_heatmap()):
#
#     colnames(var_mat) <- 1:ncol(var_mat)
#
# REPLACE WITH:
#
#     # Keep the GroupLabel suffixes as axis labels instead of 1..N integers.
#     # Extract them from the column names (format: Variable_GroupLabel).
#     col_labels <- sub("^[^_]+_", "", colnames(var_mat))  # everything after first "_"
#     colnames(var_mat) <- col_labels


# ─────────────────────────────────────────────────────────────────────────────
# REPLACEMENT 6 — ID_corr() function  (trial similarity)
# ─────────────────────────────────────────────────────────────────────────────
# FIND the function signature call inside run_ID_corr():
#
#     ID_corr(
#       matsel       = input$trial_matSelect,
#       final_x      = final_x,
#       seasonal_data = seasonal_data,
#       ...
#     )
#
# REPLACE WITH:
#
#     ID_corr(
#       matsel        = input$trial_matSelect,
#       final_x       = configured_final_x(),
#       seasonal_data = configured_seasonal_data(),
#       ...
#     )


# ─────────────────────────────────────────────────────────────────────────────
# REPLACEMENT 7 — viewKey (period key table in heatmap tab)
# ─────────────────────────────────────────────────────────────────────────────
# FIND (inside output$viewKey <- renderDT):
#
#     datatable(arrange(period_key, Period), ...)
#
# REPLACE WITH:
#
#     datatable(configured_period_key(), ...)
#
# (No arrange() needed; configured_period_key is already ordered.)


# ─────────────────────────────────────────────────────────────────────────────
# REPLACEMENT 8 — downloadData (zip download)
# ─────────────────────────────────────────────────────────────────────────────
# The existing handler zips the files already on disk from the results_dir.
# To export the *configured* versions, write them out first.
#
# FIND (inside the content function of output$downloadData):
#
#     files <- list.files(results_dir, full.names = TRUE)
#
# REPLACE the entire content function with:
#
# content = function(file) {
#   temp_dir <- tempdir()
#
#   # Write configured versions so the download reflects any period changes
#   write_csv(trial_info,                  file.path(temp_dir, "trial_info.csv"))
#   write_csv(daily_sim_outputs,           file.path(temp_dir, "daily_sim_outputs.csv"))
#   write_csv(configured_seasonal_data(),  file.path(temp_dir, "seasonal_data.csv"))
#   write_csv(configured_final_x(),        file.path(temp_dir, "final_x.csv"))
#   write_csv(configured_period_key(),     file.path(temp_dir, "period_key.csv"))
#
#   file_paths <- file.path(temp_dir,
#     c("trial_info.csv","daily_sim_outputs.csv",
#       "seasonal_data.csv","final_x.csv","period_key.csv"))
#   zip::zipr(file, files = file_paths)
# }