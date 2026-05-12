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





## reactive: stores the *applied* configuration (a data.frame) ----------------
#  Columns: Period (factor), APSIMName, CustomName, MergeGroup
applied_config <- reactiveVal(NULL)

## helper: build a default config from whatever period_key currently holds ----
default_config <- function(pk) {
  tibble(
    Period     = pk$Period,
    APSIMName  = pk$Label,
    CustomName = pk$Label,
    MergeGroup = pk$Period
  )
}

## render the per-row configuration UI ----------------------------------------
output$period_config_tableUI <- renderUI({
  req(analysisDone(), !is.null(raw_period_key()))
  
  pk     <- raw_period_key()
  config <- isolate(applied_config()) %||% default_config(pk)
  
  # Header row
  header <- fluidRow(
    column(2, strong("Original Period")),
    column(3, strong("APSIM Phase Name")),
    column(4, strong("Custom Label")),
    column(3, strong("Merge Group"))
  )
  
  rows <- lapply(seq_len(nrow(pk)), function(i) {
    p_val  <- as.character(pk$Period[i])
    cfg_i  <- config[as.character(config$Period) == p_val, ]
    
    cur_name  <- if (nrow(cfg_i) == 1) cfg_i$CustomName  else pk$Label[i]
    cur_group <- if (nrow(cfg_i) == 1) cfg_i$MergeGroup  else p_val
    
    fluidRow(
      style = if (i %% 2 == 0) "background-color: #f9f9f9; padding: 4px 0;"
      else              "background-color: #ffffff; padding: 4px 0;",
      column(2, tags$div(style = "padding-top: 8px;", strong(paste("Period", p_val)))),
      column(3, tags$div(style = "padding-top: 8px;", pk$Label[i])),
      column(4,
             textInput(
               inputId = paste0("pcfg_name_",  p_val),
               label   = NULL,
               value   = cur_name,
               width   = "100%"
             )
      ),
      column(3,
             numericInput(
               inputId = paste0("pcfg_group_", p_val),
               label   = NULL,
               value   = as.numeric(cur_group),
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

## !!! function to get seasonal_data and final_x outputs from daily_sim_outputs ---------

build_period_outputs <- function(daily_sim_outputs, new_config, trial_info) {
  
  SUM_VARS <- c("AccRain", "ThermalTime", "AccTT", "AccEmTT", "Duration")
  
  sd_base <- daily_sim_outputs %>%
    group_by(Period, ID) %>%
    select(-any_of(c("Stage"))) %>%
    summarize(
      across(where(is.numeric) & !any_of(c("DOY", "AccEmTT", "Duration", "Period_Start_DOY", "Period_End_DOY")),
             ~ mean(.x, na.omit = TRUE)),
      AccRain           = sum(Rain),
      AccTT             = sum(ThermalTime),
      AccEmTT           = max(AccEmTT),
      Period_Start_Date = min(Date),
      Period_End_Date   = max(Date),
      .groups = "drop"
    ) %>%
    mutate(Period = as.character(Period))
  
  period_to_group <- select(new_config, Period, MergeGroup)
  
  sd_out <- sd_base %>%
    left_join(period_to_group, by = "Period") %>%
    group_by(ID, MergeGroup) %>%
    summarise(
      across(where(is.numeric) & any_of(SUM_VARS),
             ~ sum(.x, na.rm = TRUE)),
      across(where(is.numeric) & !any_of(SUM_VARS) &
               !any_of(c("Duration", "Period_Start_DOY", "Period_End_DOY")),
             ~ mean(.x, na.rm = TRUE)),
      Period_Start_Date = min(Period_Start_Date, na.rm = TRUE),
      Period_End_Date   = max(Period_End_Date,   na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      Duration         = as.numeric(as.period(Period_End_Date - Period_Start_Date, "days")) / 86400 + 1,
      Period_Start_DOY = yday(Period_Start_Date),
      Period_End_DOY   = yday(Period_End_Date)
    ) %>%
    rename(Period = MergeGroup) %>%
    relocate(ID, Period) %>%
    arrange(as.numeric(Period))
  
  fx_out <- pivot_wider(
    select(sd_out, -Period_Start_Date, -Period_End_Date) %>% rename(PivotPeriod = Period),
    names_from  = PivotPeriod,
    values_from = setdiff(names(select(sd_out, -Period_Start_Date, -Period_End_Date)), c("ID", "Period"))
  ) %>%
    right_join(trial_info, ., by = "ID")
  
  pk_out <- new_config %>%
    group_by(MergeGroup) %>%
    summarise(
      Label                   = first(CustomName),
      `APSIM Phases Included` = paste(APSIMName, collapse = " → "),
      `Original Periods`      = paste(Period,    collapse = ", "),
      .groups = "drop"
    ) %>%
    rename(Period = MergeGroup) %>%
    select(Period, Label, `APSIM Phases Included`, `Original Periods`) %>%
    arrange(as.numeric(Period))
  
  list(seasonal_data = sd_out, final_x = fx_out, period_key = pk_out)
}

## apply period config button -------
observeEvent(input$apply_period_config, {
  req(!is.null(raw_period_key()))
  pk <- raw_period_key()
  
  new_config <- tibble(
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
  applied_config(new_config)
  
  out <- build_period_outputs(daily_sim_outputs, new_config, trial_info)
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

## reset button -----
observeEvent(input$reset_period_config, {
  req(!is.null(raw_period_key()))
  pk  <- raw_period_key()
  def <- default_config(pk)
  applied_config(def)
  
  for (p in as.character(pk$Period)) {
    cfg_i <- def[def$Period == p, ]
    updateTextInput(session,    paste0("pcfg_name_",  p), value = cfg_i$CustomName)
    updateNumericInput(session, paste0("pcfg_group_", p), value = as.numeric(cfg_i$MergeGroup))
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


## live preview table in the Configure Periods tab ----------------------------
output$period_config_preview <- renderDT({
  req(analysisDone(), !is.null(raw_period_key()))
  
  pk <- raw_period_key()
  
  config <- lapply(as.character(pk$Period), function(p) {
    name_val  <- input[[paste0("pcfg_name_",  p)]]
    group_val <- input[[paste0("pcfg_group_", p)]]
    tibble(
      Period     = p,
      APSIMName  = pk$Label[as.character(pk$Period) == p],
      CustomName = if (is.null(name_val) || trimws(name_val) == "")
        pk$Label[as.character(pk$Period) == p] else trimws(name_val),
      MergeGroup = if (is.null(group_val) || is.na(group_val))
        p else as.character(group_val)
    )
  }) %>% bind_rows()
  
  preview_key <- config %>%
    group_by(MergeGroup) %>%
    summarise(
      Label                   = first(CustomName),
      `APSIM Phases Included` = paste(APSIMName, collapse = " → "),
      `Original Periods`      = paste(Period,    collapse = ", "),
      .groups = "drop"
    ) %>%
    rename(Period = MergeGroup) %>%
    select(Period, Label, `APSIM Phases Included`, `Original Periods`) %>%
    arrange(as.numeric(Period))
  
  datatable(
    preview_key,
    rownames = FALSE,
    class    = "compact stripe",
    options  = list(paging = FALSE, searching = FALSE, scrollX = TRUE)
  )
})