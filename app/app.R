# =========================
# WP2 REA Tool 
# =========================

library(shiny)
library(shinyjs)
library(sortable)
library(readr)
library(dplyr)
library(lubridate)
library(tibble)
library(stringr)
library(httr)
library(jsonlite)

# ---- CONFIG ----
GITHUB_USER <- "Mabo1399"
REPO_NAME   <- "m4nature_WP2"
BRANCH      <- "main"

# Set the data source type and raw file URL in your repo:
file_type   <- "scientific"  # "scientific" (TSV WoS) or "overton" (CSV)
DATA_URL    <- sprintf("https://raw.githubusercontent.com/%s/%s/%s/data/wos_results.txt",
                       GITHUB_USER, REPO_NAME, BRANCH)

# Paste your Apps Script Web App URL (ends with /exec) after you deploy it (Phase 2):
GS_ENDPOINT <- "REPLACE_WITH_YOUR_APPS_SCRIPT_EXEC_URL"

# ---- HELPERS ----
`%||%` <- function(x, y) if (is.null(x)) y else x
as_int <- function(x) suppressWarnings(as.integer(x))

get_col <- function(df, col, default = NA_character_) {
  if (col %in% names(df)) {
    v <- df[[col]]
    if (is.list(v)) v <- sapply(v, function(x) paste(x, collapse = "; "))
    if (is.factor(v)) v <- as.character(v)
    v
  } else {
    rep(default, nrow(df))
  }
}

safe_year <- function(x) {
  suppressWarnings({
    d <- parse_date_time(x, orders = c("ymd", "dmy", "mdy", "Ymd", "BdY", "dbY"))
  })
  y <- year(d)
  y[is.na(y)] <- suppressWarnings(as.integer(x[is.na(y)]))
  y
}

# ---- LOAD DATA (from GitHub raw URL) ----
load_source_data <- function(file_type, url) {
  txt <- GET(url) |> content(as = "text", encoding = "UTF-8")
  if (file_type == "overton") {
    read_csv(txt, show_col_types = FALSE)
  } else {
    read_tsv(txt, show_col_types = FALSE, quote = "")
  }
}

# ---- ADAPTERS ----
adapt_overton <- function(df) {
  tibble(
    source    = "Overton",
    raw_id    = get_col(df, "Overton id"),
    authors   = get_col(df, "Policy authors"),
    year      = safe_year(get_col(df, "Published_on")),
    title     = get_col(df, "Title"),
    journal   = NA_character_,
    publisher = get_col(df, "Source title"),
    volume    = NA_character_,
    issue     = NA_character_,
    pages     = NA_character_,
    doi       = NA_character_,
    url       = get_col(df, "Document URL"),
    abstract  = NA_character_
  )
}

adapt_scientific <- function(df) {
  BP <- get_col(df, "BP")
  EP <- get_col(df, "EP")
  pages <- ifelse(!is.na(BP) & !is.na(EP), paste0(BP, "-", EP), NA_character_)
  VL <- get_col(df, "VL")
  IS <- get_col(df, "IS")
  PY <- get_col(df, "PY")
  DI <- get_col(df, "DI")
  DL <- get_col(df, "DL")
  tibble(
    source    = "Web of Science",
    raw_id    = get_col(df, "UT"),
    authors   = get_col(df, "AU"),
    year      = suppressWarnings(as.integer(PY)),
    title     = get_col(df, "TI"),
    journal   = get_col(df, "SO"),
    publisher = NA_character_,
    volume    = as.character(VL),
    issue     = IS,
    pages     = pages,
    doi       = DI,
    url       = DL,
    abstract  = get_col(df, "AB")
  )
}

# ---- RESULTS SCHEMA ----
expected_cols <- c(
  "title","authors","year","included","exclusion_reason",
  "literature_type","scale_governance",
  "decision_type","policy_stage","participation_level","methods_used",
  "challenges","other_benefits","method_notes",
  paste0("method_group_", 1:7)
)

# ---- GOOGLE SHEET API HELPERS ----
sheet_get <- function() {
  tryCatch({
    resp <- httr::GET(GS_ENDPOINT)
    stop_for_status(resp)
    txt <- content(resp, as = "text", encoding = "UTF-8")
    dat <- jsonlite::fromJSON(txt, flatten = TRUE)
    if (!is.data.frame(dat)) dat <- tibble()
    for (mc in setdiff(expected_cols, names(dat))) dat[[mc]] <- NA_character_
    dat %>% dplyr::select(dplyr::all_of(expected_cols)) %>% dplyr::mutate(dplyr::across(dplyr::everything(), as.character))
  }, error = function(e) {
    message("Failed to GET sheet: ", e$message)
    tibble::as_tibble(setNames(rep(list(character()), length(expected_cols)), expected_cols))
  })
}

sheet_upsert <- function(row_list) {
  tryCatch({
    resp <- httr::POST(
      GS_ENDPOINT,
      body = jsonlite::toJSON(row_list, auto_unbox = TRUE),
      encode = "raw",
      httr::add_headers("Content-Type" = "application/json")
    )
    stop_for_status(resp)
    TRUE
  }, error = function(e) {
    showNotification(paste("Failed to save to Google Sheet:", e$message), type = "error")
    FALSE
  })
}

# =========================
# UI
# =========================
ui <- fluidPage(
  useShinyjs(),
  titlePanel("WP2 REA Tool"),

  tags$style(HTML("
    .two-col .shiny-options-group { column-count: 2; -moz-column-count: 2; -webkit-column-count: 2; }
    .two-col .checkbox, .two-col .radio { margin-top: 4px; margin-bottom: 4px; }
  ")),

  sidebarLayout(
    sidebarPanel(
      selectInput("record", "Select article", choices = NULL),
      width = 3
    ),

    mainPanel(
      tabsetPanel(
        id = "main_tabs",

        tabPanel(
          "1. Eligibility & metadata", value = "tab1",
          h4("Citation Info"),
          textOutput("title_text"),
          textOutput("authors_text"),
          textOutput("year_text"),
          hr(),
          radioButtons("included", "Meets inclusion criteria?", choices = c("Yes", "No"), selected = character(0)),
          hr(),
          radioButtons("literature_type", "Literature type", choices = c("Grey literature", "Scientific Literature", "Eklipse/BioAgora"), selected = character(0)),
          conditionalPanel(
            condition = "input.included == 'No'",
            textAreaInput("exclusion_reason", "Reason for exclusion", rows = 3),
            actionButton("save_entry_tab1", "Save Entry (Not Included)")
          )
        ),

        tabPanel(
          "2. Investigation attributes", value = "tab2",
          div(class = "two-col",
              checkboxGroupInput("decision_type", "Type of decision / knowledge need (Pullin et al., 2016) [multi-select]",
                choices = c(
                  "Seeking better understanding of an issue (including predictions and forecasting)",
                  "Identifying appropriate ways and means of realising certain decisions",
                  "Improving understanding of possibilities and boundaries for decision-making"
                )
              )
          ),
          div(class = "two-col",
              checkboxGroupInput("policy_stage", "Stage of policy cycle [multi-select]",
                choices = c("Agenda-Setting","Policy Formulation","Policy Adoption","Policy Implementation","Policy Evaluation")
              )
          ),
          div(class = "two-col",
              checkboxGroupInput("participation_level", "Level of participation [multi-select]",
                choices = c("Inform", "Involve", "Consult", "Collaborate", "Empower")
              )
          ),
          div(class = "two-col",
              checkboxGroupInput("scale_governance", "Scale or governance level [multi-select]",
                choices = c("Community/Local","Municipal/City","Regional/Subnational","National","Multinational (e.g., EU)","Global")
              )
          )
        ),

        tabPanel(
          "3. Methods application", value = "tab3",
          div(class = "two-col",
              checkboxGroupInput("methods_used", "Methods used [multi-select]",
                choices = c(
                  "Bayesian Belief Networks","Bow Tie Analysis","Causal Criteria Analysis","Collaborative Adaptive Management",
                  "Discourse Analysis","Expert Consultation","Focus Group(s)","Fuzzy Cognitive Mapping","Joint Fact-finding",
                  "Meta-analysis","Multi-criteria Decision Analysis","Multiple Expert Consultation + Delphi Process",
                  "Participatory Mapping","Nominal Group Technique","Non-systematic Literature Review",
                  "Qualitative Comparative Analysis","Rapid Evidence Assessment","Scenario Analysis","Scoping Review",
                  "Solution Scanning","Structured Decision-making","Subject-wide Evidence Synthesis","Systematic Map",
                  "Systematic Review","Vote Counting","Additional Method used [elaborate]"
                )
              )
          ),
          hr(),
          h4("Organize methods into sequential groups (simultaneous within group)"),
          uiOutput("methods_bucket_ui"),
          hr(),
          div(class = "two-col",
              checkboxGroupInput("challenges", "Challenges [multi-select]",
                choices = c(
                  "Participation/logistical (time, recruitment)",
                  "Evidence/expertise (gaps in expertise)",
                  "Bias/subjectivity (analytical subjectivity, non-systematic evidence base)",
                  "Synthesis/integration (complexity of combining heterogeneous inputs)"
                )
              )
          ),
          textAreaInput("other_benefits", "Other benefits", rows = 3),
          textAreaInput("method_notes", "Method notes / additions", rows = 4),
          actionButton("save_entry", "Save Entry")
        )
      )
    )
  )
)

# =========================
# SERVER
# =========================
server <- function(input, output, session) {
  raw_data   <- load_source_data(file_type, DATA_URL)
  records_df <- if (file_type == "overton") adapt_overton(raw_data) else adapt_scientific(raw_data)

  results_df <- sheet_get()
  results_rv <- reactiveVal(results_df)

  safe_equal_str <- function(a, b) { a <- ifelse(is.na(a), "", str_trim(a)); b <- ifelse(is.na(b), "", str_trim(b)); a == b }
  safe_equal_int <- function(a, b) { ai <- as_int(a); bi <- as_int(b); (is.na(ai) & is.na(bi)) | (!is.na(ai) & !is.na(bi) & ai == bi) }

  completion_status <- function(row) {
    inc <- row$included
    if (is.na(inc) || inc == "") return("🟠 Not complete")
    if (inc == "No") {
      reason_ok <- !is.null(row$exclusion_reason) && nzchar(row$exclusion_reason)
      if (reason_ok) return("🟢 Done – not included")
      return("🟠 Not complete")
    }
    if (inc == "Yes") {
      req_ok <- all(
        nzchar(row$literature_type %||% ""),
        nzchar(row$scale_governance %||% ""),
        nzchar(row$decision_type %||% ""),
        nzchar(row$policy_stage %||% ""),
        nzchar(row$participation_level %||% ""),
        nzchar(row$methods_used %||% ""),
        nzchar(row$challenges %||% "")
      )
      if (req_ok) return("🟢 Done")
      return("🟠 Not complete")
    }
    "🟠 Not complete"
  }

  update_record_choices <- function() {
    res <- isolate(results_rv())
    choices <- sapply(seq_len(nrow(records_df)), function(i) {
      r <- records_df[i, ]
      if (nrow(res) > 0) {
        res_row <- res %>% dplyr::filter(safe_equal_str(title, r$title) & safe_equal_int(year, r$year))
        status <- if (nrow(res_row) == 0) "🔴 Not started" else completion_status(res_row[1, ])
      } else {
        status <- "🔴 Not started"
      }
      paste0(r$year, " – ", r$title, " [", status, "]")
    })
    updateSelectInput(session, "record", choices = setNames(seq_len(nrow(records_df)), choices))
  }
  update_record_choices()

  current <- reactive({ req(input$record); records_df[as.integer(input$record), ] })

  output$title_text   <- renderText({ paste("Title:",   current()$title) })
  output$authors_text <- renderText({ paste("Authors:", current()$authors) })
  output$year_text    <- renderText({ paste("Year:",    current()$year) })

  observe({ inc <- input$included; if (is.null(inc) || inc != "Yes") updateTabsetPanel(session, "main_tabs", selected = "tab1") })

  output$methods_bucket_ui <- renderUI({
    req(input$methods_used)
    n_methods <- length(input$methods_used)
    n_groups  <- min(n_methods, 7)
    rank_lists <- lapply(seq_len(n_groups), function(i) {
      add_rank_list(text = paste0("Group ", i), labels = NULL, input_id = paste0("group_", i))
    })
    do.call(bucket_list, c(
      list(
        header = "Drag methods into sequential groups (simultaneous within group)",
        group_name = "method_groups",
        orientation = "horizontal",
        add_rank_list(text = "Available methods", labels = input$methods_used, input_id = "methods_source")
      ),
      rank_lists
    ))
  })

  observe({
    if (!is.null(input$main_tabs) && input$main_tabs %in% c("tab2", "tab3")) {
      if (is.null(input$included) || input$included != "Yes") {
        showNotification("You must mark 'Meets inclusion criteria?' as Yes to proceed.", type = "error")
        updateTabsetPanel(session, "main_tabs", selected = "tab1")
      }
    }
  })

  observeEvent(input$save_entry_tab1, {
    req(input$included)
    if (input$included != "No") {
      showNotification("Use the Tab 3 Save Entry for included records.", type = "warning"); return(NULL)
    }
    if (is.null(input$exclusion_reason) || input$exclusion_reason == "") {
      showNotification("Please provide a reason for exclusion.", type = "error"); return(NULL)
    }

    row <- list(
      title = as.character(current()$title),
      authors = as.character(current()$authors),
      year = as.character(current()$year),
      included = "No",
      exclusion_reason = as.character(input$exclusion_reason),
      literature_type = as.character(input$literature_type %||% ""),
      scale_governance = NA_character_,
      decision_type = NA_character_,
      policy_stage = NA_character_,
      participation_level = NA_character_,
      methods_used = NA_character_,
      challenges = NA_character_,
      other_benefits = NA_character_,
      method_notes = NA_character_,
      method_group_1 = NA_character_, method_group_2 = NA_character_, method_group_3 = NA_character_,
      method_group_4 = NA_character_, method_group_5 = NA_character_, method_group_6 = NA_character_,
      method_group_7 = NA_character_
    )
    if (!sheet_upsert(row)) return(NULL)

    results_rv(sheet_get())
    showNotification("Excluded entry saved.", type = "message")
    updateTabsetPanel(session, "main_tabs", selected = "tab1")
    update_record_choices()
  })

  observeEvent(input$save_entry, {
    req(input$included)
    if (input$included != "Yes") {
      showNotification("Mark inclusion as 'Yes' to save full methods entry, or save as exclusion in Tab 1.", type = "warning"); return(NULL)
    }

    methods_wide <- sapply(1:7, function(i) {
      g <- input$method_groups[[paste0("group_", i)]]
      if (is.null(g) || length(g) == 0) return(NA_character_)
      paste(g, collapse = "; ")
    })

    row <- list(
      title = as.character(current()$title),
      authors = as.character(current()$authors),
      year = as.character(current()$year),
      included = "Yes",
      exclusion_reason = NA_character_,
      literature_type = as.character(input$literature_type %||% ""),
      scale_governance = paste(input$scale_governance %||% character(), collapse = "; "),
      decision_type = paste(input$decision_type %||% character(), collapse = "; "),
      policy_stage = paste(input$policy_stage %||% character(), collapse = "; "),
      participation_level = paste(input$participation_level %||% character(), collapse = "; "),
      methods_used = paste(input$methods_used %||% character(), collapse = "; "),
      challenges = paste(input$challenges %||% character(), collapse = "; "),
      other_benefits = as.character(input$other_benefits),
      method_notes = as.character(input$method_notes),
      method_group_1 = methods_wide[1], method_group_2 = methods_wide[2], method_group_3 = methods_wide[3],
      method_group_4 = methods_wide[4], method_group_5 = methods_wide[5], method_group_6 = methods_wide[6],
      method_group_7 = methods_wide[7]
    )
    if (!sheet_upsert(row)) return(NULL)

    results_rv(sheet_get())
    showNotification("Included entry saved.", type = "message")
    updateTabsetPanel(session, "main_tabs", selected = "tab1")
    update_record_choices()
  })
}

shinyApp(ui, server)
