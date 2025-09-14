# app.R
# Shiny Invoice Builder with SQLite archive + PDF generator (R Markdown)
 
# ---- Packages ----
library(shiny)
library(DT)
library(DBI)
library(RSQLite)
library(dplyr)
library(glue)
library(lubridate)
library(fs)
library(rmarkdown)

# ---- Paths & Setup ----
db_path <- "invoices.sqlite"
pdf_dir <- "invoices"
if (!fs::dir_exists(pdf_dir)) fs::dir_create(pdf_dir)

# Serve generated PDFs under a relative path that works behind proxies (Workbench)
shiny::addResourcePath("invoices", normalizePath(pdf_dir, mustWork = TRUE))

# Path to template
template_path <- "invoice_template.Rmd"

# ---- DB Init ----
con <- dbConnect(RSQLite::SQLite(), db_path)

dbExecute(con, "
CREATE TABLE IF NOT EXISTS invoices (
  id INTEGER PRIMARY KEY,
  invoice_number TEXT,
  consultant_name TEXT,
  consultant_address TEXT,
  client_name TEXT,
  client_org TEXT,
  client_address TEXT,
  project_title TEXT,
  period_start TEXT,
  period_end TEXT,
  hourly_rate REAL,
  total_hours REAL,
  total_amount REAL,
  created_at TEXT,
  pdf_path TEXT
);
")

dbExecute(con, "
CREATE TABLE IF NOT EXISTS invoice_items (
  id INTEGER PRIMARY KEY,
  invoice_id INTEGER,
  task_date TEXT,
  description TEXT,
  hours REAL,
  rate REAL,
  amount REAL,
  FOREIGN KEY(invoice_id) REFERENCES invoices(id)
);
")

dbExecute(con, "
CREATE TABLE IF NOT EXISTS draft_items (
  id INTEGER PRIMARY KEY,
  task_date TEXT,
  description TEXT,
  hours REAL,
  rate REAL,
  amount REAL
);
")

onStop(function() {
  dbDisconnect(con)
})

# --- Helpers to prevent "invalid JSON response" in DT ---
to_chr <- function(x) {
  # collapse non-atomic/list columns; format numerics safely
  if (is.list(x)) x <- vapply(x, function(e) paste(capture.output(str(e)), collapse=" "), character(1))
  if (is.numeric(x)) return(formatC(x, format = "g", digits = 12))
  if (inherits(x, "POSIXt")) return(format(x, "%Y-%m-%d %H:%M:%S", tz = "UTC"))
  if (inherits(x, "Date")) return(format(x, "%Y-%m-%d"))
  as.character(x)
}

sanitize_df <- function(df) {
  if (is.null(df) || !nrow(df)) return(data.frame(Message = "No data.", stringsAsFactors = FALSE))
  out <- lapply(df, to_chr)
  out <- as.data.frame(out, stringsAsFactors = FALSE, check.names = FALSE)
  # ensure valid UTF-8 (DT/jsonlite can choke on native encodings)
  out[] <- lapply(out, function(col) enc2utf8(replace(col, is.na(col), "")))
  out
}

safe_dt <- function(expr, page_len = 10, escape_html = TRUE) {
  tryCatch({
    df <- expr
    df <- sanitize_df(df)
    DT::datatable(
      df,
      rownames = FALSE,
      escape   = escape_html,  # set FALSE only if you deliberately create HTML
      options  = list(pageLength = page_len)
    )
  }, error = function(e) {
    DT::datatable(
      data.frame(Error = enc2utf8(paste("Render error:", conditionMessage(e))), stringsAsFactors = FALSE),
      rownames = FALSE
    )
  })
}

# ---- Helpers ----

read_draft <- function() {
  dbReadTable(con, "draft_items") %>% arrange(task_date, id)
}

delete_draft_rows <- function(ids) {
  if (length(ids)) {
    sql <- glue("DELETE FROM draft_items WHERE id IN ({paste(ids, collapse=',')})")
    dbExecute(con, sql)
  }
}

clear_draft <- function() {
  dbExecute(con, "DELETE FROM draft_items")
}

generate_invoice_number <- function() {
  # e.g., INV-YYYYMMDD-### (sequence for day)
  today <- format(Sys.Date(), "%Y%m%d")
  existing <- dbGetQuery(con, "SELECT invoice_number FROM invoices WHERE invoice_number LIKE ?", params = paste0("INV-", today, "-%"))
  seq_no <- nrow(existing) + 1
  glue("INV-{today}-{formatC(seq_no, width=3, flag='0')}")
}

# ---- UI ----
ui <- fluidPage(
  tags$head(tags$title("Invoice Builder")),
  titlePanel("Invoice Builder (SQLite Archive + PDF)"),
  
  sidebarLayout(
    sidebarPanel(width = 4,
                 h4("Consultant Info"),
                 textInput("consultant_name", "Name", value = "Nathaniel Wilson"),
                 textAreaInput("consultant_address", "Address", value = "Lexington, KY", rows = 2),
                 
                 h4("Client Info"),
                 textInput("client_name", "Client Contact", value = "DON NJELESANI"),
                 textInput("client_org", "Organization", value = "AJWS"),
                 textAreaInput("client_address", "Address", value = "New York, USA", rows = 2),
                 
                 h4("Project & Period"),
                 textInput("project_title", "Project Title", placeholder = "Evaluation project, etc..."),
                 dateInput("period_start", "Period Start", value = Sys.Date() - 30),
                 dateInput("period_end", "Period End", value = Sys.Date()),
                 
                 h4("Default Rate"),
                 numericInput("hourly_rate", "Hourly rate (USD)", value = 100, min = 0, step = 1),
                 
                 tags$hr(),
                 h4("Add Line Item to Draft"),
                 dateInput("task_date", "Task Date", value = Sys.Date()),
                 textAreaInput("description", "Description", placeholder = "What did you do?", rows = 3),
                 numericInput("hours", "Hours", value = 1, min = 0, step = 0.25),
                 numericInput("rate_override", "Rate (optional; blank = use default)", value = NA, min = 0, step = 1),
                 
                 actionButton("add_item", "Add Item", class = "btn-primary"),
                 br(), br(),
                 actionButton("clear_draft", "Clear Draft (start over)", class = "btn-warning")
    ),
    
    mainPanel(width = 8,
              tabsetPanel(id = "tabs",
                          tabPanel("Draft",
                                   h4("Current Draft Items"),
                                   DTOutput("draft_table"),
                                   br(),
                                   fluidRow(
                                     column(6, actionButton("delete_selected", "Delete Selected Rows")),
                                     column(6, align = "right",
                                            strong("Total Hours: "), textOutput("draft_hours", inline = TRUE), " | ",
                                            strong("Amount: "), textOutput("draft_amount", inline = TRUE))
                                   ),
                                   br(),
                                   textInput("invoice_number", "Invoice Number (autofilled; you can edit)", value = ""),
                                   dateInput("invoice_date", "Invoice Date", value = Sys.Date()),
                                   actionButton("generate_pdf", "Generate Invoice PDF", class = "btn-success"),
                                   br(), br(),
                                   verbatimTextOutput("status")
                          ),
                          tabPanel("Archive",
                                   h4("Previously Generated Invoices"),
                                   DTOutput("archive_table")
                          )
              )
    )
  )
)

# ---- Server ----
server <- function(input, output, session) {
  
  # Keep a reactive copy of the current draft so the table updates instantly
  draft <- reactiveVal({
    read_draft() %>% mutate(task_date = as.Date(task_date))
  })
  
  # Autofill invoice number
  observe({
    updateTextInput(session, "invoice_number", value = generate_invoice_number())
  })
  
  # Add line item
  observeEvent(input$add_item, {
    req(input$task_date, input$description, input$hours)
    rate   <- ifelse(is.na(input$rate_override), input$hourly_rate, input$rate_override)
    amount <- round(input$hours * rate, 2)
    
    dbExecute(con,
              "INSERT INTO draft_items (task_date, description, hours, rate, amount)
             VALUES (?, ?, ?, ?, ?)",
              params = list(as.character(input$task_date),
                            input$description,
                            input$hours,
                            rate,
                            amount))
    
    # Refresh the reactive draft so the table updates immediately
    draft(read_draft() %>% mutate(task_date = as.Date(task_date)))
  })
  
  # Clear draft
  observeEvent(input$clear_draft, {
    showModal(modalDialog(
      title = "Clear Draft?",
      "This will remove all current draft items.",
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_clear", "Clear", class = "btn-danger")
      )
    ))
  })
  
  observeEvent(input$confirm_clear, {
    removeModal()
    clear_draft()
    draft(read_draft() %>% mutate(task_date = as.Date(task_date)))
  })
  
  # Draft table
  output$draft_table <- DT::renderDT({
    df <- draft()
    if (is.null(df) || !nrow(df)) {
      return(DT::datatable(data.frame(Message = "No draft items yet."), rownames = FALSE))
    }
    # display-friendly columns
    df_display <- data.frame(
      ID          = df$id,
      Date        = as.character(df$task_date),
      Description = as.character(df$description),
      Hours       = df$hours,
      Rate        = df$rate,
      Amount      = df$amount,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
    DT::datatable(
      df_display,
      rownames = FALSE,
      selection = "multiple",
      options   = list(pageLength = 8)
    )
  }, server = FALSE)
  
  # Delete selected rows
  observeEvent(input$delete_selected, {
    tbl <- draft()
    if (nrow(tbl) == 0) return()
    sel <- input$draft_table_rows_selected
    if (length(sel)) {
      ids <- tbl$id[sel]
      delete_draft_rows(ids)
      draft(read_draft() %>% mutate(task_date = as.Date(task_date)))
    }
  })
  
  # Totals for draft
  draft_totals <- reactive({
    df <- draft()
    tibble(
      hours  = sum(df$hours,  na.rm = TRUE),
      amount = sum(df$amount, na.rm = TRUE)
    )
  })
  
  output$draft_hours  <- renderText({ format(round(draft_totals()$hours, 2), nsmall = 2) })
  output$draft_amount <- renderText({ paste0("$", format(round(draft_totals()$amount, 2), nsmall = 2)) })
  
  # Generate PDF
  observeEvent(input$generate_pdf, {
    df <- read_draft()
    if (nrow(df) == 0) {
      output$status <- renderText("No draft items to include. Add items first.")
      return()
    }
    
    inv_no <- if (nzchar(input$invoice_number)) input$invoice_number else generate_invoice_number()
    inv_date <- as.character(input$invoice_date)
    
    total_hours <- sum(df$hours)
    total_amount <- sum(df$amount)
    
    # Insert invoice header
    dbExecute(con, "
      INSERT INTO invoices (
        invoice_number, consultant_name, consultant_address,
        client_name, client_org, client_address,
        project_title, period_start, period_end,
        hourly_rate, total_hours, total_amount,
        created_at, pdf_path
      ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
    ",
              params = list(
                inv_no,
                input$consultant_name, input$consultant_address,
                input$client_name, input$client_org, input$client_address,
                input$project_title, as.character(input$period_start), as.character(input$period_end),
                input$hourly_rate, total_hours, total_amount,
                as.character(Sys.time()), "" 
              ))
    
    invoice_id <- dbGetQuery(con, "SELECT last_insert_rowid() AS id")$id[1]
    
    # Copy items into invoice_items
    df_items <- read_draft() %>%
      mutate(invoice_id = invoice_id)
    
    dbWriteTable(con, "invoice_items",
                 df_items %>% select(invoice_id, task_date, description, hours, rate, amount),
                 append = TRUE)
    
    # Prepare params for Rmd
    items_for_rmd <- df_items %>%
      transmute(
        Date = as.Date(task_date),
        Description = description,
        Hours = hours,
        Rate = rate,
        Amount = amount
      ) %>%
      arrange(Date)
    
    pdf_file <- file.path(pdf_dir, glue("{inv_no}.pdf"))
    
    tryCatch({
      rmarkdown::render(
        input = template_path,
        output_file = basename(pdf_file),
        output_dir = pdf_dir,
        params = list(
          invoice_number   = inv_no,
          consultant_name  = input$consultant_name,
          consultant_address = input$consultant_address,
          client_name      = input$client_name,
          client_org       = input$client_org,
          client_address   = input$client_address,
          project_title    = input$project_title,
          period_start     = as.character(input$period_start),
          period_end       = as.character(input$period_end),
          items            = items_for_rmd,
          total_hours      = total_hours,
          total_amount     = total_amount,
          invoice_date     = inv_date
        ),
        quiet = TRUE
      )
      
      dbExecute(con, "UPDATE invoices SET pdf_path = ? WHERE id = ?", params = list(pdf_file, invoice_id))
      clear_draft()
      draft(read_draft() %>% mutate(task_date = as.Date(task_date)))
      proxy <- dataTableProxy("draft_table")
      replaceData(proxy, read_draft(), resetPaging = TRUE)
      
      
      
      output$status <- renderText(glue("Invoice generated: {pdf_file}"))
    }, error = function(e) {
      output$status <- renderText(paste("Failed to render PDF:", e$message,
                                        "\nTip: Install LaTeX via tinytex::install_tinytex() if not already installed."))
    })
  })
  
  # Archive table
  output$archive_table <- DT::renderDT({
    if (!DBI::dbExistsTable(con, "invoices")) {
      return(DT::datatable(data.frame(Message = "No invoices yet."), rownames = FALSE))
    }
    invs_raw <- tryCatch(DBI::dbReadTable(con, "invoices"), error = function(e) NULL)
    if (is.null(invs_raw) || !nrow(invs_raw)) {
      return(DT::datatable(data.frame(Message = "No invoices yet."), rownames = FALSE))
    }
    
    created_chr <- suppressWarnings(format(as.POSIXct(as.character(invs_raw$created_at), tz = "UTC"),
                                           "%Y-%m-%d %H:%M:%S"))
    created_chr[is.na(created_chr)] <- ""
    
    total_num <- suppressWarnings(as.numeric(invs_raw$total_amount))
    total_chr <- ifelse(is.na(total_num), "", paste0("$", formatC(total_num, format = "f", digits = 2, big.mark = ",")))
    
    pdf_local  <- as.character(invs_raw$pdf_path)
    pdf_exists <- !is.na(pdf_local) & nzchar(pdf_local) & file.exists(pdf_local)
    
    # NOTE: no leading "/" — use relative path so it honors Workbench prefixes
    pdf_href <- ifelse(
      pdf_exists,
      paste0('<a href="invoices/', utils::URLencode(basename(pdf_local)),
             '" target="_blank" rel="noopener">Open PDF</a>'),
      ""
    )
    
    df <- data.frame(
      `Invoice #`   = as.character(invs_raw$invoice_number),
      Created       = created_chr,
      Client        = as.character(invs_raw$client_org),
      Project       = as.character(invs_raw$project_title),
      `Total Hours` = as.character(invs_raw$total_hours),
      Total         = total_chr,
      PDF           = pdf_href,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
    df[is.na(df)] <- ""
    
    pdf_col <- which(names(df) == "PDF")
    esc_idx <- if (length(pdf_col) == 1) setdiff(seq_len(ncol(df)), pdf_col) else seq_len(ncol(df))
    
    DT::datatable(df, rownames = FALSE, escape = esc_idx, options = list(pageLength = 10))
  }, server = FALSE)
}

shinyApp(ui, server)