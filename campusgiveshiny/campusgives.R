# Load libraries
library(shiny)
library(RSQLite)
library(DBI)
library(DT)
library(leaflet)
library(ggplot2)
library(shinyWidgets)
library(shinyjs)
library(blastula)
library(shinyalert)
library(rsconnect)
library(stringr)

# Database Connection (SQLite)
con <- dbConnect(RSQLite::SQLite(), dbname = "campusgives.db")

# Fetch categories dynamically
categories <- tryCatch({
  dbGetQuery(con, "
    SELECT DISTINCT i.category
    FROM items i
    JOIN donor d ON i.item_id = d.item_id
    WHERE d.status = 'active'
  ")$category
}, error = function(e) c())
categories <- c("All", categories)

# Admin Summary Counts
get_summary_counts <- function() {
  list(
    total_items = dbGetQuery(con, "SELECT COUNT(*) FROM items")[[1]],
    total_requests = dbGetQuery(con, "SELECT COUNT(*) FROM requests")[[1]],
    total_transactions = dbGetQuery(con, "SELECT COUNT(*) FROM transactions")[[1]],
    completed_transactions = dbGetQuery(con, "SELECT COUNT(*) FROM transactions WHERE status = 'Completed'")[[1]]
  )
}

# UI
ui <- fluidPage(
  useShinyjs(),
  useShinyalert(),
  setBackgroundColor("ghostwhite"),
  titlePanel(div(icon("gift", lib = "font-awesome"), "🎁 CampusGives Donation App")),
  
  sidebarLayout(
    sidebarPanel(
      h4("🔍 Filter Items"),
      selectInput("category", "Choose Category:", choices = categories),
      pickerInput("item_condition", "Item Condition:", choices = c("New", "Used", "All"), selected = "All", multiple = TRUE, options = list(`actions-box` = TRUE)),
      actionButton("refresh", "🔄 Refresh", class = "btn btn-primary"),
      hr(),
      h4("📆 Date Range"),
      dateRangeInput("date_range", "Filter Transactions:", start = Sys.Date() - 90, end = Sys.Date()),
      checkboxInput("contactless", "Contactless Pickup", TRUE),
      hr(),
      sliderInput("rows_display", "Rows to Show:", min = 5, max = 50, value = 10, step = 5)
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("📦 Available Items", DTOutput("items_table")),
        tabPanel("📝 Requests", DTOutput("requests_table")),
        tabPanel("📃 Transactions", DTOutput("transactions_table"), br(), downloadButton("download_receipt", "Download Receipt")),
        tabPanel("🗺️ Pickup Map", leafletOutput("pickup_map")),
        tabPanel("🚚 Pickup Scheduling",
                 selectInput("pickup_txn", "Select Transaction:", choices = NULL),
                 dateInput("pickup_date", "Pickup Date:"),
                 textInput("pickup_time", "Pickup Time (e.g., 2:00 PM):"),
                 textInput("email_address", "Email:"),
                 textInput("confirm_email", "Confirm Email:"),
                 actionButton("schedule_pickup", "Confirm Pickup", class = "btn btn-success"),
                 verbatimTextOutput("pickup_confirm")
        ),
        tabPanel("📊 Statistics",
                 selectInput("stat_choice", "Choose Statistic:", choices = c("Transactions Over Time", "Transactions by Status", "Requests per Day", "Available Items by Category")),
                 plotOutput("stats_plot")
        ),
        tabPanel("👨‍💼 Admin Dashboard",
                 fluidRow(
                   column(3, strong("Items:"), textOutput("count_items")),
                   column(3, strong("Requests:"), textOutput("count_requests")),
                   column(3, strong("Transactions:"), textOutput("count_transactions")),
                   column(3, strong("Completed:"), textOutput("count_completed"))
                 ),
                 hr(),
                 h4("🌟 Feedback Summary"),
                 fluidRow(
                   column(6, strong("Average Rating:"), textOutput("average_rating")),
                   column(6, strong("Total Reviews:"), textOutput("total_feedback"))
                 )
        ),
        tabPanel("🧹 Cleanup Tools",
                 textInput("cleanup_item_id", "Item ID:"),
                 passwordInput("admin_password", "Admin Password:"),
                 actionButton("delete_item", "Delete", class = "btn btn-danger"),
                 verbatimTextOutput("cleanup_status")
        ),
        tabPanel("🌟 Feedback",
                 selectInput("feedback_txn", "Completed Transaction:", choices = NULL),
                 sliderInput("rating", "Rate (1-5):", min = 1, max = 5, value = 5),
                 textAreaInput("comment", "Your Comment:"),
                 actionButton("submit_feedback", "Submit", class = "btn btn-info"),
                 verbatimTextOutput("feedback_status")
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  get_items <- reactive({
    input$refresh
    isolate({
      query <- "
        SELECT i.title, i.category, i.condition, i.pickup_location
        FROM items i
        JOIN donor d ON i.item_id = d.item_id
        WHERE d.status = 'active'
      "
      items <- dbGetQuery(con, query)
      if (input$category != "All") items <- items[items$category == input$category, ]
      if (!"All" %in% input$item_condition) items <- items[items$condition %in% input$item_condition, ]
      items
    })
  })
  
  summary_counts <- reactive(get_summary_counts())
  output$count_items <- renderText(format(summary_counts()$total_items, big.mark = ","))
  output$count_requests <- renderText(format(summary_counts()$total_requests, big.mark = ","))
  output$count_transactions <- renderText(format(summary_counts()$total_transactions, big.mark = ","))
  output$count_completed <- renderText(format(summary_counts()$completed_transactions, big.mark = ","))
  
  feedback_summary <- reactive({
    feedback_data <- tryCatch({
      dbGetQuery(con, "SELECT rating FROM feedback")
    }, error = function(e) data.frame(rating = numeric(0)))
    total_ratings <- nrow(feedback_data)
    avg_rating <- if (total_ratings > 0) round(mean(feedback_data$rating), 1) else NA
    list(
      average_rating_display = if (total_ratings > 0) paste0(avg_rating, " / 5") else "No ratings yet",
      review_count = if (total_ratings > 0) paste(total_ratings, "reviewers") else "0 reviewers"
    )
  })
  output$total_feedback <- renderText({ feedback_summary()$review_count })
  output$average_rating <- renderText({ feedback_summary()$average_rating_display })
  
  output$items_table <- renderDT({
    items <- get_items()
    placeholder <- "https://upload.wikimedia.org/wikipedia/commons/thumb/a/ac/No_image_available.svg/480px-No_image_available.svg.png"
    items$Photo <- paste0('<img src="www/', gsub(" ", "_", tolower(items$title)), '.jpg" height="60" onerror="this.onerror=null;this.src=\'', placeholder, '\'">')
    datatable(items[, c("Photo", "title", "category", "condition", "pickup_location")], escape = FALSE, options = list(pageLength = input$rows_display))
  })
  
  output$requests_table <- renderDT({
    query <- "SELECT request_id, user_id AS requester_id, item_id, status, request_date FROM requests"
    datatable(dbGetQuery(con, query), options = list(pageLength = input$rows_display))
  })
  
  transactions_data <- reactive({
    query <- "SELECT transaction_id, request_id, donor_id, status, date_time FROM transactions"
    df <- dbGetQuery(con, query)
    df$date_time <- as.character(df$date_time)
    df
  })
  
  output$transactions_table <- renderDT({
    datatable(transactions_data(), options = list(pageLength = input$rows_display))
  })
  
  selected_transaction <- reactive({
    req(input$transactions_table_rows_selected)
    transactions_data()[input$transactions_table_rows_selected, ]
  })
  
  output$download_receipt <- downloadHandler(
    filename = function() paste0("Donation_Receipt_", Sys.Date(), ".txt"),
    content = function(file) {
      txn <- selected_transaction()
      receipt <- c(
        "CAMPUSGIVES DONATION RECEIPT",
        "-----------------------------",
        paste("Transaction ID:", txn$transaction_id),
        paste("Request ID:", txn$request_id),
        paste("Donor ID:", txn$donor_id),
        paste("Status:", txn$status),
        paste("Date:", txn$date_time),
        "\nThank you for your generous donation!"
      )
      writeLines(receipt, file)
    }
  )
  
  output$pickup_map <- renderLeaflet({
    items <- get_items()
    if (nrow(items) == 0) return(leaflet() %>% addTiles())
    locations <- data.frame(
      lat = runif(nrow(items), 32.98, 32.99),
      lng = runif(nrow(items), -96.75, -96.74),
      label = items$title,
      link = paste0("https://www.google.com/maps?q=", runif(nrow(items), 32.98, 32.99), ",", runif(nrow(items), -96.75, -96.74))
    )
    leaflet(locations) %>% addTiles() %>% addMarkers(~lng, ~lat, popup = ~paste0("<b>", label, "</b><br><a href='", link, "' target='_blank'>Get Directions</a>"))
  })
  
  output$stats_plot <- renderPlot({
    choice <- input$stat_choice
    if (choice == "Transactions Over Time") {
      data <- dbGetQuery(con, "SELECT date_time FROM transactions")
      data$date_time <- as.Date(data$date_time)
      ggplot(data, aes(date_time)) + geom_histogram(binwidth = 1, fill = "#0072B2", color = "white") + labs(title = "Transactions Over Time", x = "Date", y = "Count") + theme_minimal()
    } else if (choice == "Transactions by Status") {
      data <- dbGetQuery(con, "SELECT status FROM transactions")
      ggplot(data, aes(status)) + geom_bar(fill = "#E69F00") + labs(title = "By Status", x = "Status", y = "Count") + theme_minimal()
    } else if (choice == "Requests per Day") {
      data <- dbGetQuery(con, "SELECT request_date FROM requests")
      data$request_date <- as.Date(data$request_date)
      ggplot(data, aes(request_date)) + geom_bar(fill = "#009E73") + labs(title = "Requests Per Day", x = "Date", y = "Count") + theme_minimal()
    } else {
      data <- get_items()
      data$category <- str_wrap(data$category, width = 15)
      ggplot(data, aes(x = category)) + geom_bar(fill = "#D55E00") + labs(title = "Available Items", x = "Category", y = "Count") + theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
  })
  
  observe({
    txns <- dbGetQuery(con, "SELECT transaction_id FROM transactions WHERE status = 'Pending'")
    updateSelectInput(session, "pickup_txn", choices = txns$transaction_id)
  })
  
  observeEvent(input$schedule_pickup, {
    if (is.null(input$pickup_txn) || input$pickup_txn == "") {
      shinyalert("Oops!", "Select a transaction!", type = "warning")
      return()
    }
    if (input$email_address != input$confirm_email) {
      shinyalert("Mismatch", "Emails do not match!", type = "error")
      return()
    }
    output$pickup_confirm <- renderText(paste("\u2705 Pickup confirmed for Transaction", input$pickup_txn, "on", input$pickup_date, "at", input$pickup_time))
    if (nzchar(input$email_address)) {
      email <- compose_email(body = md(paste("### CampusGives Pickup Confirmation", paste("Transaction:", input$pickup_txn), paste("Date:", input$pickup_date), paste("Time:", input$pickup_time), "Thanks for supporting CampusGives!", sep = "\n\n")))
      smtp_send(email, from = "campusgives@yourdomain.com", to = input$email_address, subject = "Pickup Confirmation", credentials = creds_anonymous())
    }
  })
  
  observeEvent(input$delete_item, {
    req(input$cleanup_item_id, input$admin_password)
    if (input$admin_password != "admin123") {
      shinyalert("Access Denied", "Wrong password!", type = "error")
      return()
    }
    shinyalert("Confirm Delete", paste("Delete Item ID:", input$cleanup_item_id), type = "warning", showCancelButton = TRUE, callbackR = function(x) {
      if (x) {
        tryCatch({
          dbExecute(con, paste0("DELETE FROM items WHERE item_id = '", input$cleanup_item_id, "'"))
          output$cleanup_status <- renderText("\u2705 Item deleted.")
        }, error = function(e) {
          output$cleanup_status <- renderText("\u274C Error deleting item.")
        })
      }
    })
  })
  
  observe({
    txns <- dbGetQuery(con, "SELECT transaction_id FROM transactions WHERE status = 'Completed'")
    updateSelectInput(session, "feedback_txn", choices = txns$transaction_id)
  })
  
  observeEvent(input$submit_feedback, {
    req(input$feedback_txn, input$rating)
    dbExecute(con, "CREATE TABLE IF NOT EXISTS feedback (transaction_id TEXT, rating INTEGER, comment TEXT)")
    dbExecute(con, "INSERT INTO feedback (transaction_id, rating, comment) VALUES (?, ?, ?)", params = list(input$feedback_txn, input$rating, input$comment))
    shinyalert("\ud83c\udf89 Thank you!", "Your feedback was submitted.", type = "success")
  })
}

# Launch App
shinyApp(ui, server)