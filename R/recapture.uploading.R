#' @title upload_recaptures
#' @import dplyr ROracle DBI shiny svDialogs
#' @description allows individual or batch uploading of recapture data
#' @export

upload_recaptures <- function(){

library(shinyjs)


  library(shiny)
  library(ROracle)
  library(shinyjs)

  # Define UI
  ui <- fluidPage(

    # Use shinyjs to include required JavaScript
    shinyjs::useShinyjs(),

    # Title
    titlePanel("Data Entry Form"),

    # Sidebar layout
    sidebarLayout(

      # Sidebar panel for inputs
      sidebarPanel(

        # Input for Tag Prefix
        textInput("tag_prefix", "Tag Prefix:", ""),

        # Input for Tag Number
        numericInput("tag_number", "Tag Number:", value = NULL, min = 1, step = 1),

        # Input for Date
        dateInput("date", "Date:", ""),

        # Submit button
        actionButton("submit", "Submit")
      ),

      # Main panel for displaying output
      mainPanel(
        # Placeholder for status
        textOutput("status"),

        # Display submitted data
        tableOutput("submitted_data")
      )
    )
  )

  # Define server logic
  server <- function(input, output, session) {

    # Initialize empty list to store submitted data
    submitted_data <- reactiveVal(data.frame(Tag_Prefix = character(), Tag_Number = numeric(), Date = character()))

    # Connect to Oracle database
    con <- dbConnect(drv = dbDriver("Oracle"),
                     username = oracle.personal.user,
                     password = oracle.personal.password,
                     dbname = oracle.personal.server)

    # Update Oracle table when Submit button is clicked
    observeEvent(input$submit, {

      # Get input values
      tag_prefix <- input$tag_prefix
      tag_number <- input$tag_number
      date <- format(input$date, "%d/%m/%Y")

      # Prepare SQL insert statement
      sql <- paste("INSERT INTO ELEMENTG.TEST (Tag_Prefix, Tag_Number, REC_DATE) VALUES ('", tag_prefix, "', ", tag_number, ", '", date, "')", sep="")

      # Execute SQL insert statement
      dbExecute(con, sql)

      # Update status
      output$status <- renderText({
        "Data uploaded to Oracle table ELEMENTG.TEST"
      })

      # Add submitted data to the table
      current_data <- submitted_data()
      new_row <- data.frame(Tag_Prefix = tag_prefix, Tag_Number = tag_number, Date = date)
      updated_data <- rbind(current_data, new_row)
      submitted_data(updated_data)

      # Clear input fields
      shinyjs::reset("tag_prefix")
      shinyjs::reset("tag_number")
      shinyjs::reset("date")
    })

    # Render submitted data
    output$submitted_data <- renderTable({
      submitted_data()
    })

    # Close Oracle connection on app exit
    session$onSessionEnded(function() {
      dbDisconnect(con)
    })
  }

  # Run the application
  shinyApp(ui = ui, server = server)



}
