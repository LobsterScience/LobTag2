#' @title upload_recaptures
#' @import dplyr ROracle DBI shiny svDialogs
#' @description allows individual or batch uploading of recapture data
#' @export

upload_recaptures <- function(){



  # Define UI for application
  ui <- fluidPage(

    # Use shinyjs to include required JavaScript
    shinyjs::useShinyjs(),

    # Title
    titlePanel("Upload Recaptured Tags"),

    # Sidebar layout
    sidebarLayout(

      # Sidebar panel for inputs
      sidebarPanel(

        # Input for Tag Prefix
        textInput("tag_prefix", "Tag Prefix:", ""),

        # Input for Tag Number
        numericInput("tag_number", "Tag Number:", value = NULL, min = 0, step = 1),

        # Input for Date
        dateInput("date", "Date:", ""),

        # Input: Person
        textInput("person", "Person"),

        # Input: Person 2
        textInput("person2", "Person 2"),

        # Input: Latitude Degrees
        numericInput("lat_deg", "Latitude Degrees:", value = NULL, min = -90, max = 90),

        # Input: Latitude Decimal Minutes
        numericInput("lat_dec_min", "Latitude Decimal Minutes:", value = NULL, min = 0, max = 59.99, step = 0.01),

        # Input: Longitude Degrees
        numericInput("long_deg", "Longitude Degrees:", value = NULL, min = -180, max = 180),

        # Input: Longitude Decimal Minutes
        numericInput("long_dec_min", "Longitude Decimal Minutes:", value = NULL, min = 0, max = 59.99, step = 0.01),

        # Submit button (disabled initially)
        actionButton("submit", "Submit", disabled = TRUE)
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

    # Disable submit button by default
    shinyjs::disable("submit")

    # Enable submit button only when all mandatory fields are filled
    observe({
      if (nchar(input$tag_prefix) > 0 && is.numeric(input$tag_number) && length(input$date) > 0 &&
          is.numeric(input$lat_deg) && is.numeric(input$lat_dec_min) &&
          is.numeric(input$long_deg) && is.numeric(input$long_dec_min)) {
        shinyjs::enable("submit")
      } else {
        shinyjs::disable("submit")
      }
    })

    # Initialize empty list to store submitted data
    submitted_data <- reactiveVal(data.frame(Tag_Prefix = character(), Tag_Number = numeric(), Date = character(), Person = character(), Person_2 = character(), Latitude = character(), Longitude = character()))

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
      person <- input$person
      person_2 <- input$person2

      # Format latitude degrees and decimal minutes
      lat_deg <- as.character(input$lat_deg)
      lat_dec_min <- as.character(ifelse(input$lat_dec_min < 10, paste0("0", input$lat_dec_min), input$lat_dec_min))
      latitude_ddmm_mm <- paste(lat_deg, lat_dec_min, sep = "")

      # Calculate latitude decimal degree value
      if(lat_deg<0){
        latitude_dddd <- as.numeric(lat_deg) - (as.numeric(lat_dec_min) / 60)
      }else{
        latitude_dddd <- as.numeric(lat_deg) + (as.numeric(lat_dec_min) / 60)
      }


      # Format longitude degrees and decimal minutes
      long_deg <- as.character(input$long_deg)
      long_dec_min <- as.character(ifelse(input$long_dec_min < 10, paste0("0", input$long_dec_min), input$long_dec_min))
      longitude_ddmm_mm <- paste(long_deg, long_dec_min, sep = "")

      # Calculate longitude decimal degree value
      if(long_deg<0){
        longitude_dddd <- as.numeric(long_deg) - (as.numeric(long_dec_min) / 60)
      }else{
        longitude_dddd <- as.numeric(long_deg) + (as.numeric(long_dec_min) / 60)
      }


      # Generate TAG_ID
      tag_id <- paste(tag_prefix, tag_number, sep = "")

      # Prepare SQL insert statement
      sql <- paste("INSERT INTO ELEMENTG.TEST (Tag_Prefix, Tag_Number, TAG_ID, REC_DATE, PERSON, PERSON_2, LAT_DEGREE, LAT_MINUTE, LON_DEGREE, LON_MINUTE, LAT_DD_DDDD, LON_DD_DDDD) VALUES ('", tag_prefix, "', ", tag_number, ", '", tag_id, "', '", date, "', '", person, "', '", person_2, "', '", lat_deg, "', '", lat_dec_min, "', '", long_deg, "', '", long_dec_min, "', ", latitude_dddd, ", ", longitude_dddd, ")", sep="")

      # Execute SQL insert statement
      dbExecute(con, sql)

      # Update status
      output$status <- renderText({
        "Data uploaded to Oracle table ELEMENTG.TEST"
      })

      # Add submitted data to the table
      current_data <- submitted_data()
      new_row <- data.frame(Tag_Prefix = tag_prefix, Tag_Number = tag_number, Date = date, Person = person, Person_2 = person_2, Latitude = paste(lat_deg, "°", lat_dec_min, sep = ""), Longitude = paste(long_deg, "°", long_dec_min, sep = ""))
      updated_data <- rbind(current_data, new_row)
      submitted_data(updated_data)

      # Clear input fields
      shinyjs::reset("tag_prefix")
      shinyjs::reset("tag_number")
      shinyjs::reset("date")
      shinyjs::reset("person")
      shinyjs::reset("person2")
      shinyjs::reset("lat_deg")
      shinyjs::reset("lat_dec_min")
      shinyjs::reset("long_deg")
      shinyjs::reset("long_dec_min")
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




  ########SCRAP
  # library(dplyr)
  # library(svDialogs)
  # library(shiny)
  # library(ROracle)



}








