
#' @title delete_recaptures
#' @import dplyr ROracle DBI shiny
#' @description allows user to delete chosen tag recaptures and associated paths
#' @export

delete_recaptures <- function(username = oracle.personal.user, password = oracle.personal.password, dbname = oracle.personal.server){

  oracle.personal.user<<-username
  oracle.personal.password<<-password
  oracle.personal.server<<-dbname



  # Function to check if the recapture exists
  check_recapture <- function(tag_prefix, tag_number, date_caught, con) {
    # Convert date to Oracle date format
    date_caught_oracle <- format(as.Date(date_caught, "%Y-%m-%d"), "%d/%m/%Y")

    query <- paste0("SELECT PERSON FROM ",oracle.personal.user,".LBT_RECAPTURES WHERE TAG_PREFIX = '", tag_prefix, "' AND TAG_NUMBER = '", tag_number, "' AND REC_DATE = '", date_caught_oracle, "'")
    result <- tryCatch({
      dbGetQuery(con, query)
    }, error = function(e) {
      print(paste("Error in check_recapture:", e$message))
      return(NULL)
    })
    return(result)
  }

  # Function to delete the recapture
  delete_recapture <- function(tag_prefix, tag_number, date_caught, con) {
    # Convert date to Oracle date format
    date_caught_oracle <- format(as.Date(date_caught, "%Y-%m-%d"), "%d/%m/%Y")

    query1 <- paste0("DELETE FROM ",oracle.personal.user,".LBT_RECAPTURES WHERE TAG_PREFIX = '", tag_prefix, "' AND TAG_NUMBER = '", tag_number, "' AND REC_DATE = '", date_caught_oracle, "'")
    query2 <- paste0("DELETE FROM ",oracle.personal.user,".LBT_PATH WHERE TAG_PREFIX = '", tag_prefix, "' AND TAG_NUMBER = '", tag_number, "' AND CDATE = '", date_caught, "'")

    ##for paths, need to check which recapture it is in sequence and update CID values after delete
    q <- paste0("SELECT CID FROM ",oracle.personal.user,".LBT_PATHS WHERE TAG_PREFIX = '", tag_prefix, "' AND TAG_NUMBER = '", tag_number, "' AND REC_DATE = '", date_caught_oracle,"' AND REC_PERSON = '",person,"'")


    # Execute the delete querys
    success <- tryCatch({
      dbExecute(con, query1)
      dbExecute(con, query2)
      dbCommit(con)
      dbDisconnect(con)
      return(TRUE)  # Deletion successful
    }, error = function(e) {
      print(paste("Error in delete_recapture:", e$message))
      return(FALSE)  # Deletion failed
    })
    return(success)
  }

  # UI
  ui <- fluidPage(
    titlePanel("Recapture Deletion"),
    sidebarLayout(
      sidebarPanel(
        textInput("tag_prefix", "Tag Prefix"),
        textInput("tag_number", "Tag Number"),
        dateInput("date_caught", "Date Caught"),
        actionButton("search_button", "Search"),
        uiOutput("delete_button")  # Placeholder for delete button
      ),
      mainPanel(
        textOutput("delete_message")
      )
    )
  )

  # Server
  server <- function(input, output) {
    # Connect to Oracle database
    con <- dbConnect(ROracle::Oracle(), username = oracle.personal.user, password = oracle.personal.password, dbname = oracle.personal.server)

    observeEvent(input$search_button, {
      tag_prefix <- input$tag_prefix
      tag_number <- input$tag_number
      date_caught <- format(input$date_caught, "%Y-%m-%d")

      # Check if the recapture exists
      recapture <- check_recapture(tag_prefix, tag_number, date_caught, con)

      if (is.null(recapture) || nrow(recapture) == 0) {
        output$delete_message <- renderText("No recapture found with the specified values.")
        output$delete_button <- renderUI(NULL)
      } else {
        person <- recapture$PERSON
        output$delete_message <- renderText({
          paste("This tag was caught on", date_caught, "by", person, ". Do you want to delete this recapture?")
        })
        output$delete_button <- renderUI({
          actionButton("delete_button", "Delete")
        })
        outputOptions(output, "delete_button", suspendWhenHidden = FALSE)  # Ensure button remains active
      }
    })

    observeEvent(input$delete_button, {
      print("Delete button clicked")  # Debug message

      tag_prefix <- input$tag_prefix
      tag_number <- input$tag_number
      date_caught <- format(input$date_caught, "%Y-%m-%d")

      # Delete the recapture
      delete_success <- delete_recapture(tag_prefix, tag_number, date_caught, con)

      print(paste("Delete success:", delete_success))  # Debug message

      if (delete_success) {
        output$delete_message <- renderText("Recapture deleted successfully.")
      } else {
        output$delete_message <- renderText("Error: Recapture could not be deleted.")
      }
    })
  }

  # Run the application
  shinyApp(ui = ui, server = server)




}

