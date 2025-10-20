
#' @title delete_recaptures
#' @import dplyr RSQLite DBI shiny shinyjs
#' @description allows user to delete chosen tag recaptures and associated paths
#' @export

delete_recaptures <- function(db = NULL,
                              oracle.user =if(exists("oracle.lobtag.user")) oracle.lobtag.user else NULL,
                              oracle.password = if(exists("oracle.lobtag.password")) oracle.lobtag.password else NULL,
                              oracle.dbname = if(exists("oracle.lobtag.server")) oracle.lobtag.server else NULL){

  if(is.null(db)){return(base::message("You need to specify a database with db = "))}

  if(db %in% c("local","Local","LOCAL")){
    db = "local"
  }

####################################################### Main Function:


  # Function to check if the recapture exists
  check_recapture <- function(tag_prefix, tag_number, date_caught, con) {

    # Convert date to desired format
    date_caught_oracle <- format(as.Date(date_caught, "%Y-%m-%d"), "%Y-%m-%d")

    query <- paste0("SELECT PERSON FROM LBT_RECAPTURES WHERE TAG_PREFIX = '", tag_prefix, "' AND TAG_NUMBER = '", tag_number, "' AND REC_DATE = '", date_caught_oracle, "'")
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
    # Convert date to desired format
    date_caught_oracle <- format(as.Date(date_caught, "%Y-%m-%d"), "%Y-%m-%d")

    ## delete recapture
    query1 <- paste0("DELETE FROM LBT_RECAPTURES WHERE TAG_PREFIX = '", tag_prefix, "' AND TAG_NUMBER = '", tag_number, "' AND REC_DATE = '", date_caught_oracle, "'")

    ## delete all single paths for tag
    query2 <- paste0("DELETE FROM LBT_PATH WHERE TAG_PREFIX = '", tag_prefix, "' AND TAG_NUM = '", tag_number, "'")

    ## delete all paths for tag
    query3 <- paste0("DELETE FROM LBT_PATHS WHERE TAG_PREFIX = '", tag_prefix, "' AND TAG_NUM = '", tag_number, "'")

    # Execute the delete querys
    success <- tryCatch({
      if(db %in% "local"){dbBegin(con)}
      dbExecute(con, query1)
      dbExecute(con, query2)
      dbExecute(con, query3)
      dbCommit(con)

      return(TRUE)  # Deletion successful
    }, error = function(e) {
      print(paste("Error in delete_recapture:", e$message))
      return(FALSE)  # Deletion failed
    })
    return(success)
  }

  ## check if there are still other recaptures for tag (path regeneration necessary)
  check.regen <- function(tag_prefix, tag_number,con){
    ## check if other recaptures still exist for this tag (re-pathing necessary)
    q.check <- paste0("SELECT * FROM LBT_RECAPTURES WHERE TAG_PREFIX = '", tag_prefix, "' AND TAG_NUMBER = '", tag_number,"'")
    check <- dbSendQuery(con, q.check)
    check <- fetch(check)
    ## regenerate paths for deleted tag if other recaptures exist
    if(nrow(check)>0){
      regen=TRUE
    }else{regen=FALSE}
    return(regen)
  }

  # UI
  ui <- fluidPage(
    useShinyjs(),  ### this line is needed to make delay() function work in server code
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
        uiOutput("delete_message")
      )
    )
  )




  # Server
  server <- function(input, output, session) {

    # Function to update message text with HTML for color
    update_message <- function(message, color = "black") {
      output$delete_message <- renderUI({
        HTML(paste0("<span style='color:", color, ";'>", message, "</span>"))
      })
    }

    observeEvent(input$search_button, {

      ### open db connection
      db_connection(db, oracle.user, oracle.password, oracle.dbname)

      tag_prefix <- input$tag_prefix
      tag_number <- input$tag_number
      date_caught <- format(input$date_caught, "%Y-%m-%d")

      # Check if the recapture exists
      recapture <- check_recapture(tag_prefix, tag_number, date_caught, con)

      if (is.null(recapture) || nrow(recapture) == 0) {
        update_message("No recapture found with the specified values.")
        output$delete_button <- renderUI(NULL)
      } else {
        person <- recapture$PERSON
        update_message(paste0("This tag was caught on ", date_caught, " by ", person, ". Do you want to delete this recapture?"))
        output$delete_button <- renderUI({
          actionButton("delete_button", "Delete")
        })
        outputOptions(output, "delete_button", suspendWhenHidden = TRUE)  # Ensure button becomes inactive
      }
    })

    observeEvent(input$delete_button, {

      # Disable the delete button immediately upon click
      shinyjs::disable("delete_button")
      output$delete_button <- renderUI(NULL)  # Hide the delete button

      tag_prefix <- input$tag_prefix
      tag_number <- input$tag_number
      date_caught <- format(input$date_caught, "%Y-%m-%d")

      # Delete the recapture
      delete_success <- delete_recapture(tag_prefix, tag_number, date_caught, con)

      # regenerate paths
      regen_needed <- check.regen(tag_prefix, tag_number, con)
      if(regen_needed){
        update_message("There are other recaptures for this tag! Regenerating paths for this tag, please wait ... this may take a while.    DO NOT CLOSE THIS WINDOW!",color = "red")
        print(paste0("Regenerating paths for ",tag_prefix,tag_number," ... please wait"))
        # Introduce a delay before executing the function
        delay(10,
              LobTag2::generate_paths(db = db, tags = paste0(tag_prefix, tag_number)))
        ### reopen db connection
        db_connection(db, oracle.user, oracle.password, oracle.dbname)
      }
      delay(100,
            if (delete_success) {
              update_message("Recapture deleted successfully! Close this window if you are done deleting.")
            } else {
              update_message("Error: Recapture could not be deleted.", color = "red")
            }
      )

    })



  }


  # Run the application
  shinyApp(ui = ui, server = server)




}

