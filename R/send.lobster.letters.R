
#' @title  send.lobster.letters
#' @description  Review and send reward letters to fishers emails.
#' @import ROracle DBI dplyr shiny svDialogs gmailr
#' @export
send.lobster.letters <- function(db = "Oracle",
                         oracle.user = oracle.personal.user,
                         oracle.password = oracle.personal.password,
                         oracle.dbname = oracle.personal.server){

  ## choose location of letters
  dlg_message("In the following window, choose the directory where your letters are stored.")
  input.location <- dlg_dir(filter = dlg_filters["csv",])$res

  ## gmailr function
  send.email <- function(fisher_name = NULL, email_to = NULL, total_attach = NULL){
    ### email text
    subject = paste("Lobster Tag Report for ", fisher_name, sep = "")
    body_header = paste(fisher_name, ",\n\n", sep = "")
    #body_main = "Thanks for your participation in the lobster tagging program.\nPlease see attached report for movement information about the tagged lobster(s) you reported."
    body_main = "Thanks for your participation in the lobster tagging program. Please see attached tag recapture report for information on the tag(s) that you reported to us.

Undererstanding how fast lobsters grow in the wild is difficult but tagged lobster measurements are very helpful in determining this. For future tag recaptures, if you are able to share a picture of the lobster with the measuring guage laid on its back, we can use this to determine lobster growth.

Much appreciated.

DFO Lobster Science Team
Bedford Institute of Oceanography,
1 Challenger Dr.
Dartmouth, NS B2Y 4A2"

    body = paste0(body_header, body_main)


    ### send email
    my_email_message <- gm_mime() %>%
      gm_to(email_to) %>%
      gm_from("lobtags@gmail.com") %>%
      gm_subject(subject) %>%
      gm_text_body(body) %>%
      gm_attach_file(total_attach)

    gm_send_message(my_email_message)

  }


### open Oracle connection ahead of time, for some reason need to do this both here and in server function
  if(db %in% "Oracle"){
    tryCatch({
      drv <- DBI::dbDriver("Oracle")
      con <- ROracle::dbConnect(drv, username = oracle.user, password = oracle.password, dbname = oracle.dbname)
    }, warning = function(w) {
    }, error = function(e) {
      return(toJSON("Connection failed"))
    }, finally = {
    })
  }

  # Define UI
  ui <- fluidPage(
    titlePanel("Letter Review App"),

    sidebarLayout(
      sidebarPanel(
        actionButton("open_pdf", "Open Next PDF"),
        textOutput("pdf_message"),
        br(),
        uiOutput("confirm_ui")  # Dynamic UI for confirmation after the PDF is closed
      ),

      mainPanel(
        textOutput("confirm_message")
      )
    )
  )

  # Define server logic
  server <- function(input, output, session) {
    # Keep track of the list of PDFs and which one is current
    #input.location <- "C:/LOBTAG/letters"  # Modify this to the folder where your PDFs are stored
    pdf_files <- list.files(input.location, pattern = "\\.pdf$", full.names = TRUE)
    pdf_counter <- reactiveVal(1)  # Start with the first PDF
    pdf_opened <- reactiveVal(FALSE)  # Track whether the PDF has been opened
    user_email <- reactiveVal(NULL)  # Store the email address reactively
    file_name <- reactiveVal(NULL)   # Store the file name reactively

    # Reactivity to show messages
    output$pdf_message <- renderText({
      if (pdf_opened()) {
        paste("Opened PDF:", pdf_files[pdf_counter()])
      } else {
        "No PDF opened yet."
      }
    })

    # Open the next PDF when the button is clicked
    observeEvent(input$open_pdf, {
      # Get the current PDF to open
      pdf_path <- pdf_files[pdf_counter()]

      # Open the PDF using the default viewer
      shell.exec(pdf_path)

      # Update the opened status
      pdf_opened(TRUE)

      # Update the UI to ask if the letter is ready to send
      output$confirm_ui <- renderUI({
        tagList(
          br(),
          actionButton("confirm", "I have closed the PDF, ready to confirm email address")
        )
      })
    })

    # Confirmation UI after the PDF is closed
    observeEvent(input$confirm, {

      # Connect to Oracle database
      if(db %in% "Oracle"){
        tryCatch({
          drv <- DBI::dbDriver("Oracle")
          con <- ROracle::dbConnect(drv, username = oracle.user, password = oracle.password, dbname = oracle.dbname)
        }, warning = function(w) {
        }, error = function(e) {
          return(toJSON("Connection failed"))
        }, finally = {
        })
      }

      # Extract name from PDF file name (assuming it's in the format "Name_Surname.pdf")
      current_file_name <- tools::file_path_sans_ext(basename(pdf_files[pdf_counter()]))  ###reactively stored
      file_name(current_file_name)

      # Query the Oracle table to retrieve the email based on the name
      email_query <- paste0("SELECT EMAIL FROM LBT_PEOPLE WHERE NAME = '", file_name(), "'")
      email_result <- dbGetQuery(con, email_query)

      # Check if an email was found
      if (nrow(email_result) > 0) {
        user_email(email_result$EMAIL[1])  # Get the first email found (reactively stored)

        output$confirm_ui <- renderUI({
          tagList(
            h3(paste("Ready to send the letter to:", user_email())),
            actionButton("ready_yes", "Yes, Send it!", style = "background-color: red; color: white;"),
            actionButton("ready_no", "No, Not Ready")
          )
        })
      } else {
        output$confirm_ui <- renderUI({
          tagList(
            h3("No email found for this name."),
            actionButton("next_pdf", "Proceed to Next PDF")
          )
        })
      }
    })

    # Move to the next PDF after confirmation
    observeEvent(input$ready_yes, {

      current_pdf <- pdf_files[pdf_counter()]

      ## create and send the email
      send.email(fisher_name = file_name(),email_to = user_email(),total_attach = current_pdf)

      ## update REWARDED status for now rewarded tags
      map.files <- list.files(paste0(dirname(input.location),"/maps"),full.names = T)
      sent.tags <- map.files[grep(file_name(), map.files)]
      sent.tags <- gsub(file_name(),"",basename(sent.tags))
      sent.tags <- gsub(".pdf","",sent.tags)

      if(db %in% "Oracle"){
        tryCatch({
          drv <- DBI::dbDriver("Oracle")
          con <- ROracle::dbConnect(drv, username = oracle.user, password = oracle.password, dbname = oracle.dbname)
        }, warning = function(w) {
        }, error = function(e) {
          return(toJSON("Connection failed"))
        }, finally = {
        })
      }

      for (i in sent.tags){
          update_query <- sprintf("
    UPDATE LBT_RECAPTURES
    SET REWARDED = '%s'
    WHERE TAG_ID = '%s' AND PERSON = '%s'",
    "yes",i,file_name())

          # Execute the update query in Oracle
          dbExecute(con, update_query)
      }
      dbCommit(con)

      # Move the current PDF to the sent folder
      sent_folder <- file.path(input.location, "sent")
      dir.create(sent_folder, showWarnings = FALSE)  # Create folder if it doesn't exist

      file.rename(current_pdf, file.path(sent_folder, basename(current_pdf)))  # Move the PDF file

      pdf_counter(pdf_counter() + 1)  # Increment to the next PDF
      pdf_opened(FALSE)  # Reset opened status
      output$confirm_ui <- renderUI(NULL)  # Reset confirmation UI

      # Update the confirmation message to inform the user
      output$confirm_message <- renderText({
        paste("Letter:", basename(current_pdf), "has been moved to the 'sent' folder.")
      })
    })

    observeEvent(input$ready_no, {
      # Move the current PDF to the not.sent folder
      not_sent_folder <- file.path(input.location, "not.sent")
      dir.create(not_sent_folder, showWarnings = FALSE)  # Create folder if it doesn't exist

      current_pdf <- pdf_files[pdf_counter()]
      file.rename(current_pdf, file.path(not_sent_folder, basename(current_pdf)))  # Move the PDF file

      pdf_counter(pdf_counter() + 1)  # Increment to the next PDF
      pdf_opened(FALSE)  # Reset opened status
      output$confirm_ui <- renderUI(NULL)  # Reset confirmation UI

      # Update the confirmation message to inform the user
      output$confirm_message <- renderText({
        paste("Letter:", basename(current_pdf), "has been moved to the 'not.sent' folder.")
      })
    })

    observeEvent(input$next_pdf, {
      pdf_counter(pdf_counter() + 1)  # Move to the next PDF
      pdf_opened(FALSE)  # Reset opened status
      output$confirm_ui <- renderUI(NULL)  # Reset confirmation UI
    })

    # Disconnect from the database when the app is closed
    on.exit(dbDisconnect(con), add = TRUE)
  }

  # Run the application
  shinyApp(ui = ui, server = server)
}











