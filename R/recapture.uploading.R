#' @title upload_recaptures
#' @import dplyr ROracle DBI shiny shinyjs svDialogs
#' @description allows individual or batch uploading of recapture data
#' @export

upload_recaptures <- function(){


  # Define UI for application
  ui <- fluidPage(

    # Use shinyjs to include required JavaScript
    shinyjs::useShinyjs(),

    # Title
    titlePanel("Upload Recaptured Tags"),

    # CSS Style for Mandatory Fields
    tags$style(HTML("
    .mandatory-field input[type='text'],
    .mandatory-field input[type='number'],
    .mandatory-field input[type='date'],
    .mandatory-field select {
      border-color: #007bff; /* Blue border color */
    }
        .error-input {
      border-color: red; /* Set border color to red */
      background-color: #FEE; /* Add a light red background */
      color: #A00; /* Change text color to a darker red */
    }
  ")),

    # Sidebar layout
    sidebarLayout(

      # Sidebar panel for inputs
      sidebarPanel(

        # Input for Tag Prefix (Mandatory)
        div(class = "mandatory-field",
            textInput("tag_prefix", "Tag Prefix:", "")),

        # Input for Tag Number (Mandatory)
        div(class = "mandatory-field",
            numericInput("tag_number", "Tag Number:", value = NULL, min = 0, step = 1)),

        # Input for Date (Mandatory)
        div(class = "mandatory-field",
            dateInput("date", "Date:", "")),

        # Input: Person
        textInput("person", "Person"),

        # Conditional panel for additional inputs
        conditionalPanel(
          condition = "input.person != ''",
          div(
            style = "margin-left: 20px;font-size: 12px;",
            textInput("street_address", "Street Address"),
            textInput("town", "Town"),
            textInput("province", "Province"),
            textInput("country", "Country"),
            textInput("postal_code", "Postal Code"),
            textInput("email", "Email"),
            textInput("phone1", "Phone Number 1"),
            textInput("phone2", "Phone Number 2"),
            textInput("affiliation", "Affiliation"),
            textInput("license_area", "License Area"),
            # Add more input fields as needed
          )
        ),

        # Input: Person 2
        textInput("person2", "Person 2"),

        # Input: Latitude Degrees (Mandatory)
        div(class = "mandatory-field",
            numericInput("lat_deg", "Latitude Degrees:", value = NULL, min = -90, max = 90)),

        # Input: Latitude Decimal Minutes (Mandatory)
        div(class = "mandatory-field",
            numericInput("lat_dec_min", "Latitude Decimal Minutes:", value = NULL, min = 0, max = 59.99, step = 0.01)),

        # Input: Longitude Degrees (Mandatory)
        div(class = "mandatory-field",
            numericInput("long_deg", "Longitude Degrees:", value = NULL, min = -180, max = 180)),

        # Input: Longitude Decimal Minutes (Mandatory)
        div(class = "mandatory-field",
            numericInput("long_dec_min", "Longitude Decimal Minutes:", value = NULL, min = 0, max = 59.99, step = 0.01)),

        # Input: Depth Fathoms
        numericInput("depth_fathoms", "Depth (fathoms):", value = NULL, min = 0),

        # Input: Released
        selectInput("relcode", "Released?",choices = c("","yes", "no", "unknown")),

        # Input: Captain
        textInput("captain", "Captain"),

        # Input: Vessel
        textInput("vessel", "Vessel"),

        # Input: Management Area
        textInput("management_area", "Management Area"),

        # Input: Capture Length
        numericInput("capture_length", "Capture Length:", value = NULL, min = 0),

        # Input: Sex
        selectInput("sex", "Sex:",choices = c("","1", "2", "3")),

        # Input: Egg State
        selectInput("egg_state", "Egg State:", choices = c("","1", "2", "3","4","5")),

        # Input: Comments
        textInput("comments", "Comments"),

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

    # Observer to check if Tag Prefix and Tag Number combination exists in the Oracle table
    observeEvent(c(input$tag_prefix, input$tag_number), {
      if (nchar(input$tag_prefix) > 0 & is.numeric(input$tag_number)) {
        query <- paste("SELECT * FROM ELEMENTG.LBT_RELEASES WHERE TAG_PREFIX = '", input$tag_prefix, "' AND TAG_NUM = ", input$tag_number, sep = "")
        result <- dbGetQuery(con, query)
        if (nrow(result) == 0) {
          # Update style and display warning message if combination not found
          shinyjs::addClass("tag_number", "error-input")
          shinyjs::addClass("tag_prefix", "error-input")
          output$release_error <- renderText({
            "No release data found for this tag"
          })
        } else {
          # Remove error style and message if combination found
          shinyjs::removeClass("tag_number", "error-input")
          shinyjs::removeClass("tag_prefix", "error-input")
          output$release_error <- renderText({
            ""
          })
        }
      }
    })

    # Real-time data retrieval based on "Person" field input
    observeEvent(input$person, {
      if (nchar(input$person) > 0) {
        query <- paste("SELECT CIVIC, TOWN, PROV, COUNTRY, POST, EMAIL, PHO1, PHO2, AFFILIATION, LICENSE_AREA FROM ELEMENTG.LBT_PEOPLE WHERE NAME = '", input$person, "'", sep = "")
        result <- dbGetQuery(con, query)
        if (nrow(result) > 0) {
          updateTextInput(session, "street_address", value = result$CIVIC)
          updateTextInput(session, "town", value = result$TOWN)
          updateTextInput(session, "province", value = result$PROV)
          updateTextInput(session, "country", value = result$COUNTRY)
          updateTextInput(session, "postal_code", value = result$POST)
          updateTextInput(session, "email", value = result$EMAIL)
          updateTextInput(session, "phone1", value = result$PHO1)
          updateTextInput(session, "phone2", value = result$PHO2)
          updateTextInput(session, "affiliation", value = result$AFFILIATION)
          updateTextInput(session, "license_area", value = result$LICENSE_AREA)
        }
      } else {
        # Reset sub-fields when "Person" field is empty
        updateTextInput(session, "street_address", value = "")
        updateTextInput(session, "town", value = "")
        updateTextInput(session, "province", value = "")
        updateTextInput(session, "country", value = "")
        updateTextInput(session, "postal_code", value = "")
        updateTextInput(session, "email", value = "")
        updateTextInput(session, "phone1", value = "")
        updateTextInput(session, "phone2", value = "")
        updateTextInput(session, "affiliation", value = "")
        updateTextInput(session, "license_area", value = "")
      }
    })

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
      latitude_dddd <- as.numeric(lat_deg) + (as.numeric(lat_dec_min) / 60)

      # Format longitude degrees and decimal minutes
      long_deg <- as.character(input$long_deg)
      long_dec_min <- as.character(ifelse(input$long_dec_min < 10, paste0("0", input$long_dec_min), input$long_dec_min))
      longitude_ddmm_mm <- paste(long_deg, long_dec_min, sep = "")

      # Calculate longitude decimal degree value
      longitude_dddd <- as.numeric(long_deg) + (as.numeric(long_dec_min) / 60)


      # Check if selected and numeric (non-mandatory) fields are empty and handle by making null
      # selected values
      released <- ifelse(input$relcode == "", "NULL", input$relcode)
      sex <- ifelse(input$sex == "", "NULL", input$sex)
      egg_state <- ifelse(input$egg_state == "", "NULL", input$egg_state)
      # numeric values
      capture_length <- ifelse(is.na(input$capture_length), "NULL", input$capture_length)
      depth_fathoms <- ifelse(is.na(input$depth_fathoms), "NULL", input$depth_fathoms)

      # Generate TAG_ID
      tag_id <- paste(tag_prefix, tag_number, sep = "")

      # Prepare SQL insert statement
      sql_1 <- paste("INSERT INTO ELEMENTG.LBT_RECAPTURES (Tag_Prefix, Tag_Number, TAG_ID, REC_DATE, PERSON, PERSON_2, LAT_DEGREE, LAT_MINUTE, LON_DEGREE, LON_MINUTE, LAT_DD, LON_DD, CAPTAIN, VESSEL, YEAR, MANAGEMENT_AREA, CAPTURE_LENGTH, SEX, EGG_STATE, REWARDED, COMMENTS, RELCODE, FATHOMS) VALUES ('", tag_prefix, "', ", tag_number, ", '", tag_id, "', '", date, "', '", person, "', '", person_2, "', ", lat_deg, ", ", lat_dec_min, ", ", long_deg, ", ", long_dec_min, ", ", latitude_dddd, ", ", longitude_dddd, ", '", input$captain, "', '", input$vessel, "', EXTRACT(YEAR FROM TO_DATE('", date, "', 'DD/MM/YYYY')), '", input$management_area, "', ", capture_length, ", ", sex, ", ", egg_state, ", 'no', '", input$comments, "', ", released, ", ", depth_fathoms, ")", sep="")

      # Execute SQL insert statement
      dbExecute(con, sql_1)

      # Prepare SQL update/insert statement for the LBT_PEOPLE table and insert data
      if (nchar(input$person) > 0){
        sql_2 <- paste("
      MERGE INTO ELEMENTG.LBT_PEOPLE tgt
      USING (SELECT '", input$person, "' AS name FROM dual) src
      ON (tgt.NAME = src.name)
      WHEN MATCHED THEN
      UPDATE SET tgt.CIVIC = '", input$street_address, "',
                 tgt.TOWN = '", input$town, "',
                 tgt.PROV = '", input$province, "',
                 tgt.COUNTRY = '", input$country, "',
                 tgt.POST = '", input$postal_code, "',
                 tgt.EMAIL = '", input$email, "',
                 tgt.PHO1 = '", input$phone1, "',
                 tgt.PHO2 = '", input$phone2, "',
                 tgt.AFFILIATION = '", input$affiliation, "',
                 tgt.LICENSE_AREA = '", input$license_area, "'
      WHEN NOT MATCHED THEN
      INSERT (tgt.NAME, tgt.CIVIC, tgt.TOWN, tgt.PROV, tgt.COUNTRY, tgt.POST, tgt.EMAIL, tgt.PHO1, tgt.PHO2, tgt.AFFILIATION, tgt.LICENSE_AREA)
      VALUES ('", input$person, "', '", input$street_address, "', '", input$town, "', '", input$province, "', '", input$country, "', '", input$postal_code, "', '", input$email, "', '", input$phone1, "', '", input$phone2, "', '", input$affiliation, "', '", input$license_area, "')"
                       , sep = "")

        # Execute SQL update/insert statement for the LBT_PEOPLE table
        dbExecute(con, sql_2)
      }

      # Update status
      output$status <- renderText({
        "Tags to be uploaded to Oracle. Close this window to complete upload."
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
      shinyjs::reset("depth_fathoms")
      shinyjs::reset("relcode")
      shinyjs::reset("captain")
      shinyjs::reset("vessel")
      shinyjs::reset("management_area")
      shinyjs::reset("capture_length")
      shinyjs::reset("sex")
      shinyjs::reset("egg_state")
      shinyjs::reset("comments")
    })

    # Render submitted data
    output$submitted_data <- renderTable({
      submitted_data()[,1:7]
    })

    # Close Oracle connection on app exit
    session$onSessionEnded(function() {
      dbDisconnect(con)
    })
  }

  # Run the application
  shinyApp(ui = ui, server = server)







}


#' @title batch_upload_recaptures
#' @import dplyr ROracle DBI shiny DT svDialogs
#' @description batch uploads tag recaptures data
#' @export
batch_upload_recaptures <- function(){
  ## Allow user to choose data file to upload
  dlg_message("In the following window, choose a csv file containing your recaptures data")
  file_path <- dlg_open(filter = dlg_filters["csv",])$res
  recaptures <- read.csv(file_path)
  rec <- recaptures
  ## Process / standardize the data table

  ##testing
  # rec[1,12] =NA
  # rec[2,12] = NA
  # rec[2,13] =NA
  # rec[2,8] = 44
  # rec[2,9] =33.456
  # rec[2,10] = -63
  # rec[2,11] =-34.456
  # rec <- rbind(rec,rec[1,])

  ## check what coordinate is provided and autofill if necessary
  for (i in 1:nrow(rec)){

    if(is.na(rec$LAT_DD[i])){
      if(!is.na(rec$LAT_MINUTE[i]) & rec$LAT_MINUTE[i]<0){rec$LAT_MINUTE[i]=rec$LAT_MINUTE[i]*(-1)}
      if(!is.na(rec$LAT_DEGREE[i]) & rec$LAT_DEGREE[i]<0){
        rec$LAT_DD[i] = rec$LAT_DEGREE[i] - rec$LAT_MINUTE[i]/60
      }else{rec$LAT_DD[i] = rec$LAT_DEGREE[i] + rec$LAT_MINUTE[i]/60}
    }

    if(is.na(rec$LON_DD[i])){
      if(rec$LON_MINUTE[i]<0){rec$LON_MINUTE[i]=rec$LON_MINUTE[i]*(-1)}
      if(rec$LON_DEGREE[i]<0){
        rec$LON_DD[i] = rec$LON_DEGREE[i] - rec$LON_MINUTE[i]/60
      }else{rec$LON_DD[i] = rec$LON_DEGREE[i] + rec$LON_MINUTE[i]/60}
    }

  }

  rec <- rec %>% mutate(TAG_ID = paste0(TAG_PREFIX,as.character(TAG_NUMBER)))
  rec <- rec %>% mutate(REC_DATE = paste(DAY,MONTH,YEAR, sep="/"))
  rec$REC_DATE = format(as.Date(rec$REC_DATE, format = "%d/%m/%Y"), "%d/%m/%Y")
  ## clean vessel names for problematic characters
  rec$VESSEL = gsub("'","",rec$VESSEL)

  ## error checking (General):
  bad_tag_pre = which(rec$TAG_PREFIX %in% NA)
  bad_tag_num = which(rec$TAG_NUMBER %in% NA | as.numeric(rec$TAG_NUMBER) %in% NA)

  bad_lat = which(rec$LAT_DD %in% NA | nchar(as.character(rec$LAT_DD))<2 | !is.numeric(rec$LAT_DD))
  bad_lon = which(rec$LON_DD %in% NA | nchar(as.character(rec$LON_DD))<2 | !is.numeric(rec$LON_DD))
  bad_date = which(rec$REL_DATE %in% NA)

  ## check if any recapture events seems to be replicates
  rec <- rec %>% mutate(reps = paste(TAG_ID,REC_DATE,LAT_DD,LON_DD))
  repeat_event = which(duplicated(rec$reps)==TRUE)

  error_out= ""
  error_tab = NULL
  return_error = FALSE

  if(length(bad_tag_pre) > 0){
    for(i in bad_tag_pre){
      error_out = paste(error_out, "\nMissing tag prefix for tag number:",rec$TAG_NUMBER[i],"at row:",i)
      error_tab = rbind(error_tab,c(i,rec$TAG_PREFIX[i],rec$TAG_NUMBER[i],"Missing tag prefix"))
    }
    return_error = TRUE
  }
  if(length(bad_tag_num) > 0){
    for(i in bad_tag_num){
      error_out = paste(error_out, "\nBad or missing tag number at row:", i)
      error_tab = rbind(error_tab,c(i,rec$TAG_PREFIX[i],rec$TAG_NUMBER[i],"Bad or missing tag number"))
    }
    return_error = TRUE
  }
  if(length(bad_lat) > 0){
    for(i in bad_lat){
      error_out = paste(error_out, "\nBad or missing latitude for tag:",rec$TAG_ID[i],"at row:",i)
      error_tab = rbind(error_tab,c(i,rec$TAG_PREFIX[i],rec$TAG_NUMBER[i],"Bad or missing latitude"))
    }
    return_error = TRUE
  }
  if(length(bad_lon) > 0){
    for(i in bad_lon){
      error_out = paste(error_out, "\nBad or missing longitude for tag:",rec$TAG_ID[i],"at row:",i)
      error_tab = rbind(error_tab,c(i,rec$TAG_PREFIX[i],rec$TAG_NUMBER[i],"Bad or missing longitude"))
    }
    return_error = TRUE
  }
  if(length(bad_date) > 0){
    for(i in bad_date){
      error_out = paste(error_out, "\nBad or missing date for tag:", rec$TAG_ID[i],"at row:",i)
      error_tab = rbind(error_tab,c(i,rec$TAG_PREFIX[i],rec$TAG_NUMBER[i],"Bad or missing date"))
    }
    return_error = TRUE
  }
  if(length(repeat_event) > 0){
    for(i in repeat_event){
      error_out = paste(error_out, "\nDuplicate recapture event suspected for:", rec$TAG_ID[i],"at row:",i,"Is this really a separate recapture event?")
      error_tab = rbind(error_tab,c(i,rec$TAG_PREFIX[i],rec$TAG_NUMBER[i],"Suspected duplicate recapture event"))
    }
    return_error = TRUE
  }

  if(return_error){
    colnames(error_tab)=c("Row","Tag Prefix","Tag Number","Error")
    ## Create interactive dialogue showing uploading errors and giving user option to download these in a table

    # Define the UI
    ui <- fluidPage(
      titlePanel("Uploading Errors"),
      mainPanel(
        # Display text from the string variable
        h3("Fix issues below and try uploading again:"),
        verbatimTextOutput("text_output"),

        # Button to download the table
        downloadButton("download_table", label = "Download Table")
      )
    )

    # Define the server logic
    server <- function(input, output) {
      # Display the text from the string variable
      output$text_output <- renderText({
        error_out
      })

      # Function to generate a downloadable file
      output$download_table <- downloadHandler(
        filename = function() {
          "recaptures_uploading_errors.csv"
        },
        content = function(file) {
          write.csv(error_tab, file,row.names = F)
        }
      )
    }

    # Create the Shiny app object
    return(shinyApp(ui = ui, server = server))
  }

  if(!return_error){
    ###### ORACLE UPLOAD HERE. Check that entry doesn't already exist before uploading

    table_name <- "ELEMENTG.LBT_RECAPTURES"
    people.tab.name <- "ELEMENTG.LBT_PEOPLE"
    ### open ORACLE connection
    tryCatch({
      drv <- DBI::dbDriver("Oracle")
      con <- ROracle::dbConnect(drv, username = oracle.personal.user, password = oracle.personal.password, dbname = oracle.personal.server)
    }, warning = function(w) {
    }, error = function(e) {
      return(toJSON("Connection failed"))
    }, finally = {
    })

    ## check for already entered recapture events, then upload all new recaptures
    entered =NULL
    for(i in 1:nrow(rec)){
      #sql <- paste("SELECT * FROM ",table_name, " WHERE TAG_ID = '", rec$TAG_ID[i], "'"," AND REC_DATE = '", rec$REC_DATE[i], "'"," AND LAT_DD = ", rec$LAT_DD[i]," AND LON_DD = ", rec$LON_DD[i],sep = "")
      sql <- paste("SELECT * FROM ",table_name, " WHERE TAG_ID = '", rec$TAG_ID[i], "'"," AND REC_DATE = '", rec$REC_DATE[i],"'",sep = "")
      check <- dbSendQuery(con, sql)
      existing_event <- dbFetch(check)
      entered <- rbind(entered,existing_event)
      dbClearResult(check)

      if(nrow(existing_event)==0){
        sql <- paste("INSERT INTO ",table_name, " VALUES ('",rec$TAG_PREFIX[i],"', '",rec$TAG_NUMBER[i],"', '",rec$TAG_ID[i],"', '",rec$REC_DATE[i],"','",rec$PERSON[i],"','",rec$PERSON_2[i],"','",rec$LAT_DEGREE[i],"','",rec$LAT_MINUTE[i],"','",rec$LON_DEGREE[i],"','",rec$LON_MINUTE[i],"','",rec$LAT_DD[i],"','",rec$LON_DD[i],"','",rec$FATHOMS[i],"','",rec$RELCODE[i],"','",rec$CAPTAIN[i],"','",rec$VESSEL[i],"','",rec$YEAR[i],"','",rec$MANAGEMENT_AREA[i],"','",rec$CAPTURE_LENGTH[i],"','",rec$SEX[i],"','",rec$EGG_STATE[i],"','","no","','",rec$COMMENTS[i],"')", sep = "")
        result <- dbSendQuery(con, sql)
        dbCommit(con)
        dbClearResult(result)
      }

      ## check and update PEOPLE table if person is new
      sql <- paste("SELECT * FROM ",people.tab.name, " WHERE NAME = '", rec$PERSON[i],"'",sep = "")
      check <- dbSendQuery(con, sql)
      existing_person <- dbFetch(check)
      dbClearResult(check)

      if(nrow(existing_person)==0){
        sql <- paste("INSERT INTO ",people.tab.name, " VALUES ('",rec$PERSON[i],"', '",rec$CIVIC[i],"', '",rec$TOWN[i],"', '",rec$PROV[i],"','",rec$COUNTRY[i],"','",rec$POST[i],"','",rec$EMAIL[i],"','",rec$PHO1[i],"','",rec$PHO2[i],"','",rec$AFFILIATION[i],"','",rec$LICENSE_AREA[i],"')", sep = "")
        result <- dbSendQuery(con, sql)
        dbCommit(con)
        dbClearResult(result)
      }

    }

    dbDisconnect(con)

    ### show interactive info window if there were any tags found to be already entered
    if(nrow(entered)>0){
      # Define UI for application
      ui <- fluidPage(
        titlePanel("Upload Success!"),
        tags$br(),
        h4("Recaptures for the following tags on the same day were found already in the database, these are assumed to be the same event so were not uploaded:"),
        sidebarLayout(
          sidebarPanel(
            # Text box to display all TAG_NUM values
            textOutput("tag_values")
          ),

          mainPanel(
            # Display table based on selection
            DTOutput("table"),

            # Download button
            downloadButton("download_table", "Download Table")
          )
        )
      )


      # Define server logic
      server <- function(input, output) {
        # Render unique TAG_NUM values in text box
        output$tag_values <- renderText({
          paste(unique(entered$TAG_ID), collapse = ", ")
        })

        # Render table based on selection
        output$table <- renderDT({
          datatable(entered)
        })

        # Download entire table
        output$download_table <- downloadHandler(
          filename = function() {
            paste("entered_table", ".csv", sep = "")
          },
          content = function(file) {
            write.csv(entered, file, row.names = FALSE)
          }
        )
      }

      # Run the application
      shinyApp(ui = ui, server = server)

    }else{
      dlg_message("All recaptures uploaded successfully without errors!")
    }



  }
  # library(dplyr)
  # library(ROracle)
  # library(shiny)
  # library(shinyjs)
  # library(svDialogs)
  # library(DT)

}

