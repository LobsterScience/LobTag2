#' @title upload_recaptures
#' @import dplyr RSQLite DBI shiny shinyjs svDialogs
#' @description allows individual or batch uploading of recapture data
#' @export

upload_recaptures <- function(db = "local",oracle.user = oracle.personal.user, oracle.password = oracle.personal.password, oracle.dbname = oracle.personal.server){


  ## only install / load ROracle if the user chooses Oracle functionality
  if(db %in% "Oracle"){
    pkg <- "ROracle"
    if (!requireNamespace(pkg, quietly = TRUE)) {
      # If not installed, install the package
      install.packages(pkg)

      # Load the package after installing
      library(pkg, character.only = TRUE)
    } else {
      # If already installed, just load the package
      library(pkg, character.only = TRUE)
    }
  }
  #######################################################

  #####################################################################################################
  #####################################################################################################
  ## Check if recaptures and people tables already exist and create if not

  ### open db connection
  if(db %in% "Oracle"){
    tryCatch({
      drv <- DBI::dbDriver("Oracle")
      con <- ROracle::dbConnect(drv, username = oracle.user, password = oracle.password, dbname = oracle.dbname)
    }, warning = function(w) {
    }, error = function(e) {
      return(toJSON("Connection failed"))
    }, finally = {
    })
  }else{
    dir.create("C:/LOBTAG",showWarnings = F)
    con <- dbConnect(RSQLite::SQLite(), "C:/LOBTAG/LOBTAG.db")
    }

  table_name <- "LBT_RECAPTURES"

  ## look for existing table
  if(db %in% "Oracle"){
    query <- paste("SELECT COUNT(*) FROM user_tables WHERE table_name = '", table_name, "'", sep = "")
  }else{query <- paste("SELECT COUNT(*) AS table_count FROM sqlite_master WHERE type='table' AND name='", table_name, "'", sep = "")}

  result <- dbGetQuery(con, query)
  # If the table does not exist, create it
  if (result[[1]] == 0) {
    print(paste0("Creating new table called: ",ifelse(db %in% "Oracle",paste0(oracle.user,"."),""),table_name))
    # Define the SQL statement to create the table
    sql_statement <- paste0("
    CREATE TABLE ",table_name," (
    TAG_PREFIX VARCHAR2(50),
    TAG_NUMBER VARCHAR2(50),
    TAG_ID VARCHAR2(50),
    REC_DATE VARCHAR2(50),
    PERSON VARCHAR2(100),
    PERSON_2 VARCHAR2(100),
    LAT_DEGREE VARCHAR2(50),
    LAT_MINUTE VARCHAR2(50),
    LON_DEGREE VARCHAR2(50),
    LON_MINUTE VARCHAR2(50),
    LAT_DD VARCHAR2(50),
    LON_DD VARCHAR2(50),
    FATHOMS VARCHAR2(50),
    RELEASED VARCHAR2(50),
    CAPTAIN VARCHAR2(100),
    VESSEL VARCHAR2(100),
    YEAR VARCHAR2(50),
    MANAGEMENT_AREA VARCHAR2(100),
    CAPTURE_LENGTH VARCHAR2(50),
    SEX VARCHAR2(10),
    EGG_STATE VARCHAR2(50),
    REWARDED VARCHAR2(10),
    COMMENTS VARCHAR2(1000)
)")

    # Execute the SQL statement
    dbSendQuery(con, sql_statement)

}

    table_name <- "LBT_PEOPLE"
    ## look for existing table
    if(db %in% "Oracle"){
      query <- paste("SELECT COUNT(*) FROM user_tables WHERE table_name = '", table_name, "'", sep = "")
    }else{query <- paste("SELECT COUNT(*) AS table_count FROM sqlite_master WHERE type='table' AND name='", table_name, "'", sep = "")}
    result <- dbGetQuery(con, query)
    # If the table does not exist, create it
    if (result[[1]] == 0) {
    print(paste0("Creating new table called: ",ifelse(db %in% "Oracle",paste0(oracle.user,"."),""),table_name))
    # Define the SQL statement to create the table
    sql_statement <- paste0("
    CREATE TABLE ",table_name," (
    NAME VARCHAR2(100),
    CIVIC VARCHAR2(50),
    TOWN VARCHAR2(100),
    PROV VARCHAR2(50),
    COUNTRY VARCHAR2(100),
    POST VARCHAR2(20),
    EMAIL VARCHAR2(100),
    PHO1 VARCHAR2(20),
    PHO2 VARCHAR2(20),
    AFFILIATION VARCHAR2(100),
    LICENSE_AREA VARCHAR2(100)
)")

      # Execute the SQL statement
      dbSendQuery(con, sql_statement)

  }
    # Close the connection
    dbDisconnect(con)

####################################################################################################
######################################################################## Main Function:
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

        # Placeholder for no release error message
        uiOutput("release_error"),

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
            numericInput("long_deg", "Longitude Degrees:", value = NULL, min = -180, max = 0)),

        # Longitude format reminder message:
        uiOutput("reminder"),

        # Input: Longitude Decimal Minutes (Mandatory)
        div(class = "mandatory-field",
            numericInput("long_dec_min", "Longitude Decimal Minutes:", value = NULL, min = 0, max = 59.99, step = 0.01)),

        # Input: Depth Fathoms
        numericInput("depth_fathoms", "Depth (fathoms):", value = NULL, min = 0),

        # Input: Released
        selectInput("released", "Released?",choices = c("","yes", "no", "unknown")),

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

    # Connect to database
    if(db %in% "Oracle"){
      tryCatch({
        drv <- DBI::dbDriver("Oracle")
        con <- ROracle::dbConnect(drv, username = oracle.user, password = oracle.password, dbname = oracle.dbname)
      }, warning = function(w) {
      }, error = function(e) {
        return(toJSON("Connection failed"))
      }, finally = {
      })
    }else{con <- dbConnect(RSQLite::SQLite(), "C:/LOBTAG/LOBTAG.db")}

    # Observer to check if Tag Prefix and Tag Number combination exists in the Oracle table
    observeEvent(c(input$tag_prefix, input$tag_number), {
      if (nchar(input$tag_prefix) > 0 & is.numeric(input$tag_number)) {
        query <- paste("SELECT * FROM LBT_RELEASES WHERE TAG_PREFIX = '", input$tag_prefix, "' AND TAG_NUM = ", input$tag_number, sep = "")
        result <- dbGetQuery(con, query)
        if (nrow(result) == 0) {
          # Update style and display warning message if combination not found
          shinyjs::addClass("tag_number", "error-input")
          shinyjs::addClass("tag_prefix", "error-input")
          output$release_error <- renderText({
            HTML("<span style='color: red;'>No release data found for this tag!</span>")
          })
        } else {
          # Remove error style and message if combination found
          shinyjs::removeClass("tag_number", "error-input")
          shinyjs::removeClass("tag_prefix", "error-input")
          output$release_error <- renderText({""})}
      }

    })

    ## conditional reminder if user enters positive longitude
    observeEvent(c(input$long_deg), {
      if(is.numeric(input$long_deg)){
        if(input$long_deg > 0){
          output$reminder <- renderText({
            HTML("<span style='color: red;'>Careful! Degrees Longitude must be negative for western hemisphere!</span>")
          })
        }else{output$reminder <- renderText({""})}
      }
    })

    # Real-time data retrieval based on "Person" field input
    observeEvent(input$person, {
      if (nchar(input$person) > 0) {
        query <- paste("SELECT CIVIC, TOWN, PROV, COUNTRY, POST, EMAIL, PHO1, PHO2, AFFILIATION, LICENSE_AREA FROM LBT_PEOPLE WHERE NAME = '", input$person, "'", sep = "")
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

    # Update table when Submit button is clicked
    observeEvent(input$submit, {

      # Get input values
      tag_prefix <- input$tag_prefix
      tag_number <- input$tag_number
      date <- format(input$date, "%Y-%m-%d")  # Format date for SQLite
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
      if(as.numeric(long_deg)<0){
        longitude_dddd <- as.numeric(long_deg) - (as.numeric(long_dec_min) / 60)
      }else{longitude_dddd <- as.numeric(long_deg) + (as.numeric(long_dec_min) / 60)}



      # Check if selected and numeric (non-mandatory) fields are empty and handle by making null
      # selected values
      released <- ifelse(input$released == "", "NULL", input$released)
      sex <- ifelse(input$sex == "", "NULL", input$sex)
      egg_state <- ifelse(input$egg_state == "", "NULL", input$egg_state)
      # numeric values
      capture_length <- ifelse(is.na(input$capture_length), "NULL", input$capture_length)
      depth_fathoms <- ifelse(is.na(input$depth_fathoms), "NULL", input$depth_fathoms)

      # Generate TAG_ID
      tag_id <- paste(tag_prefix, tag_number, sep = "")

      # Prepare SQL insert statement
      if(db %in% "Oracle"){
        sql_1 <- paste("INSERT INTO LBT_RECAPTURES (Tag_Prefix, Tag_Number, TAG_ID, REC_DATE, PERSON, PERSON_2, LAT_DEGREE, LAT_MINUTE, LON_DEGREE, LON_MINUTE, LAT_DD, LON_DD, FATHOMS, RELEASED, CAPTAIN, VESSEL, YEAR, MANAGEMENT_AREA, CAPTURE_LENGTH, SEX, EGG_STATE, REWARDED, COMMENTS) VALUES ('", tag_prefix, "', ", tag_number, ", '", tag_id, "', '", date, "', '", person, "', '", person_2, "', ", lat_deg, ", ", lat_dec_min, ", ", long_deg, ", ", long_dec_min, ", ", latitude_dddd, ", ", longitude_dddd, ", ", depth_fathoms, ", '", released, "', '", input$captain, "', '", input$vessel, "', EXTRACT(YEAR FROM TO_DATE('", date, "', 'YYYY/MM/DD')), '", input$management_area, "', ", capture_length, ", ", sex, ", ", egg_state, ", 'no', '", input$comments, "')", sep="")
      }else{sql_1 <- paste0(
        "INSERT INTO LBT_RECAPTURES (Tag_Prefix, Tag_Number, TAG_ID, REC_DATE, PERSON, PERSON_2, LAT_DEGREE, LAT_MINUTE, LON_DEGREE, LON_MINUTE, LAT_DD, LON_DD, FATHOMS, RELEASED, CAPTAIN, VESSEL, YEAR, MANAGEMENT_AREA, CAPTURE_LENGTH, SEX, EGG_STATE, REWARDED, COMMENTS) ",
        "VALUES ('", tag_prefix, "', ", tag_number, ", '", tag_id, "', '", date, "', '", person, "', '", person_2, "', ",
        lat_deg, ", ", lat_dec_min, ", ", long_deg, ", ", long_dec_min, ", ", latitude_dddd, ", ", longitude_dddd, ", ",
        depth_fathoms, ", '", released, "', '", input$captain, "', '", input$vessel, "', strftime('%Y', '", date, "'), '",
        input$management_area, "', ", capture_length, ", ", sex, ", ", egg_state, ", 'no', '", input$comments, "')"
      )}

      # Execute SQL insert statement
      dbExecute(con, sql_1)

      # Prepare SQL update/insert statement for the LBT_PEOPLE table and insert data
      if (nchar(input$person) > 0){
        ## MERGE querying works differently with SQLite and Oracle
        if(db %in% "Oracle"){
          sql_2 <- paste("
      MERGE INTO LBT_PEOPLE tgt
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
          #dbCommit(con)

        }else{
          # Check if the person already exists in the LBT_PEOPLE table
          check_query <- paste("
  SELECT COUNT(*) as count
  FROM LBT_PEOPLE
  WHERE NAME = '", input$person, "'", sep = "")

          # Execute the check query
          res <- dbGetQuery(con, check_query)

          # If the person exists, update their information
          if (res$count > 0) {
            update_query <- paste("
    UPDATE LBT_PEOPLE
    SET CIVIC = '", input$street_address, "',
        TOWN = '", input$town, "',
        PROV = '", input$province, "',
        COUNTRY = '", input$country, "',
        POST = '", input$postal_code, "',
        EMAIL = '", input$email, "',
        PHO1 = '", input$phone1, "',
        PHO2 = '", input$phone2, "',
        AFFILIATION = '", input$affiliation, "',
        LICENSE_AREA = '", input$license_area, "'
    WHERE NAME = '", input$person, "'", sep = "")

            dbExecute(con, update_query)
          } else {
            # If the person does not exist, insert their information
            insert_query <- paste("
    INSERT INTO LBT_PEOPLE
    (NAME, CIVIC, TOWN, PROV, COUNTRY, POST, EMAIL, PHO1, PHO2, AFFILIATION, LICENSE_AREA)
    VALUES
    ('", input$person, "', '", input$street_address, "', '", input$town, "', '", input$province, "', '", input$country, "', '", input$postal_code, "', '", input$email, "', '", input$phone1, "', '", input$phone2, "', '", input$affiliation, "', '", input$license_area, "')", sep = "")

            dbExecute(con, insert_query)
          }

        }

      }


      # Update status
      output$status <- renderText({
        "Tags to be uploaded to database. Close this window to complete upload."
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
      shinyjs::reset("released")
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
#' @import dplyr ROracle DBI shiny DT svDialogs readxl
#' @description batch uploads tag recaptures data
#' @export
batch_upload_recaptures <- function(db = "local",oracle.user = oracle.personal.user, oracle.password = oracle.personal.password, oracle.dbname = oracle.personal.server){

  ## Check if recaptures and people tables already exist and create if not

  # Connect to database
  if(db %in% "Oracle"){
    tryCatch({
      drv <- DBI::dbDriver("Oracle")
      con <- ROracle::dbConnect(drv, username = oracle.user, password = oracle.password, dbname = oracle.dbname)
    }, warning = function(w) {
    }, error = function(e) {
      return(toJSON("Connection failed"))
    }, finally = {
    })
  }else{
    dir.create("C:/LOBTAG", showWarnings = F)
    con <- dbConnect(RSQLite::SQLite(), "C:/LOBTAG/LOBTAG.db")
    }

  table_name <- "LBT_RECAPTURES"

  ## look for existing table
  if(db %in% "Oracle"){
    query <- paste("SELECT COUNT(*) FROM user_tables WHERE table_name = '", table_name, "'", sep = "")
  }else{query <- paste("SELECT COUNT(*) AS table_count FROM sqlite_master WHERE type='table' AND name='", table_name, "'", sep = "")}

  result <- dbGetQuery(con, query)
  # If the table does not exist, create it
  if (result[[1]] == 0) {
    print(paste0("Creating new table called: ",ifelse(db %in% "Oracle",paste0(oracle.user,"."),""),table_name))
    # Define the SQL statement to create the table
    sql_statement <- paste0("
    CREATE TABLE ",table_name," (
    TAG_PREFIX VARCHAR2(50),
    TAG_NUMBER VARCHAR2(50),
    TAG_ID VARCHAR2(50),
    REC_DATE VARCHAR2(50),
    PERSON VARCHAR2(100),
    PERSON_2 VARCHAR2(100),
    LAT_DEGREE VARCHAR2(50),
    LAT_MINUTE VARCHAR2(50),
    LON_DEGREE VARCHAR2(50),
    LON_MINUTE VARCHAR2(50),
    LAT_DD VARCHAR2(50),
    LON_DD VARCHAR2(50),
    FATHOMS VARCHAR2(50),
    RELEASED VARCHAR2(50),
    CAPTAIN VARCHAR2(100),
    VESSEL VARCHAR2(100),
    YEAR VARCHAR2(50),
    MANAGEMENT_AREA VARCHAR2(100),
    CAPTURE_LENGTH VARCHAR2(50),
    SEX VARCHAR2(10),
    EGG_STATE VARCHAR2(50),
    REWARDED VARCHAR2(10),
    COMMENTS VARCHAR2(1000)
)")

    # Execute the SQL statement
    dbSendQuery(con, sql_statement)

  }

  table_name <- "LBT_PEOPLE"

  ## look for existing table
  if(db %in% "Oracle"){
    query <- paste("SELECT COUNT(*) FROM user_tables WHERE table_name = '", table_name, "'", sep = "")
  }else{query <- paste("SELECT COUNT(*) AS table_count FROM sqlite_master WHERE type='table' AND name='", table_name, "'", sep = "")}

  result <- dbGetQuery(con, query)
  # If the table does not exist, create it
  if (result[[1]] == 0) {
    print(paste0("Creating new table called: ",ifelse(db %in% "Oracle",paste0(oracle.user,"."),""),table_name))
    # Define the SQL statement to create the table
    sql_statement <- paste0("
    CREATE TABLE ",table_name," (
    NAME VARCHAR2(100),
    CIVIC VARCHAR2(50),
    TOWN VARCHAR2(100),
    PROV VARCHAR2(50),
    COUNTRY VARCHAR2(100),
    POST VARCHAR2(20),
    EMAIL VARCHAR2(100),
    PHO1 VARCHAR2(20),
    PHO2 VARCHAR2(20),
    AFFILIATION VARCHAR2(100),
    LICENSE_AREA VARCHAR2(100)
)")

    # Execute the SQL statement
    dbSendQuery(con, sql_statement)

  }
  # Close the connection
  dbDisconnect(con)

####################################################################################################################
################################################################################################# MAIN FUNCTION:

  ## Allow user to choose data file to upload
  dlg_message("In the following window, choose a csv file containing your recaptures data")
  file_path <- dlg_open(filter = dlg_filters["xls",])$res
  recaptures <- read_xlsx(file_path, na = c("","NA"))
  #recaptures <- read.csv(file_path, na.strings = c("","NA"))
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

  ## some pre-coordinate formatting error checking
  bad_lat.deg = which(!is.na(rec$LAT_DEGREE) & (nchar(as.character(rec$LAT_DEGREE))<2 | as.numeric(rec$LAT_DEGREE) %in% NA) ) ## these error check degrees and minutes, regardless of whether they get used to generate DD coordinates
  bad_lat.min = which(!is.na(rec$LAT_MINUTE) & (nchar(as.character(rec$LAT_MINUTE))<2 | as.numeric(rec$LAT_MINUTE) %in% NA) )
  bad_lon.deg = which(!is.na(rec$LON_DEGREE) & (nchar(as.character(rec$LON_DEGREE))<2 | as.numeric(rec$LON_DEGREE) %in% NA) )
  bad_lon.min = which(!is.na(rec$LON_MINUTE) & (nchar(as.character(rec$LON_MINUTE))<2 | as.numeric(rec$LON_MINUTE) %in% NA) )

  ##ccordinate decimal degrees and degrees minutes formatting done here
  ## account for negative degrees
  rec$LAT_DEGREE = as.numeric(rec$LAT_DEGREE)
  rec$LAT_MINUTE = as.numeric(rec$LAT_MINUTE)
  rec$LON_DEGREE = as.numeric(rec$LON_DEGREE)
  rec$LON_MINUTE = as.numeric(rec$LON_MINUTE)
  rec$LAT_DD = as.numeric(rec$LAT_DD)
  rec$LON_DD = as.numeric(rec$LON_DD)
## ultimately will use DD format for error checking, can back fill DEgree nd Minute columns from this if they're missing
  ## latitude

  for(i in 1:nrow(rec)){
    if(is.na(rec$LAT_DD[i])){
      if(!is.na(rec$LAT_DEGREE[i]) & !is.na(rec$LAT_MINUTE[i]) & is.numeric(rec$LAT_DEGREE[i]) &
         is.numeric(rec$LAT_MINUTE[i])){
        if(rec$LAT_DEGREE[i]<0){
        #  rec$LATDDMM_MM[i] = rec$LAT_DEGREE[i] * 100 - rec$LAT_MINUTE[i]
          rec$LAT_DD[i] = rec$LAT_DEGREE[i] - rec$LAT_MINUTE[i] / 60
        }else{
        #  rec$LATDDMM_MM[i] = rec$LAT_DEGREE[i] * 100 + rec$LAT_MINUTE[i]
          rec$LAT_DD[i] = rec$LAT_DEGREE[i] + rec$LAT_MINUTE[i] / 60
        }
      }
    }

  }
  ## longitude
  for(i in 1:nrow(rec)){
    if(is.na(rec$LON_DD[i])){
      if(!is.na(rec$LON_DEGREE[i]) & !is.na(rec$LON_MINUTE[i]) & is.numeric(rec$LON_DEGREE[i]) &
         is.numeric(rec$LON_MINUTE[i])){
        if(rec$LON_DEGREE[i]<0){
         # rec$LONDDMM_MM[i] = rec$LON_DEGREE[i] * 100 - rec$LON_MINUTE[i]
          rec$LON_DD[i] = rec$LON_DEGREE[i] - rec$LON_MINUTE[i] / 60
        }else{
         # rec$LONDDMM_MM[i] = rec$LON_DEGREE[i] * 100 + rec$LON_MINUTE[i]
          rec$LON_DD[i] = rec$LON_DEGREE[i] + rec$LON_MINUTE[i] / 60
        }
      }
    }
  }


  rec <- rec %>% mutate(TAG_ID = paste0(TAG_PREFIX,as.character(TAG_NUMBER)))
  rec <- rec %>% mutate(REC_DATE = paste(DAY,MONTH,YEAR, sep="/"))
  rec$REC_DATE = format(as.Date(rec$REC_DATE, format = "%d/%m/%Y"), "%Y-%m-%d")
  ## clean columns that tend to have problematic characters
  rec$COMMENTS = gsub("'","",rec$COMMENTS)
  rec$VESSEL = gsub("'","",rec$VESSEL)
  rec$TOWN = gsub("'","",rec$TOWN)
  rec$CIVIC = gsub("'","",rec$CIVIC)
  rec$CIVIC = gsub("#"," ",rec$CIVIC)


  ## error checking (General):
  bad_tag_pre = which(rec$TAG_PREFIX %in% NA)
  bad_tag_num = which(rec$TAG_NUMBER %in% NA | as.numeric(rec$TAG_NUMBER) %in% NA)

  bad_lat = which(rec$LAT_DD %in% NA | nchar(as.character(rec$LAT_DD))<2 | !is.numeric(rec$LAT_DD))
  bad_lon = which(rec$LON_DD %in% NA | nchar(as.character(rec$LON_DD))<2 | !is.numeric(rec$LON_DD))
  sus_lon = which(rec$LON_DD>0) ## suspect longitudes not in the Western hemisphere
  neg_lat.min = which(!is.na(rec$LAT_MINUTE) & rec$LAT_MINUTE<0)
  neg_lon.min = which(!is.na(rec$LON_MINUTE) & rec$LON_MINUTE<0)
  bad_date = which(rec$REL_DATE %in% NA)

  ## check if any recapture events seems to be replicates
  rec <- rec %>% mutate(reps = paste(TAG_ID,REC_DATE,LAT_DD,LON_DD))
  repeat_event = which(duplicated(rec$reps)==TRUE)

  error_out= ""
  error_tab = NULL
  warning_out= ""
  warning_tab = NULL
  return_error = FALSE
  return_warning = FALSE
################################ errors
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
  if(length(bad_lat.deg) > 0){
    for(i in bad_lat.deg){
      error_out = paste(error_out, "\nBad latitude degrees for tag:",rec$TAG_ID[i],"at row:",i)
      error_tab = rbind(error_tab,c(i,rec$TAG_PREFIX[i],rec$TAG_NUMBER[i],"Bad latitude degrees"))
    }
    return_error = TRUE
  }
  if(length(bad_lat.min) > 0){
    for(i in bad_lat.min){
      error_out = paste(error_out, "\nBad latitude minutes for tag:",rec$TAG_ID[i],"at row:",i)
      error_tab = rbind(error_tab,c(i,rec$TAG_PREFIX[i],rec$TAG_NUMBER[i],"Bad latitude minutes"))
    }
    return_error = TRUE
  }
  if(length(bad_lon.deg) > 0){
    for(i in bad_lon.deg){
      error_out = paste(error_out, "\nBad longitude degrees for tag:",rec$TAG_ID[i],"at row:",i)
      error_tab = rbind(error_tab,c(i,rec$TAG_PREFIX[i],rec$TAG_NUMBER[i],"Bad longitude degrees"))
    }
    return_error = TRUE
  }
  if(length(bad_lon.min) > 0){
    for(i in bad_lon.min){
      error_out = paste(error_out, "\nBad longitude minutes for tag:",rec$TAG_ID[i],"at row:",i)
      error_tab = rbind(error_tab,c(i,rec$TAG_PREFIX[i],rec$TAG_NUMBER[i],"Bad longitude minutes"))
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
  if(length(neg_lat.min) > 0){
    for(i in neg_lat.min){
      error_out = paste(error_out, "\nNegative Latitude Minutes found for tag:", rec$TAG_ID[i],"at row:",i,"These should never be negative.")
      error_tab = rbind(error_tab,c(i,rec$TAG_PREFIX[i],rec$TAG_NUMBER[i],"Negative Latitude Minutes"))
    }
    return_error = TRUE
  }
  if(length(neg_lon.min) > 0){
    for(i in neg_lon.min){
      error_out = paste(error_out, "\nNegative Longitude Minutes found for tag:", rec$TAG_ID[i],"at row:",i,"These should never be negative.")
      error_tab = rbind(error_tab,c(i,rec$TAG_PREFIX[i],rec$TAG_NUMBER[i],"Negative Longitude Minutes"))
    }
    return_error = TRUE
  }
  ########################## Warnings
  if(length(sus_lon)>0){
    for(i in sus_lon){
      warning_out = paste(warning_out, "\nSuspicious positive longitudes (should be negative for Western hemisphere) for tags:", rec$TAG_ID[i],"at row:",i)
      warning_tab = rbind(warning_tab,c(i,rec$TAG_PREFIX[i],rec$TAG_NUMBER[i],"Suspicious positive longitude"))
    }
    return_warning = TRUE
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
        downloadButton("download_table", label = "Download Error Table")
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

  if(!return_error & return_warning){
    colnames(warning_tab)=c("Row","Tag Prefix","Tag Number","Warning")
    ## Create interactive dialogue showing uploading errors and giving user option to download these in a table
    # Define the UI
    ui <- fluidPage(
      titlePanel("Uploading Warnings"),
      mainPanel(
        uiOutput("dynamicUI")

      )
    )

    # Define the server logic
    server <- function(input, output) {
############################################# Dialogue 1
      # Initial content in the main panel
      output$dynamicUI <- renderUI({
        h3("Check issues below before proceeding with upload:")
      })
      output$dynamicUI <- renderUI({
        h4(verbatimTextOutput("text_output"),
           # Button to download the table
           downloadButton("download_table", label = "Download Warning Table"),
           # Button to proceed with upload
           actionButton(inputId = "upload_table", label = "Ignore Warnings and Upload Data")
        )
      })
      # Display the text from the string variable
      output$text_output <- renderText({
        warning_out
      })

      # Function to generate a downloadable file
      output$download_table <- downloadHandler(
        filename = function() {
          "recaptures_uploading_warnings.csv"
        },
        content = function(file) {
          write.csv(warning_tab, file,row.names = F)
        }
      )


    observeEvent(input$upload_table,{
      ###### Database UPLOAD HERE. Check that entry doesn't already exist before uploading

      table_name <- "LBT_RECAPTURES"
      people.tab.name <- "LBT_PEOPLE"
      # Connect to database
      if(db %in% "Oracle"){
        tryCatch({
          drv <- DBI::dbDriver("Oracle")
          con <- ROracle::dbConnect(drv, username = oracle.user, password = oracle.password, dbname = oracle.dbname)
        }, warning = function(w) {
        }, error = function(e) {
          return(toJSON("Connection failed"))
        }, finally = {
        })
      }else{con <- dbConnect(RSQLite::SQLite(), "C:/LOBTAG/LOBTAG.db")}

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
          sql <- paste("INSERT INTO ",table_name, " VALUES ('",rec$TAG_PREFIX[i],"', '",rec$TAG_NUMBER[i],"', '",rec$TAG_ID[i],"', '",rec$REC_DATE[i],"','",rec$PERSON[i],"','",rec$PERSON_2[i],"','",rec$LAT_DEGREE[i],"','",rec$LAT_MINUTE[i],"','",rec$LON_DEGREE[i],"','",rec$LON_MINUTE[i],"','",rec$LAT_DD[i],"','",rec$LON_DD[i],"','",rec$FATHOMS[i],"','",rec$RELEASED[i],"','",rec$CAPTAIN[i],"','",rec$VESSEL[i],"','",rec$YEAR[i],"','",rec$MANAGEMENT_AREA[i],"','",rec$CAPTURE_LENGTH[i],"','",rec$SEX[i],"','",rec$EGG_STATE[i],"','","no","','",rec$COMMENTS[i],"')", sep = "")
          if(db %in% "local"){dbBegin(con)}
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
          if(db %in% "local"){dbBegin(con)}
          result <- dbSendQuery(con, sql)
          dbCommit(con)
          dbClearResult(result)
        }

      }

      dbDisconnect(con)

      ### show interactive info window if there were any tags found to be already entered
      if(nrow(entered)>0){
        # Define the UI
        ui <- fluidPage(
          titlePanel("Uploading Warnings"),
          mainPanel(
            uiOutput("dynamicUI")

          )
        )
        # Dynamically render new UI elements
        output$dynamicUI <- renderUI({
          fluidPage(
          tags$br(),
          h4("Upload Success! Recaptures for the following tags by the same person on the same day were found already in the database, these are assumed to be the same event so were not uploaded:"),
          sidebarLayout(
            sidebarPanel(
              # Text box to display all TAG_NUM values
              textOutput("tag_values")
            ),

            mainPanel(
              # Display table based on selection
              DTOutput("table"),

              # Download button
              downloadButton("download_table", "Download Existing Recaptures Table")
            )
          )
        )
        })

        # Define server logic

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
              paste("existing_recaptures_table", ".csv", sep = "")
            },
            content = function(file) {
              write.csv(entered, file, row.names = FALSE)
            }
          )


      }else{
        output$dynamicUI <- renderUI({
          fluidPage(
            h3("All recaptures uploaded successfully without errors! Close this window.")
          ) })
      }

    }) ## observ event

    } ## serv logic
    # Run the application
    return(shinyApp(ui = ui, server = server))
  }

  if(!return_error & !return_warning){
    ###### Database UPLOAD HERE. Check that entry doesn't already exist before uploading

    table_name <- "LBT_RECAPTURES"
    people.tab.name <- "LBT_PEOPLE"
    # Connect to database
    if(db %in% "Oracle"){
      tryCatch({
        drv <- DBI::dbDriver("Oracle")
        con <- ROracle::dbConnect(drv, username = oracle.user, password = oracle.password, dbname = oracle.dbname)
      }, warning = function(w) {
      }, error = function(e) {
        return(toJSON("Connection failed"))
      }, finally = {
      })
    }else{con <- dbConnect(RSQLite::SQLite(), "C:/LOBTAG/LOBTAG.db")}

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
        sql <- paste("INSERT INTO ",table_name, " VALUES ('",rec$TAG_PREFIX[i],"', '",rec$TAG_NUMBER[i],"', '",rec$TAG_ID[i],"', '",rec$REC_DATE[i],"','",rec$PERSON[i],"','",rec$PERSON_2[i],"','",rec$LAT_DEGREE[i],"','",rec$LAT_MINUTE[i],"','",rec$LON_DEGREE[i],"','",rec$LON_MINUTE[i],"','",rec$LAT_DD[i],"','",rec$LON_DD[i],"','",rec$FATHOMS[i],"','",rec$RELEASED[i],"','",rec$CAPTAIN[i],"','",rec$VESSEL[i],"','",rec$YEAR[i],"','",rec$MANAGEMENT_AREA[i],"','",rec$CAPTURE_LENGTH[i],"','",rec$SEX[i],"','",rec$EGG_STATE[i],"','","no","','",rec$COMMENTS[i],"')", sep = "")
        if(db %in% "local"){dbBegin(con)}
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
        if(db %in% "local"){dbBegin(con)}
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
        h4("Recaptures for the following tags by the same person on the same day were found already in the database, these are assumed to be the same event so were not uploaded:"),
        sidebarLayout(
          sidebarPanel(
            # Text box to display all TAG_NUM values
            textOutput("tag_values")
          ),

          mainPanel(
            # Display table based on selection
            DTOutput("table"),

            # Download button
            downloadButton("download_table", "Download Existing Recaptures Table")
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
            paste("existing_recaptures_table", ".csv", sep = "")
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

