#' @title upload_releases
#' @import dplyr ROracle RSQLite DBI shiny DT svDialogs
#' @description batch uploads tag release data
#' @export

upload_releases <- function(db = "local",oracle.user = oracle.personal.user, oracle.password = oracle.personal.password, oracle.dbname = oracle.personal.server){


  # Check if releases table already exists and create if not
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


  table_name <- "LBT_RELEASES"

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
    SAMPLER VARCHAR2(100),
    SAMPLER_2 VARCHAR2(100),
    AFFILIATION VARCHAR2(100),
    VESSEL VARCHAR2(100),
    CAPTAIN VARCHAR2(100),
    PORT VARCHAR2(100),
    LFA VARCHAR2(50),
    DAY VARCHAR2(50),
    MONTH VARCHAR2(50),
    YEAR VARCHAR2(50),
    TAG_COLOR VARCHAR2(50),
    TAG_PREFIX VARCHAR2(50),
    TAG_NUM VARCHAR2(50),
    TAG_ID VARCHAR2(50),
    CARAPACE_LENGTH VARCHAR2(50),
    SEX VARCHAR2(10),
    SHELL VARCHAR2(10),
    CLAW VARCHAR2(10),
    LAT_DEGREES VARCHAR2(50),
    LAT_MINUTES VARCHAR2(50),
    LON_DEGREES VARCHAR2(50),
    LON_MINUTES VARCHAR2(50),
    LATDDMM_MM VARCHAR2(20),
    LONDDMM_MM VARCHAR2(20),
    LAT_DD VARCHAR2(50),
    LON_DD VARCHAR2(50),
    REL_DATE VARCHAR2(50),
    COMMENTS VARCHAR2(1000)
)")

    # Execute the SQL statement
    dbSendQuery(con, sql_statement)

  }
  # Close the connection
  dbDisconnect(con)

####################################################################################################

## Allow user to choose data file to upload
dlg_message("In the following window, choose a csv file containing your releases data")
file_path <- dlg_open(filter = dlg_filters["csv",])$res
releases <- read.csv(file_path, na.strings = "")
## Process / standardize the data table

##decimal degrees and degrees minutes formatting done here
releases$LATDDMM_MM = releases$LAT_DEGREES * 100 + releases$LAT_MINUTES
releases$LAT_DD = releases$LAT_DEGREES + releases$LAT_MINUTES / 60
## account for negative longitudes
for(i in 1:nrow(releases)){
  if(releases$LON_DEGREES[i]<0){
    releases$LONDDMM_MM[i] = releases$LON_DEGREES[i] * 100 - releases$LON_MINUTES[i]
    releases$LON_DD[i] = releases$LON_DEGREES[i] - releases$LON_MINUTES[i] / 60
  }else{
    releases$LONDDMM_MM[i] = releases$LON_DEGREES[i] * 100 + releases$LON_MINUTES[i]
    releases$LON_DD[i] = releases$LON_DEGREES[i] + releases$LON_MINUTES[i] / 60
    }
}

##date column isn't 100% necessary but it's a good indication if things are going wrong
releases$REL_DATE = paste(releases$DAY, releases$MONTH, releases$YEAR, sep = "/")
releases$REL_DATE = format(as.Date(releases$REL_DATE, format = "%d/%m/%Y"), "%Y-%m-%d")
releases$TAG_ID = paste0(releases$TAG_PREFIX,releases$TAG_NUM)

## retrieve only selected variables if there are extra / differently ordered columns
select.names = c("SAMPLER"	,"SAMPLER_2",	"AFFILIATION","VESSEL",	"CAPTAIN","PORT",	"MANAGEMENT_AREA",	"DAY",	"MONTH",	"YEAR",	"TAG_COLOR",	"TAG_PREFIX",	"TAG_NUM", "TAG_ID", "CARAPACE_LENGTH",	"SEX",	"SHELL",	"CLAW",	"LAT_DEGREES",	"LAT_MINUTES",	"LON_DEGREES",	"LON_MINUTES", "LATDDMM_MM","LONDDMM_MM","LAT_DD","LON_DD","REL_DATE","COMMENTS")

rel <- dplyr::select(releases,(all_of(select.names)))
## clean variables for problematic characters
rel$VESSEL = gsub("'","",rel$VESSEL)
rel$PORT = gsub("'","",rel$PORT)
#\ rel$PORT = gsub("\\(","-",rel$PORT)
# rel$PORT = gsub("\\)","-",rel$PORT)

## error checking (Lobster specific, edit for other species / tagging programs):
# sex_values <- c(NA,0,1,2,3)
# shell_values <- c(NA, 1:7)
# claw_values <- c(NA,1,2,3)
# vnotch_values <- c(NA,"YES","NO")
# carapace_values <- c(NA, 40:150) #this one will alert the user but continue with upload.
# carapace_values_fsrs <- c(NA, 40:170) # fsrs samples some really big females as part of the v-notch program, so increase the threshold in this case.

########### Testing
# rel[1,26]= NA
#  rel[2,12]=NA
#  rel[3,12]=NA
#  rel[4,12]=NA
#  rel[5,13]=NA
# rel <- rbind(rel,rel[1,])
# rel <- rbind(rel,rel[1,])
# rel[6,27]=NA
# rel[7,26]=NA


## error checking (General):

bad_tag_pre = which(rel$TAG_PREFIX %in% NA)
bad_tag_num = which(rel$TAG_NUM %in% NA | as.numeric(rel$TAG_NUM) %in% NA)
repeat_tags = which(duplicated(rel$TAG_ID)==TRUE)

bad_lat = which(rel$LAT_DD %in% NA | nchar(as.character(rel$LAT_DD))<2 | !is.numeric(rel$LAT_DD))
bad_lon = which(rel$LON_DD %in% NA | nchar(as.character(rel$LON_DD))<2 | !is.numeric(rel$LON_DD))
sus_lon = which(rel$LON_DD>0) ## suspect longitudes not in the Western hemisphere
bad_date = which(rel$REL_DATE %in% NA)


error_out= ""
error_tab = NULL
return_error = FALSE
return_warning = FALSE

if(length(bad_tag_pre) > 0){
  for(i in bad_tag_pre){
    error_out = paste(error_out, "\nMissing tag prefix for tag number:",rel$TAG_NUM[i],"at row:",i)
    error_tab = rbind(error_tab,c(i,rel$TAG_PREFIX[i],rel$TAG_NUM[i],"Missing tag prefix"))
  }
  return_error = TRUE
}
if(length(bad_tag_num) > 0){
  for(i in bad_tag_num){
    error_out = paste(error_out, "\nBad or missing tag number at row:", i)
    error_tab = rbind(error_tab,c(i,rel$TAG_PREFIX[i],rel$TAG_NUM[i],"Bad or missing tag number"))
  }
  return_error = TRUE
}
if(length(repeat_tags) > 0){
  for(i in repeat_tags){
    error_out = paste(error_out, "\nDuplicate tag:",rel$TAG_ID[i],"at row:",i)
    error_tab = rbind(error_tab,c(i,rel$TAG_PREFIX[i],rel$TAG_NUM[i],"Duplicate tag prefix and number"))
  }
  return_error = TRUE
}
if(length(bad_lat) > 0){
  for(i in bad_lat){
    error_out = paste(error_out, "\nBad or missing latitude for tag:",rel$TAG_ID[i],"at row:",i)
    error_tab = rbind(error_tab,c(i,rel$TAG_PREFIX[i],rel$TAG_NUM[i],"Bad or missing latitude"))
  }
  return_error = TRUE
}
if(length(bad_lon) > 0){
  for(i in bad_lon){
    error_out = paste(error_out, "\nBad or missing longitude for tag:",rel$TAG_ID[i],"at row:",i)
    error_tab = rbind(error_tab,c(i,rel$TAG_PREFIX[i],rel$TAG_NUM[i],"Bad or missing longitude"))
  }
  return_error = TRUE
}
if(length(bad_date) > 0){
  for(i in bad_date){
    error_out = paste(error_out, "\nBad or missing date for tag:", rel$TAG_ID[i],"at row:",i)
    error_tab = rbind(error_tab,c(i,rel$TAG_PREFIX[i],rel$TAG_NUM[i],"Bad or missing date"))
  }
  return_error = TRUE
}
if(length(sus_lon)>0){
  for(i in sus_lon){
    error_out = paste(error_out, "\nSuspicious positive longitudes (should be negative for Western hemisphere) for tags:", rel$TAG_ID[i],"at row:",i)
    error_tab = rbind(error_tab,c(i,rel$TAG_PREFIX[i],rel$TAG_NUM[i],"Suspicious positive longitude"))
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
        "releases_uploading_errors.csv"
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
    colnames(error_tab)=c("Row","Tag Prefix","Tag Number","Warning")
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
        error_out
      })

      # Function to generate a downloadable file
      output$download_table <- downloadHandler(
        filename = function() {
          "releases_uploading_warnings.csv"
        },
        content = function(file) {
          write.csv(error_tab, file,row.names = F)
        }
      )

      observeEvent(input$upload_table,{

        ###### db UPLOAD HERE. Check that entry doesn't already exist before uploading

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
        }else{con <- dbConnect(RSQLite::SQLite(), "C:/LOBTAG/LOBTAG.db")}

        ## check for already entered tags, then upload all new tag entries
        entered =NULL
        for(i in 1:nrow(rel)){
          sql <- paste0("SELECT * FROM ",table_name," WHERE TAG_ID= '",rel$TAG_ID[i],"'")
          check <- dbSendQuery(con, sql)
          existing_tag <- dbFetch(check)
          entered <- rbind(entered,existing_tag)
          dbClearResult(check)

          if(nrow(existing_tag)==0){
            sql <- paste("INSERT INTO ",table_name, " VALUES ('",rel$SAMPLER[i],"', '",rel$SAMPLER_2[i],"', '",rel$AFFILIATION[i],"', '",rel$VESSEL[i],"','",rel$CAPTAIN[i],"','",rel$PORT[i],"','",rel$MANAGEMENT_AREA[i],"','",rel$DAY[i],"','",rel$MONTH[i],"','",rel$YEAR[i],"','",rel$TAG_COLOR[i],"','",rel$TAG_PREFIX[i],"','",rel$TAG_NUM[i],"','",rel$TAG_ID[i],"','",rel$CARAPACE_LENGTH[i],"','",rel$SEX[i],"','",rel$SHELL[i],"','",rel$CLAW[i],"','",rel$LAT_DEGREES[i],"','",rel$LAT_MINUTES[i],"','",rel$LON_DEGREES[i],"','",rel$LON_MINUTES[i],"','",rel$LATDDMM_MM[i],"','",rel$LONDDMM_MM[i],"','",rel$LAT_DD[i],"','",rel$LON_DD[i],"','",rel$REL_DATE[i],"','",rel$COMMENTS[i],"')", sep = "")
            if(db %in% "local"){dbBegin(con)}
            result <- dbSendQuery(con, sql)
            dbCommit(con)
            dbClearResult(result)
          }

        }

        dbDisconnect(con)

        ### show interactive info window if there were any tags found to be already entered
        if(nrow(entered)>0){
          # Dynamically render new UI elements
          output$dynamicUI <- renderUI({
            fluidPage(
            tags$br(),
            h3("Upload Success! The following tags already exist in the database so were not uploaded:"),
            sidebarLayout(
              sidebarPanel(
                # Text box to display all TAG_NUM values
                textOutput("tag_values")
              ),

              mainPanel(
                # Display table based on selection
                DTOutput("table"),

                # Download button
                downloadButton("download_table", "Download Table of Existing Tags")
              )
            )
          )
          })

          # New server logic

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
                paste("already_existing_tags", ".csv", sep = "")
              },
              content = function(file) {
                write.csv(entered, file, row.names = FALSE)
              }
            )


        }else{
          output$dynamicUI <- renderUI({
            fluidPage(
            h3("All releases uploaded successfully! There were no errors and none of the tags were found to already exist. Close this window.")
            ) })
          }

      })

    }

    # Create the Shiny app object
    return(shinyApp(ui = ui, server = server))
  }


  if(!return_error & !return_warning){

  ###### db UPLOAD HERE. Check that entry doesn't already exist before uploading

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
  }else{con <- dbConnect(RSQLite::SQLite(), "C:/LOBTAG/LOBTAG.db")}

  ## check for already entered tags, then upload all new tag entries
  entered =NULL
  for(i in 1:nrow(rel)){
  sql <- paste0("SELECT * FROM ",table_name," WHERE TAG_ID= '",rel$TAG_ID[i],"'")
  check <- dbSendQuery(con, sql)
  existing_tag <- dbFetch(check)
  entered <- rbind(entered,existing_tag)
  dbClearResult(check)

  if(nrow(existing_tag)==0){
    sql <- paste("INSERT INTO ",table_name, " VALUES ('",rel$SAMPLER[i],"', '",rel$SAMPLER_2[i],"', '",rel$AFFILIATION[i],"', '",rel$VESSEL[i],"','",rel$CAPTAIN[i],"','",rel$PORT[i],"','",rel$MANAGEMENT_AREA[i],"','",rel$DAY[i],"','",rel$MONTH[i],"','",rel$YEAR[i],"','",rel$TAG_COLOR[i],"','",rel$TAG_PREFIX[i],"','",rel$TAG_NUM[i],"','",rel$TAG_ID[i],"','",rel$CARAPACE_LENGTH[i],"','",rel$SEX[i],"','",rel$SHELL[i],"','",rel$CLAW[i],"','",rel$LAT_DEGREES[i],"','",rel$LAT_MINUTES[i],"','",rel$LON_DEGREES[i],"','",rel$LON_MINUTES[i],"','",rel$LATDDMM_MM[i],"','",rel$LONDDMM_MM[i],"','",rel$LAT_DD[i],"','",rel$LON_DD[i],"','",rel$REL_DATE[i],"','",rel$COMMENTS[i],"')", sep = "")
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
    h4("The following tags already exist in the database so were not uploaded:"),
    sidebarLayout(
      sidebarPanel(
        # Text box to display all TAG_NUM values
        textOutput("tag_values")
      ),

      mainPanel(
        # Display table based on selection
        DTOutput("table"),

        # Download button
        downloadButton("download_table", "Download Table of Existing Tags")
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
        paste("already_existing_tags", ".csv", sep = "")
      },
      content = function(file) {
        write.csv(entered, file, row.names = FALSE)
      }
    )
  }

  # Run the application
  shinyApp(ui = ui, server = server)

 }else{
   dlg_message("All releases uploaded successfully! There were no errors and none of the tags were found to already exist.")
  }

  }



#######  SCRAP
# new.names= c("Vessel", "Captain", "Port", "MANAGEMENT_AREA", "Sampler", "Sampler 2", "Affiliation", "Day",	"Month", "Year", "Tag Prefix",	"Tag Color", "Tag Num",	"Carapace Length",	"Sex",	"Shell", "Claw", "Lat Degrees",	"Lat Minutes",	"Lon Degrees",	"Lon Minutes", "latddmm.mm", "londdmm.mm", "latdd.dd", "londd.dd", "Date")

# library(dplyr)
# library(ROracle)
# library(shiny)
# library(DT)
# library(svDialogs)


}

