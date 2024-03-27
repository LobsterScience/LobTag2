#' @title upload_releases
#' @import dplyr shiny svDialogs
#' @description batch uploads tag release data
#' @export

upload_releases <- function(){

## Allow user to choose data file to upload
dlg_message("In the following window, choose a csv file containing your releases data")
file_path <- dlg_open(filter = dlg_filters["csv",])$res
releases <- read.csv(file_path)
## Process / standardize the data table

##decimal degrees and degrees minutes formatting done here
releases$latddmm.mm = releases$Lat.Degrees * 100 + releases$Lat.Minutes
releases$londdmm.mm = releases$Lon.Degrees * 100 + releases$Lon.Minutes

releases$latdd.dd = releases$Lat.Degrees + releases$Lat.Minutes / 60
releases$londd.dd = releases$Lon.Degrees + releases$Lon.Minutes / 60

##date column isn't 100% necessary but it's a good indication if things are going wrong
releases$Date = paste(releases$Day, releases$Month, releases$Year, sep = "/")


## retrieve only selected variables if there are extra / differently ordered columns
select.names = c("Vessel", "Captain", "Port", "LFA",  "Sampler", "Sampler.2", "Affiliation", "Day",	"Month", "Year", "Tag.Prefix",	"Tag.Color", "Tag.Num",	"Carapace.Length",	"Sex",	"Shell", "Claw", "V.Notch", "Lat.Degrees",	"Lat.Minutes",	"Lon.Degrees",	"Lon.Minutes", "latddmm.mm", "londdmm.mm", "latdd.dd", "londd.dd", "Date")
rel <- dplyr::select(releases,(all_of(select.names)))
new.names= c("Vessel", "Captain", "Port", "LFA", "Sampler", "Sampler 2", "Affiliation", "Day",	"Month", "Year", "Tag Prefix",	"Tag Color", "Tag Num",	"Carapace Length",	"Sex",	"Shell", "Claw", "V-Notch", "Lat Degrees",	"Lat Minutes",	"Lon Degrees",	"Lon Minutes", "latddmm.mm", "londdmm.mm", "latdd.dd", "londd.dd", "Date")
names(rel) = new.names

## error checking (Lobster specific, edit for other species / tagging programs):
# sex_values <- c(NA,0,1,2,3)
# shell_values <- c(NA, 1:7)
# claw_values <- c(NA,1,2,3)
# vnotch_values <- c(NA,"YES","NO")
# carapace_values <- c(NA, 40:150) #this one will alert the user but continue with upload.
# carapace_values_fsrs <- c(NA, 40:170) # fsrs samples some really big females as part of the v-notch program, so increase the threshold in this case.

########### Testing
rel[1,27]= NA
rel[2,13]=NA


## error checking (General):
repeat_tags = rel$`Tag Num`[duplicated(rel$`Tag Num`)==TRUE & duplicated(rel$`Tag Prefix`)==TRUE]
bad_tag = rownames(rel)[rel$`Tag Num` %in% NA]
bad_lat = rel$`Tag Num`[rel$latdd.dd %in% NA | nchar(as.character(rel$latdd.dd))<2 | !is.numeric(rel$latdd.dd)]
bad_lon = rel$`Tag Num`[rel$londd.dd %in% NA | nchar(as.character(rel$londd.dd))<2 | !is.numeric(rel$londd.dd)]
bad_date = rel$`Tag Num`[rel$Date %in% NA]


error_out= ""
error_tab = NULL
return_error = FALSE

if(length(bad_tag) > 0){
  for(i in 1:length(bad_tag)){
    error_out = paste(error_out, "\nBad or missing tag at row: ", bad_tag[i], sep = "")
    error_tab = rbind(error_tab,c(bad_tag[i],"","Bad or missing tag"))
  }
  return_error = TRUE
}
if(length(repeat_tags) > 0){
  for(i in 1:length(repeat_tags)){
    error_out = paste(error_out, "\nPossible duplicate tag number: ", repeat_tags[i], sep = "")
    error_tab = rbind(error_tab,c("",repeat_tags[i],"Possible duplicate tag number"))
  }
  return_error = TRUE
}
if(length(bad_lat) > 0){
  for(i in 1:length(bad_lat)){
    error_out = paste(error_out, "\nBad or missing latitude for tag number: ", bad_lat[i], sep = "")
    error_tab = rbind(error_tab,c("",bad_lat[i],"Bad or missing latitude"))
  }
  return_error = TRUE
}
if(length(bad_lon) > 0){
  for(i in 1:length(bad_lon)){
    error_out = paste(error_out, "\nBad or missing longitude for tag number: ", bad_lon[i], sep = "")
    error_tab = rbind(error_tab,c("",bad_lon[i],"Bad or missing longitude"))
  }
  return_error = TRUE
}
if(length(bad_date) > 0){
  for(i in 1:length(bad_date)){
    error_out = paste(error_out, "\nBad or missing date for tag number: ", bad_date[i], sep = "")
    error_tab = rbind(error_tab,c("",bad_date[i],"Bad or missing date"))
  }
  return_error = TRUE
}

if(return_error){
  colnames(error_tab)=c("Row","Tag Number","Error")
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

if(!return_error){
  ###### ORACLE UPLOAD HERE


  ######

  }





}

