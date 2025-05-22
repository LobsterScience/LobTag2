#' @title get_table
#' @import dplyr RSQLite
#' @description Quickly access SQL database tables for LobTag2 in R
#' @export

get_table <- function(table= NULL, db = "local",
                           oracle.user =if(exists("oracle.lobtag.user")) oracle.lobtag.user else NULL,
                           oracle.password = if(exists("oracle.lobtag.password")) oracle.lobtag.password else NULL,
                           oracle.dbname = if(exists("oracle.lobtag.server")) oracle.lobtag.server else NULL){

  ### open db connection
db_connection(db, oracle.user, oracle.password, oracle.dbname)

if(table %in% c("releases","Releases","RELEASES")){
  query = paste0("SELECT * FROM LBT_RELEASES")
  releases <- dbSendQuery(con, query)
  releases <<- fetch(releases)
}

if(table %in% c("recaptures","Recaptures","RECAPTURES")){
  query = paste0("SELECT * FROM LBT_RECAPTURES")
  recaptures <- dbSendQuery(con, query)
  recaptures <<- fetch(recaptures)
}

if(table %in% c("path","Path","PATH")){
  query = paste0("SELECT * FROM LBT_PATH")
  path <- dbSendQuery(con, query)
  path <<- fetch(path)
}

if(table %in% c("paths","Paths","PATHS")){
  query = paste0("SELECT * FROM LBT_PATHS")
  paths <- dbSendQuery(con, query)
  paths <<- fetch(paths)
}

if(table %in% c("people","People","PEOPLE")){
  query = paste0("SELECT * FROM LBT_PEOPLE")
  peeps <- dbSendQuery(con, query)
  people <<- fetch(peeps)
}

  dbDisconnect(con)
}


