
#' @title db_connection
#' @import dplyr RSQLite DBI
#' @description establishes connection to database of users choosing
#' @export

db_connection = function(db = NULL, oracle.user = NULL, oracle.password = NULL, oracle.dbname = NULL){

  if(db %in% "Oracle"){

    if (!requireNamespace("ROracle", quietly = TRUE)) {
      stop("You need to manually install ROracle to use Oracle functionality with this app.")
    }
    require(ROracle)

    tryCatch({
      drv <- DBI::dbDriver("Oracle")
      con <<- ROracle::dbConnect(drv, username = oracle.user, password = oracle.password, dbname = oracle.dbname)
    }, warning = function(w) {
    }, error = function(e) {
      return(toJSON("Connection failed"))
    }, finally = {
    })
  }else{
    if(db %in% c("local","Local","LOCAL")){
      dir.create("C:/LOBTAG",showWarnings = F)
      con <<- dbConnect(RSQLite::SQLite(), "C:/LOBTAG/LOBTAG.db")
    }
  }
}
