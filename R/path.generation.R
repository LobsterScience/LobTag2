#' @title generate_paths
#' @import dplyr RSQLite raster gdistance PBSmapping
#' @description Uses releases and recapture data with spatial/depth information to draw plausible paths of animal movement
#' @export

generate_paths <- function(db = "local", oracle.user = oracle.personal.user, oracle.password = oracle.personal.password, oracle.dbname = oracle.personal.server, tags = "all", depth.raster.path = system.file("data", "depthraster2.tif", package = "LobTag2"), neighborhood = 16, type = "least.cost"){

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

############################################################################################################
##########################################################################################################
  ## Check if path tables already exist and create them if they don't
  ### open db connection
  if(db %in% "Oracle"){
    tryCatch({
      drv <- DBI::dbDriver("Oracle")
      con <- dbConnect(drv, username = oracle.user, password = oracle.password, dbname = oracle.dbname)
    }, warning = function(w) {
    }, error = function(e) {
      return(toJSON("Connection failed"))
    }, finally = {
    })
  }else{
    dir.create("C:/LOBTAG",showWarnings = F)
    con <- dbConnect(RSQLite::SQLite(), "C:/LOBTAG/LOBTAG.db")
  }

  table_name <- "LBT_PATH"

  ## look for existing table
  if(db %in% "Oracle"){
    query <- paste("SELECT COUNT(*) FROM user_tables WHERE table_name = '", table_name, "'", sep = "")
  }else{query <- paste("SELECT COUNT(*) AS table_count FROM sqlite_master WHERE type='table' AND name='", table_name, "'", sep = "")}

  result <- dbGetQuery(con, query)
  if (result[[1]] == 0) {
    print(paste0("Creating new table called: ",ifelse(db %in% "Oracle",paste0(oracle.user,"."),""),table_name))
    # Define the SQL statement to create the table
    sql_statement <- paste0("
    CREATE TABLE ",table_name," (
    TID VARCHAR2(50),
    CID VARCHAR2(50),
    CDATE VARCHAR2(50),
    DIST VARCHAR2(50),
    TAG_NUM VARCHAR2(50),
    TAG_PREFIX VARCHAR2(50)
)")

    # Execute the SQL statement
    dbSendQuery(con, sql_statement)

  }

  table_name <- "LBT_PATHS"

  ## look for existing table
  if(db %in% "Oracle"){
    query <- paste("SELECT COUNT(*) FROM user_tables WHERE table_name = '", table_name, "'", sep = "")
  }else{query <- paste("SELECT COUNT(*) AS table_count FROM sqlite_master WHERE type='table' AND name='", table_name, "'", sep = "")}

  result <- dbGetQuery(con, query)
  if (result[[1]] == 0) {
    print(paste0("Creating new table called: ",ifelse(db %in% "Oracle",paste0(oracle.user,"."),""),table_name))
    # Define the SQL statement to create the table
    sql_statement <- paste0("
    CREATE TABLE ",table_name," (
    TID VARCHAR2(50),
    CID VARCHAR2(50),
    POS VARCHAR2(50),
    LON VARCHAR2(50),
    LAT VARCHAR2(50),
    TAG_NUM VARCHAR2(50),
    TAG_PREFIX VARCHAR2(50),
    REL_DATE VARCHAR2(50),
    REC_DATE VARCHAR2(50),
    REC_PERSON VARCHAR2(100)
)")

    # Execute the SQL statement
    dbSendQuery(con, sql_statement)

  }

  # Close the connection
  dbDisconnect(con)
#################################################################################################################################################
#################################################################################### MAIN FUNCTION:

    base::message("Generating paths... This may take a while if you have many recaptures!")
  ## make table with all recaptures and their release information
  recheck=TRUE
while(recheck){
  ### open db connection
  if(db %in% "Oracle"){
    tryCatch({
      drv <- DBI::dbDriver("Oracle")
      con <- dbConnect(drv, username = oracle.user, password = oracle.password, dbname = oracle.dbname)
    }, warning = function(w) {
    }, error = function(e) {
      return(toJSON("Connection failed"))
    }, finally = {
    })
  }else{
    con <- dbConnect(RSQLite::SQLite(), "C:/LOBTAG/LOBTAG.db")
  }

  query = paste0("SELECT * FROM LBT_RECAPTURES")
  recaptures <- dbSendQuery(con, query)
  recaptures <- fetch(recaptures)

  cap.samps <- paste0("('",paste(recaptures$TAG_ID, collapse ="','"),"')")
  query = paste0("SELECT * FROM LBT_RELEASES WHERE TAG_ID in ",cap.samps)
  releases <- dbSendQuery(con, query)
  releases <- fetch(releases)

  ## deal with recaptures that have no person name
  recaptures <- recaptures %>% mutate(PERSON = ifelse(PERSON %in% "",NA,PERSON))
  for(i in 1:nrow(recaptures)){
    if(is.na(recaptures$PERSON[i])){
      recaptures$PERSON[i] = paste0("Fisher ",i)
      }
    }

  ## filter back so that only recaptures that have releases are pathed. Warn user about any recaptures that don't have releases
  if(!(length(unique(recaptures$TAG_ID))==length(unique(releases$TAG_ID)))){
    base::message("WARNING! There are recaptured tags that don't have initial release coordinates! Only pathing tags that have release data.")
    recaptures <- recaptures %>% filter(TAG_ID %in% releases$TAG_ID)
  }


  rel.prep <- releases %>% dplyr::select(TAG_ID, LAT_DD,LON_DD, REL_DATE,TAG_NUM,TAG_PREFIX) %>% rename(rel_lat = LAT_DD, rel_lon = LON_DD)
  rec.prep <- recaptures %>% dplyr::select(TAG_ID, LAT_DD,LON_DD,REC_DATE,PERSON) %>% rename(rec_lat = LAT_DD, rec_lon = LON_DD)
  rec.prep$REC_DATE = as.Date(rec.prep$REC_DATE, format= "%Y-%m-%d")
  pdat <- left_join(rec.prep,rel.prep)

  ## find tags that have not yet been pathed
  dat <- dbSendQuery(con, paste0("SELECT * FROM LBT_PATH"))
  dat <-  fetch(dat)
  dbDisconnect(con)

  ## for selected tag:
  if(!(tags %in% "all")){
    pdat <- pdat %>% filter(TAG_ID %in% tags)
    if(nrow(pdat)==0){stop("Tag ID not found!")}
  }
  ## Don't re-generate paths that already exist
  pathed = which(paste(as.character(pdat$TAG_ID), pdat$REC_DATE) %in% paste(as.character(dat$TID), as.character(dat$CDATE)))
  if(length(pathed)>0){x = pdat[-pathed,]}else{x=pdat}
  ##Unless... there are new recaptures with dates earlier then current most recent recapture; in these cases will need to clear and regenerate the whole path
  repath = NULL
  for(i in unique(x$TAG_ID)){
    tab <- x %>% filter(TAG_ID %in% i)
    d <- unique(tab$REC_DATE)
    p.tab <- pdat[pathed,] %>% filter(TAG_ID %in% i)
    earlier <- any(tab$REC_DATE<max(p.tab$REC_DATE))
    if(earlier){repath=c(repath,i)}
  }
if(length(repath)>0){
  base::message("There are new recaptures of tags that are earlier in time than the most recent recaptures for those tags. Deleting and regenerating paths for these tags...")
  if(db %in% "Oracle"){
    tryCatch({
      drv <- DBI::dbDriver("Oracle")
      con <- dbConnect(drv, username = oracle.user, password = oracle.password, dbname = oracle.dbname)
    }, warning = function(w) {
    }, error = function(e) {
      return(toJSON("Connection failed"))
    }, finally = {
    })
  }else{
    con <- dbConnect(RSQLite::SQLite(), "C:/LOBTAG/LOBTAG.db")
  }

  for(i in repath){
    ## delete all single paths for tag
    query1 <- paste0("DELETE FROM LBT_PATH WHERE TID = '", i, "'")

    ## delete all paths for tag
    query2 <- paste0("DELETE FROM LBT_PATHS WHERE TID = '",i,"'")

    # Execute the delete querys
    if(db %in% "local"){dbBegin(con)}
    dbExecute(con, query1)
    dbExecute(con, query2)
    dbCommit(con)

  }
  dbDisconnect(con)
  ## if recheck is true, start back over and re-query the database with bad paths removed
  recheck=TRUE
  }else{recheck=FALSE}


  } ## checking done, begin path creation
  ## create paths
  x <- x %>% rename(PID=TAG_ID)

  dftowrite = NULL
  df2towrite = NULL
  dxtowrite = NULL

  count = 1
  previd = ""
  if(nrow(x) == 0){base::message("No new paths to create!")}else{

    ################################################ prepare depth raster
    trans = NULL
    r = raster(depth.raster.path)
    mr = as.matrix(r)
    mr[which(mr > -5000 & mr < -1)] = -1                       #using least cost (the lowest point is in the -4000s so we can go from -5000 to 0 ie. sea level)
    mr = apply(mr, 2, function(x) dnorm(x,mean=-1,sd=1))
    r = setValues(r, mr)

    tr <- transition(r, mean, neighborhood)
    if(type  == "random.walk"){
      trans = geoCorrection(tr, type = "r", scl=FALSE)
    }
    if(type  == "least.cost"){
      trans = geoCorrection(tr, type = "c", scl=FALSE)
    }
    ###############################################

    x <- x %>% arrange(PID,REC_DATE)
    dat <- arrange(dat,TID)
    for(i in 1:nrow(x)){
      if(x$PID[i] == previd){
        count = count + 1
        start <- c(as.numeric(x$rec_lon[i-1]), as.numeric(x$rec_lat[i-1]))
        #print(paste0("row 57: ", count))
      }else{
        previd = x$PID[i]
        count = 1 + sum(dat$TID == x$PID[i])
        start <- c(as.numeric(x$rel_lon[i]), as.numeric(x$rel_lat[i]))
        #print(paste0("row 62: ", count))
      }
      if(any(start %in% NA)){return(base::message("ERROR! There are recaptured tags that don't have initial release coordinates!"))}
      end <- c(as.numeric(x$rec_lon[i]), as.numeric(x$rec_lat[i]))

      if(abs(start[1] - end[1]) < res(trans)[1] && abs(start[2] - end[2]) < res(trans)[1] || is.na(cellFromXY(r, start)) || is.na(cellFromXY(r, end))){
        AtoB = rbind(start, end)
      }else{
        AtoB = shortestPath(trans, start, end, output="SpatialLines")
      }
      cor = data.frame(coordinates(AtoB))
      names(cor) = c("x", "y")
      xrep = cor$x[1]
      yrep = cor$y[1]
      #k = 1
      for(k in 1:(nrow(cor)-1)){
        if(cor$x[k] == xrep){ cor$x[k] = start[1] }
        else{ xrep = 1000000 }

        if(cor$y[k] == yrep){ cor$y[k] =  start[2] }
        else{ yrep = 1000000 }
      }
      xrep = cor$x[nrow(cor)]
      yrep = cor$y[nrow(cor)]
      #k = 2
      for(k in nrow(cor):2){
        if(cor$x[k] == xrep) cor$x[k] =  end[1]
        else xrep = 1000000

        if(cor$y[k] == yrep) cor$y[k] =  end[2]
        else yrep = 1000000
      }

      names(cor) = c("X", "Y")
      cor$PID = 1
      cor$POS = 1:nrow(cor)
      tpoly = as.PolySet(cor, projection = "LL")
      leng = calcLength (tpoly, rollup = 3, close = FALSE) #km

      events <- NULL
      for(j in 1:nrow(cor)){
        if(j<nrow(cor)){events <- rbind(events,c(x$REL_DATE[i],NA,NA))}
        if(j==nrow(cor)){events <- rbind(events,c(x$REL_DATE[i],format(x$REC_DATE[i], "%Y-%m-%d"),x$PERSON[i]))}
      }

      dxp = cbind(rep(x$PID[i], nrow(cor)),rep(count, nrow(cor)), 1:nrow(cor), cor$X, cor$Y,x$TAG_NUM[i],x$TAG_PREFIX[i],events)
      dxtowrite = rbind(dxtowrite, dxp)
      df2towrite = rbind(df2towrite, cbind(x$PID[i], count, as.character(x$REC_DATE[i]), leng$length, x$TAG_NUM[i],x$TAG_PREFIX[i]))

      dftowrite = rbind(dftowrite, cbind(x$PID[i],paste(cor[,1], collapse = ","), paste(cor[,2], collapse = ","), as.character(x$REC_DATE[i]), leng$length, x$TAG_NUM[i],x$TAG_PREFIX[i]))
    }
  }


  if(!is.null(dftowrite)){
    dftowrite = data.frame(dftowrite)
    df2towrite = data.frame(df2towrite)
    dxtowrite = data.frame(dxtowrite)
    #tax prefix and number should follow through
    names(dftowrite) = c("ID", "LON", "LAT", "CDATE", "DIST", "TAG_NUM","TAG_PREFIX")
    names(df2towrite) = c("TID", "CID", "CDATE", "DIST", "TAG_NUM","TAG_PREFIX")
    names(dxtowrite) = c("TID", "CID", "POS", "LON", "LAT", "TAG_NUM","TAG_PREFIX","REL_DATE","REC_DATE","REC_PERSON")

    #add data to database
    ### open db connection
    if(db %in% "Oracle"){
      tryCatch({
        drv <- DBI::dbDriver("Oracle")
        con <- dbConnect(drv, username = oracle.user, password = oracle.password, dbname = oracle.dbname)
      }, warning = function(w) {
      }, error = function(e) {
        return(toJSON("Connection failed"))
      }, finally = {
      })
    }else{
      con <- dbConnect(RSQLite::SQLite(), "C:/LOBTAG/LOBTAG.db")
    }


      if(db %in% "Oracle"){
        pathdb = "LBT_PATH"
        pathsdb = "LBT_PATHS"

        ##old method, caused oracle issues with large number of rows
        # path_call = create_sql_query(pathdb, df2towrite)
        # # paths_call = create_sql_query(pathsdb, dxtowrite)
        # result <- dbSendQuery(con, path_call)
        # result <- dbSendQuery(con, paths_call)

        ##path
        for(i in 1:nrow(df2towrite)){
          path_call = create_sql_query(pathdb, df2towrite[i,])
          result <- dbSendQuery(con, path_call)
          dbClearResult(result)
        }
        dbCommit(con)

        ##paths
        for(i in 1:nrow(dxtowrite)){
          paths_call = create_sql_query(pathsdb, dxtowrite[i,])
          result <- dbSendQuery(con, paths_call)
          dbClearResult(result)
        }
        dbCommit(con)

        print("New paths calculated and written to path tables.")

      }else{
        dbWriteTable(con, "LBT_PATH", df2towrite, append = TRUE, row.names = FALSE)
        dbWriteTable(con, "LBT_PATHS", dxtowrite, append = TRUE, row.names = FALSE)
        print("New paths calculated and written to paths tables.")
        }

    dbDisconnect(con)
  }else{
    print("No new paths created.")  }


  # library(dplyr)
  # library(raster)
  # library(gdistance)
  # library(PBSmapping)


  }




#this creates the sql query that we use to get around the permission glitch to upload data to oracle
#used for lbt_path/lbt_paths tables
create_sql_query = function(lbt_table, df){

  all_start = "INSERT ALL "

  helper = ""
  for (k in 1:length(names(df))){
    if (k==1){
      helper = names(df)[k]
    }
    else {helper = paste(helper, names(df)[k], sep = ", ")}
  }

  row_start = paste("INTO ", lbt_table, " (", helper ,") VALUES ('", sep = "")
  footer = " SELECT * FROM DUAL"

  #values
  values = ""
  for (i in 1:nrow(df)){
    for (j in 1:ncol(df)){
      if (j==1){
        values = paste(values, row_start, df[j][i,], sep = "")
      } else {
        values = paste(values,"' , '" ,df[j][i,], sep = "")
      }
    }
    if (i==nrow(df)){
      values = paste(values, "' )", sep = "")
    } else {
      values = paste(values, "' ) ", sep = "")
    }
  }

  sql_call = paste(all_start, values, footer, sep = "")
  return(sql_call)
}





