#' @title generate_paths
#' @import dplyr raster gdistance PBSmapping
#' @description Uses releases and recapture data with spatial/depth information to draw plausible paths of animal movement
#' @export

generate_paths <- function(depth.raster.path = "C:/bio.data/bio.lobster/data/tagging/depthraster2.tif",neighborhood = 16,type = "least.cost"){

  tryCatch({
    drv <- DBI::dbDriver("Oracle")
    conn <- ROracle::dbConnect(drv, username = oracle.personal.user, password = oracle.personal.password, dbname = oracle.personal.server)
  }, warning = function(w) {
  }, error = function(e) {
    return(toJSON("Connection failed"))
  }, finally = {
  })


  ## make table with all recaptures and their release information
  query = paste("SELECT * FROM ELEMENTG.LBT_RECAPTURES")
  recaptures <- ROracle::dbSendQuery(conn, query)
  recaptures <- ROracle::fetch(recaptures)

  cap.samps <- paste0("('",paste(recaptures$TAG_ID, collapse ="','"),"')")
  query = paste("SELECT * FROM ELEMENTG.LBT_RELEASES WHERE TAG_ID in",cap.samps)
  releases <- ROracle::dbSendQuery(conn, query)
  releases <- ROracle::fetch(releases)

  rel.prep <- releases %>% dplyr::select(TAG_ID, LAT_DD,LON_DD, REL_DATE,TAG_NUM,TAG_PREFIX) %>% rename(rel_lat = LAT_DD, rel_lon = LON_DD)
  rec.prep <- recaptures %>% dplyr::select(TAG_ID, LAT_DD,LON_DD,REC_DATE,PERSON) %>% rename(rec_lat = LAT_DD, rec_lon = LON_DD)
  rec.prep$REC_DATE = as.Date(rec.prep$REC_DATE, format= "%d/%m/%Y")
  pdat <- left_join(rec.prep,rel.prep)

  ## prepare depth raster
  trans = NULL
  r = raster(depth.raster.path)
  mr = as.matrix(r)
  mr[which(mr > -5000 & mr < 0)] = -1                       #using least cost (the lowest point is in the -4000s so we can go from -5000 to 0 ie. sea level)
  mr = apply(mr, 2, function(x) dnorm(x,mean=-1,sd=1))
  r = setValues(r, mr)

  tr <- transition(r, mean, neighborhood)
  if(type  == "random.walk"){
    trans = geoCorrection(tr, type = "r", scl=FALSE)
  }
  if(type  == "least.cost"){
    trans = geoCorrection(tr, type = "c", scl=FALSE)
  }

  ## find tags that have not yet been pathed
  dat <- ROracle::dbSendQuery(conn, "SELECT * FROM ELEMENTG.LBT_PATH")
  dat <-  ROracle::fetch(dat)
  ROracle::dbDisconnect(conn)
  pathed = which(paste(as.character(pdat$TAG_ID), pdat$REC_DATE) %in% paste(as.character(dat$TID), as.character(dat$CDATE)))
  if(length(pathed)>0){x = pdat[-pathed,]}else{x=pdat}

  ## create paths
  x <- x %>% rename(PID=TAG_ID)

  dftowrite = NULL
  df2towrite = NULL
  dxtowrite = NULL

  count = 1
  previd = ""
  if(nrow(x) == 0){base::message("No new paths to create!")}else{
    x <- x %>% arrange(PID,REC_DATE)
    dat <- arrange(dat,TID)
    for(i in 1:nrow(x)){
      if(x$PID[i] == previd){
        count = count + 1
        #print(paste0("row 57: ", count))
      }else{
        previd = x$PID[i]
        count = 1 + sum(dat$TID == x$PID[i])
        #print(paste0("row 62: ", count))
      }
      start <- c(as.numeric(x$rel_lon[i]), as.numeric(x$rel_lat[i]))
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

      dxp = cbind(rep(x$PID[i], nrow(cor)),rep(count, nrow(cor)), 1:nrow(cor), cor$X, cor$Y,x$TAG_NUM[i],x$TAG_PREFIX[i])
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
    names(dxtowrite) = c("TID", "CID", "POS", "LON", "LAT", "TAG_NUM","TAG_PREFIX")

    #add data to oracle
    drv <- DBI::dbDriver("Oracle")
    con <- ROracle::dbConnect(drv, username = oracle.personal.user, password = oracle.personal.password, dbname = oracle.personal.server)

      pathdb = paste("ELEMENTG",".","LBT_PATH", sep = "")
      pathsdb = paste("ELEMENTG",".","LBT_PATHS", sep = "")

      path_call = create_sql_query(pathdb, df2towrite)
      paths_call = create_sql_query(pathsdb, dxtowrite)

      result <- ROracle::dbSendQuery(con, path_call)
      result <- ROracle::dbSendQuery(con, paths_call)

      ROracle::dbCommit(con)
      ROracle::dbDisconnect(con)
      print("New paths calculated and written to paths table.")
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





