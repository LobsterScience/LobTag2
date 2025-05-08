#' @title generate_paths
#' @import dplyr RSQLite raster gdistance PBSmapping terra sf openxlsx
#' @description Uses releases and recapture data with spatial/depth information to draw plausible paths of animal movement
#' @export

generate_paths <- function(db = NULL, tags = "all", depth.raster.path = system.file("data", "gebco_2024.tif", package = "LobTag2"),
                           neighborhood = 8, type = "least.cost", regen.paths = FALSE, save.table = FALSE, add.land = T,
                           oracle.user =if(exists("oracle.personal.user")) oracle.personal.user else NULL,
                           oracle.password = if(exists("oracle.personal.password")) oracle.personal.password else NULL,
                           oracle.dbname = if(exists("oracle.personal.server")) oracle.personal.server else NULL){

  if(is.null(db)){return(base::message("You need to specify a database with db = "))}

   if(db %in% c("local","Local","LOCAL")){
    db = "local"
    dir.create("C:/LOBTAG",showWarnings = F)
  }

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
db_connection(db, oracle.user, oracle.password, oracle.dbname)

  table_name <- "LBT_PATH"

  ## look for existing table
  if(db %in% "Oracle"){
    query <- paste("SELECT COUNT(*) FROM user_tables WHERE table_name = '", table_name, "'", sep = "")
  }else{if(db %in% "local")query <- paste("SELECT COUNT(*) AS table_count FROM sqlite_master WHERE type='table' AND name='", table_name, "'", sep = "")}

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
  }else{if(db %in% "local")query <- paste("SELECT COUNT(*) AS table_count FROM sqlite_master WHERE type='table' AND name='", table_name, "'", sep = "")}

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
  db_connection(db, oracle.user, oracle.password, oracle.dbname)

  query = paste0("SELECT * FROM LBT_RECAPTURES")
  recaptures <- dbSendQuery(con, query)
  recaptures <- fetch(recaptures)

  ## IN clauses have 1000 element limits so use a lapply solution to bring in releases
  releases <- do.call(rbind, lapply(unique(recaptures$TAG_ID), function(i) {
    cap.samp <- paste0("'", i, "'")
    query <- paste0("SELECT * FROM LBT_RELEASES WHERE TAG_ID IN (", cap.samp, ")")
    dbGetQuery(con, query)
  }))

  ## Handle any special characters in names for SQL querying
  ### function for handling special characters
  escape_special_chars <- function(x) {
    if (is.character(x)) {
      # Convert to UTF-8 and replace invalid characters with a space
      x <- iconv(x, from = "", to = "UTF-8", sub = "[BAD char]")
      # Optionally remove or flag the Unicode replacement character (�)
      x <- gsub("\uFFFD", "[BAD char]", x)  # or use "[BAD char]" if you want to mark them
      # Escape single quotes (') and dashes (-) for Oracle
      x <- gsub("'", "''", x)
      x <- gsub("-", "\\-", x)
    }
    return(x)
  }


  rec.cols.to.handle <- c("PERSON","PERSON_2","VESSEL","CAPTAIN","MANAGEMENT_AREA","COMMENTS")
for(i in 1:nrow(recaptures)){
  for(j in rec.cols.to.handle){
    recaptures[i,j] <- escape_special_chars(recaptures[i,j])
  }
}
  rel.cols.to.handle <- c("SAMPLER","SAMPLER_2","AFFILIATION","VESSEL","CAPTAIN","PORT","MANAGEMENT_AREA","COMMENTS")
  for(i in 1:nrow(releases)){
    for(j in rel.cols.to.handle){
      releases[i,j] <- escape_special_chars(releases[i,j])
    }
  }

  ## deal with recaptures that have no person name
  recaptures <- recaptures %>% mutate(PERSON = ifelse(PERSON %in% c("","NA"),NA,PERSON))
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
  if(!(any(tags %in% "all"))){
    pdat <- pdat %>% filter(TAG_ID %in% tags)
    if(nrow(pdat)==0){stop("Tag ID not found!")}
  }
  ## Don't re-generate paths that already exist
  repath = NULL
  if(!regen.paths){
  pathed = which(paste(as.character(pdat$TAG_ID), pdat$REC_DATE) %in% paste(as.character(dat$TID), as.character(dat$CDATE)))
  if(length(pathed)>0){x = pdat[-pathed,]}else{x=pdat}

  ##Unless... there are new recaptures with dates earlier then current most recent recapture; in these cases will need to clear and regenerate the whole path

  for(i in unique(x$TAG_ID)){
    tab <- x %>% filter(TAG_ID %in% i)
    d <- unique(tab$REC_DATE)
    p.tab <- pdat[pathed,] %>% filter(TAG_ID %in% i)
    earlier <- any(tab$REC_DATE<max(p.tab$REC_DATE))
    if(earlier){repath=c(repath,i)}
    }

  }
  ## OR unless regen.paths is selected by user
  if(regen.paths){
    x = pdat
    repath = unique(x$TAG_ID)}

if(length(repath)>0){
  if(!regen.paths){
    base::message("There are new recaptures of tags that are earlier in time than the most recent recaptures for those tags. Deleting and regenerating paths for these tags...")
  }

db_connection(db, oracle.user, oracle.password, oracle.dbname)

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
  if(regen.paths){recheck=FALSE}else{recheck=TRUE} ## don't need to recheck if the user already chose to regen all the chosen tags
  }else{recheck=FALSE}


  } ## checking done, begin path creation
  ## create paths
  x <- x %>% rename(PID=TAG_ID)

  dftowrite = NULL
  df2towrite = NULL
  dxtowrite = NULL

  if(nrow(x) == 0){base::message("No new paths to create!")}else{

    ################################################ prepare depth raster


    r = rast(depth.raster.path)

      ### trim raster to data area to avoid working with whole map
      xmin = min(as.numeric(c(x$rel_lon,x$rec_lon)))
      xmax = max(as.numeric(c(x$rel_lon,x$rec_lon)))
      ymin = min(as.numeric(c(x$rel_lat,x$rec_lat)))
      ymax = max(as.numeric(c(x$rel_lat,x$rec_lat)))


      ##for troubleshooting (can see land pixels closeup. Keep cropped but increase crop size when done; crop greatly improves function speed but too small runs risk of paths not having route to escape land):
      ###################
      ymax = ymax+0.7
      ymin = ymin-0.7
      xmax=xmax+0.7
      xmin=xmin-0.7
      # ymax = ymax+0.08
      # ymin = ymin-0.08
      # xmax=xmax+0.08
      # xmin=xmin-0.08
      ###################
      extent_values <- extent(xmin,xmax,ymin,ymax)
      r <- crop(r, extent_values)

    # Reclassify matrix with 3 columns: lower bound, upper bound, and new value
    reclass_matrix <- matrix(c(-Inf, 0, 1,    # Deep water (< 0 meter) gets a cost of 1
                               0, Inf, Inf),  # Shallow water (>= 0 meter) and land are assigned Inf (impassable)
                             ncol = 3, byrow = TRUE)
    # Create a binary mask: Land = Inf cost, Ocean = depth or constant cost
    # Set land (elevation > 0) to a very high cost (or Inf)
    cost_surface <- classify(r, reclass_matrix)

    ################################### can directly edit cells to create land in problem areas
    new_land_coords <- data.frame(lon = c(-59.81, -59.815, -59.805,-59.80,-59.80,-59.80,-59.797), lat = c(46.125, 46.125,46.125,46.125,46.127,46.13,46.13))
    XY1878 <- data.frame(lon = c(-60.35), lat = c(46.655))
    XY461 <- data.frame(lon = c(-59.685,-59.682,-59.685,-59.679,-59.72,-59.715,-59.71,-59.71,-59.71,-59.71,-59.71), lat = c(46.03,46.034,46.034,46.034,46.015,46.015,46.015,46.02,46.025,46.027,46.03))
    XY4348 <- data.frame(lon = c(-63.277,-63.277,-63.285),lat = c(44.625,44.627,44.627))
    XY924 <- data.frame(lon = c(-61.055,-61.052,-61.05,-61.045,-61.045,-61.04,-61.035,-61.035,-61.055,-61.055),lat = c(45.495,45.495,45.495,45.495,45.49,45.49,45.49,45.495,45.5,45.502))
    XY11175 <- data.frame(lon = c(-59.795,-59.79), lat = c(46.007,46.007))
    XY965 <- data.frame(lon = c(-60.955,-60.955,-61.00,-61.00,-61.00,-61.005), lat = c(45.51,45.505,45.475,45.48,45.477,45.48))
    XY845<- data.frame(lon = c(-62.76,-62.765), lat = c(44.715,44.715))
    XY798 <- data.frame(lon = c(-61.89,-61.89,-61.89), lat = c(45.025,45.03,45.027))
    XY915 <- data.frame(lon = c(-62.8), lat = c(44.715))
    XY4087 <- data.frame(lon = c(-63.0), lat = c(44.705))


    new_land_coords <- rbind(new_land_coords,XY1878,XY461,XY4348,XY924,XY11175,XY965,XY845,XY798,XY915,XY4087)
    # Create a SpatVector from the coordinates
    new_land_points <- vect(new_land_coords, crs = crs(cost_surface))
    # Use extract() to find the cell numbers corresponding to the new land points
    land_cells <- cellFromXY(cost_surface, as.matrix(new_land_coords))
    # Set the identified cells to Inf (land)
    cost_surface[land_cells] <- Inf
    ##################################

    ##################################
    if(add.land){
    #### Import any additional saved land polygons drawn by user and add these as land:
    if(file.exists("C:/LOBTAG/data/new_land_coords.rds")){
    coords_table <- readRDS(file = "C:/LOBTAG/data/new_land_coords.rds")
    # Convert drawn polygons (in coords_table) to an sf object and then to SpatVector
    coords_list <- split(coords_table, coords_table$polygon_id)
    polygon_list <- lapply(coords_list, function(coords) {
      st_polygon(list(as.matrix(coords[, c("lon", "lat")])))
    })
    polygon_sf <- st_sfc(polygon_list, crs = st_crs(cost_surface))
    new_land_polygons <- vect(polygon_sf)
    # Mask the cost surface raster within the polygon areas
    cost_surface <- mask(cost_surface, new_land_polygons,inverse=T, updatevalue = Inf)
    }
      }

    # Convert SpatRaster to RasterLayer
    r <- as(cost_surface, "Raster")
    ## plot(r)

    ### having any starting points on land will cause a pathing error
    ### so need to check if any releases or recaptures fall on land in the raster

    x_rel_coords <- x[, c("rel_lon", "rel_lat")]
    x_rel_coords$rel_lon = as.numeric(x_rel_coords$rel_lon)
    x_rel_coords$rel_lat = as.numeric(x_rel_coords$rel_lat)
    rel_depth_values <- raster::extract(r, x_rel_coords)
    x$rel_depth <- rel_depth_values
    land_rels <- x %>% filter(rel_depth == Inf)

    x_rec_coords <- x[, c("rec_lon", "rec_lat")]
    x_rec_coords$rec_lon = as.numeric(x_rec_coords$rec_lon)
    x_rec_coords$rec_lat = as.numeric(x_rec_coords$rec_lat)
    rec_depth_values <- raster::extract(r, x_rec_coords)
    x$rec_depth <- rec_depth_values
    land_recs <- x %>% filter(rec_depth == Inf)

    ## assume that most land points are the result of raster resolution, not reporting error, and change these cells to water:
    land.rel.points <- land_rels %>% dplyr::select(rel_lon,rel_lat) %>% rename(x=rel_lon,y=rel_lat)
    land.rec.points <- land_recs %>% dplyr::select(rec_lon,rec_lat) %>% rename(x=rec_lon,y=rec_lat)
    land.points <- rbind(land.rel.points,land.rec.points)
    land.points$x = as.numeric(land.points$x)
    land.points$y = as.numeric(land.points$y)
    land_cells <- cellFromXY(r, land.points)
    r[land_cells] <- 1
    ## this only changes the immediate cell; if any coordinates are more than one cell inland, pathing will still fail

    ### begin pathing
    trans = NULL
    #tr <- transition(r, mean, neighborhood)
    tr <- transition(r, function(x) 1/mean(x), directions=neighborhood)
      if(type  == "random.walk"){
      trans = geoCorrection(tr, type = "r", scl=FALSE)
    }
    if(type  == "least.cost"){
      trans = geoCorrection(tr, type = "c", scl=FALSE)
    }
    ###############################################
    count = 1
    previd = ""

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

      ## if pathing fails at this point it's likely because the path crosses land, so handle error with message specifying the problem tag.
       tryCatch({

      AtoB <- NULL
        if(abs(start[1] - end[1]) < res(trans)[1] && abs(start[2] - end[2]) < res(trans)[1] || is.na(cellFromXY(r, start)) || is.na(cellFromXY(r, end))){
          AtoB = rbind(start, end)
        }else{
          AtoB = shortestPath(trans, start, end, output="SpatialLines")
        }

      if (is.null(AtoB)) {
        stop("Path calculation failed: result is NULL")
      }

      }, error = function(e) {
        base::message(paste0("Pathing failed at tag: ",x$PID[i],"! Likely because the path crosses land. Paths not created. Check release and recapture coordinates for this tag and try again."))
        break
      }, finally = {

      })



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
db_connection(db, oracle.user, oracle.password, oracle.dbname)


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

      }else{if(db %in% "local"){
        dbWriteTable(con, "LBT_PATH", df2towrite, append = TRUE, row.names = FALSE)
        dbWriteTable(con, "LBT_PATHS", dxtowrite, append = TRUE, row.names = FALSE)
        print("New paths calculated and written to paths tables.")
      }
        }

    dbDisconnect(con)
  }else{
    print("No new paths created.")  }

  ## include option to save a single spreadhseet of the data
  if(save.table){
    db_connection(db, oracle.user, oracle.password, oracle.dbname)
    query = paste0("SELECT * FROM LBT_PATH")
    path <- dbSendQuery(con, query)
    path <- fetch(path)
    query = paste0("SELECT * FROM LBT_PATHS")
    paths <- dbSendQuery(con, query)
    paths <- fetch(paths)
    dists <- path %>% dplyr::select(TID,CID,DIST)
    path.dat <- left_join(paths,dists)
    path.dat <- path.dat %>% mutate(DIST = ifelse(REC_DATE %in% NA,NA,DIST))
    dlg_message("Choose the directory where you want to save your path data table.")
    save.location <- dlg_dir(filter = dlg_filters["xls",])$res
    if(is.null(oracle.user))oracle.user <- ""
    openxlsx::write.xlsx(path.dat, file = paste0(save.location,"/",oracle.user,"_PATHS.xlsx"), rowNames = F)
    dbDisconnect(con)
  }


## troubleshooting code
# #
  # library(dplyr)
  # library(raster)
  # library(gdistance)
  # library(ROracle)
  # library(PBSmapping)
  # library(terra)
  # library(sf)
  # db = "local"
  # oracle.user = oracle.personal.user
  # oracle.password = oracle.personal.password
  # oracle.dbname = oracle.personal.server
  # tags = "XY965"
  # depth.raster.path = system.file("data", "gebco_2024.tif", package = "LobTag2")
  # neighborhood = 8
  # type = "least.cost"
  # regen.paths = T
  #a <- readRDS("C:/LOBTAG/data/new_land_coords.rds")

  ##generate_paths(db = "Oracle",tags = "XY461", regen.paths = T)
  ##generate_maps(db="Oracle",tag.IDs = "XY461")

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





