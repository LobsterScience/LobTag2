library(dplyr)
library(basemaps)
library(svDialogs)
library(shiny)

library(rgeos)
library(sp)
library(spatstat)
library(shadowtext)
library(ggplot2)
library(ggmap)
library(ggthemes)
library(ggrepel)
library(geosphere)
library(RStoolbox)
library(raster)
library(ggsflabel)

#' @title generate_maps
#' @import dplyr sf ggsflabel basemaps svDialogs ROracle DBI shinyjs shiny
#' @description creates maps of tag movement for participants
#' @export

generate_maps <- function(map_token = "pk.eyJ1IjoiZWxlbWVudGpha2UiLCJhIjoiY2xxZmh2bGFiMHhyNTJqcHJ0cDBqcW83ZiJ9.mDd49ObNcNdG6MzH3yc2dw",person=NULL)



  tryCatch({
    drv <- DBI::dbDriver("Oracle")
    conn <- ROracle::dbConnect(drv, username = oracle.personal.user, password = oracle.personal.password, dbname = oracle.personal.server)
  }, warning = function(w) {
  }, error = function(e) {
    return(toJSON("Connection failed"))
  }, finally = {
  })

## bring in paths
sql = paste("SELECT * FROM ELEMENTG.LBT_PATH")
path <- ROracle::dbSendQuery(conn, sql)
path <- ROracle::fetch(path)
sql = paste("SELECT * FROM ELEMENTG.LBT_PATHS")
paths <- ROracle::dbSendQuery(conn, sql)
paths <- ROracle::fetch(paths)

##recaptures
query = paste("SELECT * FROM ELEMENTG.LBT_RECAPTURES")
recaptures <- ROracle::dbSendQuery(conn, query)
rec <- ROracle::fetch(recaptures)

##releases
cap.samps <- paste0("('",paste(rec$TAG_ID, collapse ="','"),"')")
query = paste("SELECT * FROM ELEMENTG.LBT_RELEASES WHERE TAG_ID in",cap.samps)
releases <- ROracle::dbSendQuery(conn, query)
rel <- ROracle::fetch(releases)

##
ROracle::dbDisconnect(conn)

#### make table with all recaptures and their release information
## Prepare recaptures
rec <- rec %>% mutate(lat = LAT_DD) %>% mutate(lon = LON_DD)
rec <- rec %>% mutate(trip = paste(lat,lon,REC_DATE))
rec <- rec %>% mutate(REC_DATE = as.Date(REC_DATE, format= "%d/%m/%Y"))
rec <- rec %>% mutate(REC_DATE = format(REC_DATE, "%d-%b-%Y"))
rec <- rec %>% dplyr::select(TAG_ID,REC_DATE,YEAR,LAT_DD,LON_DD,PERSON,trip)
rec <- rec %>% rename(REC_YEAR = YEAR, REC_LAT= LAT_DD,REC_LON = LON_DD)

## Prepare releases
rel <- rel %>% rename(REL_LAT = LAT_DD, REL_LON = LON_DD, REL_YEAR = YEAR)
rel <- rel %>% mutate(REL_DATE = as.Date(REL_DATE, format= "%d/%m/%Y"))
rel <- rel %>% mutate(REL_DATE = format(REL_DATE, "%d-%b-%Y"))

dat <- left_join(rec,rel)


if(is.null(person)){print("No person chosen to make maps for")}else{

  #### load large base map for inset
  ## Allow user to choose data file to upload or manually draw extent
  result <- dlgMessage(type = "yesno", message = "Would you like to mannually draw the mapping area? If No, the default basemap will be used")
  if(result$res %in% "yes"){ext <- draw_ext()}else{ext <- readRDS("C:/bio/LobTag2/app.files/NS_extent")}

  set_defaults(ext, map_service = "mapbox",map_type = "satellite",
               map_token = map_token)

  inset <- basemap_raster(ext) #### forces basemap crs to be in 3857

  ## change back to 4326 (raster package has some masking issues with sf, so just use ::)
  inset <- raster::projectRaster(inset,  crs = 4326)

  ## start making data for selected person
  pers.dat <- dat %>% filter(PERSON %in% person)
  tags.dat <- dat %>% filter(TAG_ID %in% pers.dat$TAG_ID)

  ## link paths to specific events
  path.pers <- paths %>% filter(TID %in% pers.dat$TAG_ID)
  path.pers$CID = as.numeric(path.pers$CID)
  path.pers$POS = as.numeric(path.pers$POS)
  path.pers <- path.pers %>% arrange(TID,CID,POS)
  path.pers <- path.pers %>% group_by(TID,CID) %>% mutate(event = ifelse(POS==1,"release",
                                                                         ifelse(POS==max(POS),
                                                                                ,"pathing"))) %>% ungroup()
  #### Make sf objects
  sf_points <- sf::st_as_sf(path.pers, coords = c("LON","LAT"))
  sf::st_crs(sf_points) <- sf::st_crs(4326)  # EPSG code for WGS84
  ##loop through tags to create path lines
  path_list <- list()
  for(j in unique(sf_points$TID)){
    line_list <- list()
    a <- sf_points %>% filter(TID %in% j)
    for(i in unique(a$CID)){
      b <- a %>% filter(CID %in% i)
      coords <- sf::st_coordinates(b$geometry)
      lines <- sf::st_multilinestring(list(coords))
      line_list[[i]] <- lines
    }
    path_list[[j]] <- line_list
  }

  ##loop through each tag in path_list to generate maps for each
  for (i in names(path_list)){

    ## subset path list for current tag
    tag.path <- path_list[i]
    path_sf <- tag.path[[1]]
    path_sf <- sf::st_sfc(path_sf, crs=4326)
    sf::st_crs(path_sf) <- 4326

    #### define and load small basemap for actual mapping background
    ### set plotting region by tag
    path.tag <- path.pers %>% filter(TID %in% i)

    ##Make plotting region visually more square
    path.tag$LAT = as.numeric(path.tag$LAT)
    path.tag$LON = as.numeric(path.tag$LON)
    maxx =max(path.tag$LON)
    maxy = max(path.tag$LAT)
    minx = min(path.tag$LON)
    miny = min(path.tag$LAT)

    xlen = maxx - minx
    ylen = maxy - miny
    stretch.y=F
    stretch.x=F
    if(ylen-xlen>0.2){stretch.y=T
    stretch.y.factor = ylen-xlen}
    if(xlen-ylen>0.2){stretch.x=T}


    while(xlen < ylen){
      maxx = maxx+.01
      minx = minx-.01
      xlen =  maxx - minx
    }
    while(ylen < xlen){
      maxy = maxy+.01
      miny = miny-.01
      ylen =  maxy - miny
    }

    while(xlen < ylen){
      maxx = maxx+.001
      minx = minx-.001
      xlen =  maxx - minx
    }

    while(ylen < xlen){
      maxy = maxy+.001
      miny = miny-.001
      ylen =  maxy - miny
    }

    ##visually scale plotting area a bit wider
    scale = (maxy-miny)/120
    maxx = maxx+scale
    minx = minx-scale
    maxy = maxy+scale
    miny = miny-scale

    ##Expand region (stretch a bit on one axis if that range is very large)
    minx = minx - ylen/3
    maxx = maxx + ylen/3

    if(stretch.y){
      miny = miny - ylen/6
      maxy = maxy + ylen/6
    }
    if(stretch.x){
      minx = minx - ylen/6
      maxx = maxx + ylen/6
    }

    ylen = maxy-miny
    xlen = maxx-minx

    left <- minx
    right <- maxx
    top <- maxy
    bottom <- miny

    ## If just one point, set box size
    if(maxx == minx & maxy == miny){
      left = minx - 0.1
      right = maxx + 0.1
      top = maxy + 0.07
      bottom = miny - 0.07

      ylen = top-bottom
      xlen = right-left

      ext <- sf::st_polygon(list(matrix(c(left,bottom,right,bottom,right,top,left,top,left,bottom),ncol = 2, byrow = T)))
    }else{ ext <- sf::st_polygon(list(matrix(c(minx,miny,maxx,miny,maxx,maxy,minx,maxy,minx,miny),ncol = 2, byrow = T))) }


    ## get basemap
    ext_sf <- sf::st_sfc(ext, crs = 4326)
    ext_sf <- sf::st_sf(ext_sf)
    sf::st_crs(ext_sf) <- 4326

    set_defaults(ext_sf, map_service = "mapbox",map_type = "satellite",
                 map_token = "pk.eyJ1IjoiZWxlbWVudGpha2UiLCJhIjoiY2xxZmh2bGFiMHhyNTJqcHJ0cDBqcW83ZiJ9.mDd49ObNcNdG6MzH3yc2dw")

    base <- basemap_raster(ext_sf) #### forces basemap crs to be in 3857

    ## change back to 4326 (raster package has some masking issues with sf, so just use ::)
    base <- raster::projectRaster(base,  crs = 4326)
    limits <- sf::st_bbox(base)


    ## get labels
    rel.lab <- pers.dat %>% filter(TAG_ID  %in% i) %>% summarise(TAG_ID = first(TAG_ID),DATE = first(REL_DATE), YEAR = first(REL_YEAR) ,LAT = first(REL_LAT), LON = first(REL_LON))
    rel.lab <- rel.lab %>% mutate(name = paste("Tag:",TAG_ID,DATE))
    rel.sf <- sf::st_as_sf(rel.lab, coords = c("LON","LAT"))
    sf::st_crs(rel.sf) <- sf::st_crs(4326)

    rec.lab <- pers.dat %>% filter(TAG_ID  %in% i) %>% dplyr::select(TAG_ID,REC_DATE,REC_YEAR,REC_LAT,REC_LON) %>% mutate(name = paste("You Captured:",REC_DATE))
    rec.sf <- sf::st_as_sf(rec.lab, coords = c("REC_LON","REC_LAT"))
    sf::st_crs(rec.sf) <- sf::st_crs(4326)


    outplot <- gg_raster(base)+
      geom_sf(data=path_sf, colour = "red", linewidth=1.8, arrow = arrow(type = "open", length = unit(0.1, "inches")))+
      geom_sf(data=rel.sf, colour = "yellow")+
      geom_sf(data=rec.sf, colour = "yellow")+
      geom_sf_label_repel(data = rel.sf, aes(label=name, colour="blue"),show.legend=F,nudge_y=0, alpha=0.8,max.overlaps = 20, size=4)+
      geom_sf_label_repel(data = rec.sf, aes(label=name, colour="blue"),show.legend=F,nudge_y=0, alpha=0.8,max.overlaps = 20, size=4)+
      coord_sf(ylim=as.numeric(c(limits$ymin,limits$ymax)), xlim = as.numeric(c(limits$xmin,limits$xmax)), expand = F)

    ggsave(filename = paste0(person,i,".pdf"), path = "C:/Users/ELEMENTG/Documents/Rsaves", plot = outplot, width = 11, height = 10)
  }



}

