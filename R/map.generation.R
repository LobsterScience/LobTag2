
#' @title generate_maps
#' @import dplyr sf ggplot2 ggsflabel basemaps svDialogs RSQLite DBI raster ggspatial
#' @description creates maps of tag movement for participants
#' @export

generate_maps <- function(db = NULL, people=NULL, all.people = FALSE, tags = NULL, all.tags = FALSE, only.unrewarded = FALSE, map.token = mapbox.token, output.location = NULL,
                          max.pixels = 800000, map.res = 0.9, inset.option = T,
                          oracle.user =if(exists("oracle.personal.user")) oracle.personal.user else NULL,
                          oracle.password = if(exists("oracle.personal.password")) oracle.personal.password else NULL,
                          oracle.dbname = if(exists("oracle.personal.server")) oracle.personal.server else NULL){

  if(db %in% c("local","Local","LOCAL")){
    db = "local"
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
####################################################### Main Function:



  ## let user select output file location for maps
  if(is.null(output.location)){
    dlg_message("In the following window, choose the directory where you want to send your maps.")
    output.location <- dlg_dir(filter = dlg_filters["csv",])$res
  }


  ## Allow user to choose whether or not to manually draw inset map
  inset.result = NULL
  if(inset.option){
    inset.result <- dlgMessage(type = "yesno", message = "Would you like to manually draw the area for the inset map? If No, the inset map will be sized automatically")
  }

  ### open db connection
db_connection()



## bring in paths (and recaptures for reference)
sql = paste0("SELECT * FROM LBT_PATH")
path <- dbSendQuery(con, sql)
path <- fetch(path)
sql = paste0("SELECT * FROM LBT_PATHS")
paths <- dbSendQuery(con, sql)
paths <- fetch(paths)
sql = paste0("SELECT * FROM LBT_RECAPTURES")
rec <- dbSendQuery(con, sql)
rec <- fetch(rec)

##
dbDisconnect(con)

## can't seem to trust Oracle to maintain sorting for all tag events, so do a safety re-sort
paths <- paths %>% arrange(TID,as.numeric(CID),as.numeric(POS))

## filtering arguments

if(all.people){
  ## get all names who've recaptured tags
  people <- unique(rec$PERSON)
}
if(all.tags){
  ## get all tags that have been recaptured
  tags <- unique(rec$TAG_ID)
}

if(!is.null(tags)){
  paths <- paths %>% filter(TID %in% tags)
  people <- unique((paths %>% filter(!REC_PERSON %in% NA))$REC_PERSON)
}

if(is.null(people) & is.null(tags)){base::message("No tags or people chosen to make maps for!")}else{

## loops if there's more than one person
for (p in people){
  person = p

  ### filter path data for chosen tags or tags recaptured by chosen person
  path.pers <- paths %>% filter(TID %in% (paths %>% filter(REC_PERSON %in% person))$TID)

  ## if user chooses to only make maps for those people who haven't received rewards:
  if(only.unrewarded){
    rec.unrewarded <- rec %>% filter(PERSON %in% p, REWARDED %in% "no")
    path.pers <- path.pers %>% filter(TID %in% rec.unrewarded$TAG_ID)
  }

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



  ####loop through each tag in path_list to generate maps for each ##################################################
  for (i in names(path_list)){

    ## subset path list for current tag
    tag.path <- path_list[i]
    path_sf <- tag.path[[1]]
    path_sf <- sf::st_sfc(path_sf, crs=4326)
    sf::st_crs(path_sf) <- 4326

    #### define and load small basemap for actual mapping background
    ### set plotting region by tag
    path.tag <- path.pers %>% filter(TID %in% i)

    ##Make plotting region more square
    path.tag$LAT = as.numeric(path.tag$LAT)
    path.tag$LON = as.numeric(path.tag$LON)
    maxx =max(path.tag$LON)
    maxy = max(path.tag$LAT)
    minx = min(path.tag$LON)
    miny = min(path.tag$LAT)
    xlen = maxx - minx
    ylen = maxy - miny


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


    #stretch on x axis for visual squareness
    minx = minx - ylen/3
    maxx = maxx + ylen/3

      ##visually scale plotting area a bit wider
      scale = (maxy-miny)/3
      maxx = maxx+scale
      minx = minx-scale
      maxy = maxy+scale
      miny = miny-scale


    if(ylen <0.19){## extra limit to prevent zoom from getting too close where there are no tiles in some areas

      while(ylen < 0.19){
        maxy = maxy+.01
        miny = miny-.01
        ylen =  maxy - miny
      }

      while(xlen < ylen){
        maxx = maxx+.01
        minx = minx-.01
        xlen =  maxx - minx
      }

      while(ylen < 0.19){
        maxy = maxy+.001
        miny = miny-.001
        ylen =  maxy - miny
      }

      while(xlen < ylen){
        maxx = maxx+.001
        minx = minx-.001
        xlen =  maxx - minx
      }

      #stretch again on x axis for visual squareness
      minx = minx - ylen/3
      maxx = maxx + ylen/3

      }



      ## If just one point, set box size
    if(maxx == minx & maxy == miny){
      minx = minx - 0.1
      maxx = maxx + 0.1
      maxy = maxy + 0.07
      miny = miny - 0.07

      ylen = maxy-miny
      xlen = maxx-minx
    }
    ext <- sf::st_polygon(list(matrix(c(minx,miny,maxx,miny,maxx,maxy,minx,maxy,minx,miny),ncol = 2, byrow = T)))

    ## get basemap
    ext_sf <- sf::st_sfc(ext, crs = 4326)
    ext_sf <- sf::st_sf(ext_sf)
    sf::st_crs(ext_sf) <- 4326

    set_defaults(ext_sf, map_service = "mapbox",map_type = "satellite",
                 map_token = map.token, map_dir = tempdir())

    base <- basemap_raster(ext_sf, map_res = map.res) #### forces basemap crs to be in 3857

    ## change back to 4326 (raster package has some masking issues with sf, so just use ::)
    base <- raster::projectRaster(base,  crs = 4326)

    ## retrieve large inset map. Can choose to draw manually, otherwise will be autosized relative to basemap
    if(!is.null(inset.result) && inset.result$res %in% "yes"){ext.inset <- draw_ext()}else{

      expand = 5
      ## if the mapping area is very small, keep increasing expand factor until inset size is meaningful (at least about 2 degrees of longitude)
      while(((maxx+(xlen*expand))-(minx-(xlen*expand)))<1.9){expand = expand+0.1}

      ext.inset <- sf::st_polygon(list(matrix(c(minx-(xlen*expand),miny-(ylen*expand),maxx+(xlen*expand),miny-(ylen*expand),maxx+(xlen*expand),maxy+(ylen*expand),minx-(xlen*expand),maxy+(ylen*expand),minx-(xlen*expand),miny-(ylen*expand)),ncol = 2, byrow = T)))
      ext.inset_sf <- sf::st_sfc(ext.inset, crs = 4326)
      ext.inset_sf <- sf::st_sf(ext.inset_sf)
      sf::st_crs(ext.inset_sf) <- 4326
      }

    set_defaults(ext.inset, map_service = "mapbox",map_type = "satellite",
                 map_token = map.token, map_dir = tempdir())

    inset <- basemap_raster(ext.inset_sf, map_res = 0.9) #### mapbox's default datum is mercator (3857)

    ## reproject raster to change back to 4326, this can cause some colour problems when graphing later (raster package has some masking issues with sf, so just use ::)
    inset <- raster::projectRaster(inset,  crs = 4326)

    ## get dimensions information of base and inset for graph placement
    limits <- sf::st_bbox(base)
    ylen = as.numeric(limits$ymax-limits$ymin)
    xlen = as.numeric(limits$xmax-limits$xmin)
    left <- as.numeric(limits$xmin)
    right <- as.numeric(limits$xmax)
    top <- as.numeric(limits$ymax)
    bottom <- as.numeric(limits$ymin)

    inset.lim <- sf::st_bbox(inset)
    inset.ylen = as.numeric(inset.lim$ymax-inset.lim$ymin)
    inset.xlen = as.numeric(inset.lim$xmax-inset.lim$xmin)
    inset.ratio = inset.xlen/inset.ylen

    ## get labels
    rel.lab <- path.pers %>% filter(TID  %in% i) %>% summarise(TID = first(TID),DATE = first(REL_DATE),LAT = first(LAT), LON = first(LON))
    rel.lab <- rel.lab %>% mutate(name = paste("Tag:",TID,DATE))
    rec.lab <- path.pers %>% filter(TID  %in% i, !REC_PERSON %in% c(NA,"NA")) %>% dplyr::select(TID,REC_DATE,LAT,LON,REC_PERSON)
    rec.lab <- rec.lab %>% mutate(name = ifelse(REC_PERSON %in% person,paste("You Captured:",REC_DATE),paste("Other Fisher Captured:",REC_DATE)))

    map.labs <- rbind(rel.lab %>% dplyr::select(TID,LAT,LON,name),rec.lab %>% dplyr::select(TID,LAT,LON,name))

    labs.sf <- sf::st_as_sf(map.labs, coords = c("LON","LAT"))
    sf::st_crs(labs.sf) <- sf::st_crs(4326)

    ## graphing
    ## since reprojecting the raster may have created problems with colour values, try normalizing these if graphing doesn't work the first time

    ## function to normalize the colour values of a reprojected raster brick object for graphing with with gg_raster or other raster graphing functions
    normalize_raster_brick <- function(raster_brick) {
      # Ensure the input is a raster brick
      if (!inherits(raster_brick, "RasterBrick")) {
        stop("The input must be a RasterBrick object.")
      }

      # Extract the individual bands
      red_band <- raster_brick[[1]]
      green_band <- raster_brick[[2]]
      blue_band <- raster_brick[[3]]

      # Get the values from each band
      red_values <- raster::getValues(red_band)
      green_values <- raster::getValues(green_band)
      blue_values <- raster::getValues(blue_band)

      # Normalize the values to the range [0, 1]
      red_values_normalized <- (red_values - min(red_values, na.rm = TRUE)) / (max(red_values, na.rm = TRUE) - min(red_values, na.rm = TRUE))
      green_values_normalized <- (green_values - min(green_values, na.rm = TRUE)) / (max(green_values, na.rm = TRUE) - min(green_values, na.rm = TRUE))
      blue_values_normalized <- (blue_values - min(blue_values, na.rm = TRUE)) / (max(blue_values, na.rm = TRUE) - min(blue_values, na.rm = TRUE))

      # Replace the original values in the bands with the normalized values
      values(red_band) <- red_values_normalized
      values(green_band) <- green_values_normalized
      values(blue_band) <- blue_values_normalized

      # Combine the normalized bands back into a raster brick
      normalized_raster_brick <- brick(red_band, green_band, blue_band)

      return(normalized_raster_brick)
    }

    ## main map
      base <- normalize_raster_brick(base)
      ## For arrow head sizing:
      if(scale< 0.03){arrow.size = scale*10}else{arrow.size =0.3}

      a <- gg_raster(base, maxpixels=max.pixels)+
        ggspatial::annotation_scale(data = path_sf, bar_cols = c("grey", "white"), text_col = "white")+
        ggtitle(paste(person,"-",i))+
        geom_sf(data=path_sf, colour = "red", linewidth=1.6, arrow = arrow(type = "open", length = unit(arrow.size, "inches")))+
        geom_sf(data=labs.sf, colour = "yellow")+
        geom_sf_label_repel(data = labs.sf, aes(label=name),colour="blue",show.legend=F,nudge_y=0, alpha=0.8,max.overlaps = 20, size=4)+
        coord_sf(ylim=as.numeric(c(limits$ymin,limits$ymax)), xlim = as.numeric(c(limits$xmin,limits$xmax)), expand = F)+
        theme(plot.margin = margin(t = 73))


    ## inset map
      inset <- normalize_raster_brick(inset)
      b <- gg_raster(inset, maxpixels=300000)+
        geom_sf(data = ext, colour = "red", alpha = 0.1)+
        coord_sf(expand = F)+
        theme(axis.title = element_blank(),
              axis.text  = element_blank(),
              axis.ticks = element_blank(),
              plot.margin = unit(c(0.2,0.1,0.0,0.0),'lines'))


    b1 <- ggplotGrob(b)

    ##set positioning of inset while maintaining its ratios
    inset.top = top+ylen/5
    inset.bottom = inset.top-ylen/3
    inset.height = inset.top-inset.bottom
    inset.right = right+xlen/30
    inset.width = inset.height*inset.ratio

      outplot <- a +
        annotation_custom(grob=b1, xmin = inset.right-inset.width, xmax = inset.right, ymin = inset.bottom, ymax = inset.top)
        #annotation_custom(grob=b1, xmin = unit(0.5, "npc") - unit(0.2, "npc"), xmax = unit(1, "npc"), ymin = unit(1, "npc") - unit(0.2, "npc"), ymax = unit(1, "npc"))
      # annotation_custom(grob=b1, xmin = right-ylen/2, xmax = right+ylen/25, ymax = top+ylen/5, ymin = top-ylen/3)
      ggsave(filename = paste0(person,i,".pdf"), path = output.location, plot = outplot, width = 11, height = 10)


  }

}
}

}








#' @title map_by_factor
#' @import dplyr sf ggplot2 ggsflabel basemaps svDialogs RSQLite DBI raster ggspatial
#' @description specific mapping function for mapping releases and returns by custom factor
#' @export

map_by_factor <- function(db = NULL, factor.from = NULL, map.by=NULL, filter.maps.for=NULL, group.by=NULL, all.releases = F,
                          tag.prefix = NULL, add.paths = F, map.token = mapbox.token, output.location = NULL,
                          set.output = T, set.inset=T,max.pixels = 800000, map.res = 0.9, inset.map=T,
                          oracle.user =if(exists("oracle.personal.user")) oracle.personal.user else NULL,
                          oracle.password = if(exists("oracle.personal.password")) oracle.personal.password else NULL,
                          oracle.dbname = if(exists("oracle.personal.server")) oracle.personal.server else NULL){

  if(db %in% c("local","Local","LOCAL")){
    db = "local"
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
  ####################################################### Main Function:


  if(set.output){
    ## let user select output file location for maps
    if(is.null(output.location)){
      dlg_message("In the following window, choose the directory where you want to send your maps.")
      output.location <- dlg_dir(filter = dlg_filters["csv",])$res
    }
  }
  if(set.inset){
    ## Allow user to choose whether or not to manually draw inset map
    result <- dlgMessage(type = "yesno", message = "Would you like to manually draw the area for the inset map? If No, the inset map will be sized automatically")
  }

  ### open db connection
db_connection()

  query = ifelse(is.null(tag.prefix),"SELECT * FROM LBT_RECAPTURES",paste0("SELECT * FROM LBT_RECAPTURES where TAG_PREFIX= ","'",tag.prefix,"'"))
  recaptures <- dbSendQuery(con, query)
  recaptures <- fetch(recaptures)

  cap.tags <- recaptures$TAG_ID
  chosen.str <- paste0("('",paste(cap.tags, collapse ="','"),"')")
  query = ifelse(all.releases,"SELECT * FROM LBT_RELEASES", paste0("SELECT * FROM LBT_RELEASES where TAG_ID in ",chosen.str))
  releases <- dbSendQuery(con, query)
  releases <- fetch(releases)

  select.factors <- c("TAG_ID")    ### minimal information that can be included
  tab <- sub(paste0(":", ".*"), "", factor.from)

  if(!is.null(map.by)){
    map_fact <- sub(paste0(".*", ":"), "", map.by)
    releases <- releases %>% rename("map_fact" = all_of(map_fact))
    recaptures <- recaptures %>% rename("map_fact" = all_of(map_fact))
    select.factors <- c(select.factors,"map_fact")

  }else{
    ### defaults to whole dataset if no map.by is chosen
    releases$map_fact = "all"
    recaptures$map_fact = "all"
    select.factors <- c(select.factors,"map_fact")
    }

  if(!is.null(group.by)){
    select.factors <- c(select.factors,group.by)
  }

  if(tab %in% c("releases","RELEASES")){
    rel.dat <- releases %>% dplyr::select(all_of(select.factors),"LAT_DD","LON_DD","REL_DATE")
    rel <- releases %>% dplyr::select(all_of(select.factors))
    rec <- left_join(recaptures,rel)
    rec.dat <- rec %>% dplyr::select(select.factors,"LAT_DD","LON_DD","REC_DATE")
    loop.factor = rel.dat$map_fact
    }
  if(tab %in% c("recaptures","RECAPTURES")){
    rec.dat <- recaptures %>% dplyr::select(all_of(select.factors),"LAT_DD","LON_DD","REC_DATE")
    rec <- recaptures %>% dplyr::select(all_of(select.factors))
    rel<- left_join(releases,rec)
    rel.dat <- rel %>% dplyr::select(select.factors,"LAT_DD","LON_DD","REL_DATE")
    loop.factor = rec.dat$map_fact
  }


  if(add.paths){
    ## bring in paths
    sql = paste0("SELECT * FROM LBT_PATH")
    path <- dbSendQuery(con, sql)
    path <- fetch(path)
    sql = paste0("SELECT * FROM LBT_PATHS")
    paths <- dbSendQuery(con, sql)
    paths <- fetch(paths)

    ## can't seem to trust Oracle to maintain sorting for all tag events, so do a safety re-sort
    paths <- paths %>% arrange(TID,as.numeric(CID),as.numeric(POS))
  }


  ##
  dbDisconnect(con)

  ## include optional filter for specific value of mapping factor
  if(!is.null(filter.maps.for)){
    rec.dat <- rec.dat %>% filter(map_fact %in% filter.maps.for)
  }

  for(i in unique(loop.factor)){

    ##point geometry
    rel.points <- rel.dat %>% filter(map_fact %in% i)
    rec.points <- rec.dat %>% filter(map_fact %in% i)

    sf_rel <- sf::st_as_sf(rel.points, coords = c("LON_DD","LAT_DD"))
    sf::st_crs(sf_rel) <- sf::st_crs(4326)

    sf_rec <- sf::st_as_sf(rec.points, coords = c("LON_DD","LAT_DD"))
    sf::st_crs(sf_rec) <- sf::st_crs(4326)
    ## path geometry
    if(add.paths){
      path.choose <-  paths %>% filter(TID  %in% rec.points$TAG_ID)
      sf_paths <- sf::st_as_sf(path.choose, coords = c("LON","LAT"))
      sf::st_crs(sf_paths) <- sf::st_crs(4326)  # EPSG code for WGS84
      ##loop through tags to create path lines
      lines_list <- list()
      for(j in unique(sf_paths$TID)){
        a <- sf_paths %>% filter(TID %in% j)
        line_list <- list()
        for(k in unique(a$CID)){
          b <- a %>% filter(CID %in% k)
          coords <- sf::st_coordinates(b$geometry)
          lines <- sf::st_multilinestring(list(coords))
        }
        lines_list[[j]] <- lines
      }

      path_sf <- sf::st_sfc(lines_list, crs=4326)
      sf::st_crs(path_sf) <- 4326
    }

    ## Set mapping area
    maxx = max(as.numeric(rec.points$LON_DD),as.numeric(rel.points$LON_DD))
    minx = min(as.numeric(rec.points$LON_DD),as.numeric(rel.points$LON_DD))
    maxy = max(as.numeric(rec.points$LAT_DD),as.numeric(rel.points$LAT_DD))
    miny = min(as.numeric(rec.points$LAT_DD),as.numeric(rel.points$LAT_DD))

    ##Make plotting region more square
    xlen = maxx - minx
    ylen = maxy - miny

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


    #stretch on x axis for visual squareness
    minx = minx - ylen/3
    maxx = maxx + ylen/3

    ##visually scale plotting area a bit wider
    scale = (maxy-miny)/6
    maxx = maxx+scale
    minx = minx-scale
    maxy = maxy+scale
    miny = miny-scale

    ## If just one point, set box size
    if(maxx == minx & maxy == miny){
      minx = minx - 0.1
      maxx = maxx + 0.1
      maxy = maxy + 0.07
      miny = miny - 0.07

      ylen = maxy-miny
      xlen = maxx-minx
    }
    ext <- sf::st_polygon(list(matrix(c(minx,miny,maxx,miny,maxx,maxy,minx,maxy,minx,miny),ncol = 2, byrow = T)))

    ## get basemap
    ext_sf <- sf::st_sfc(ext, crs = 4326)
    ext_sf <- sf::st_sf(ext_sf)
    sf::st_crs(ext_sf) <- 4326

    set_defaults(ext_sf, map_service = "mapbox",map_type = "satellite",
                 map_token = map.token, map_dir = tempdir())

    base <- basemap_raster(ext_sf, map_res = map.res) #### forces basemap crs to be in 3857

    ## change back to 4326 (raster package has some masking issues with sf, so just use ::)
    base <- raster::projectRaster(base,  crs = 4326)

    if(inset.map){
    ## retrieve large inset map. Can choose to draw manually, otherwise will be autosized relative to basemap
    if(result$res %in% "yes"){ext.inset <- draw_ext()}else{

      expand = 5
      ## if the mapping area is very small, keep increasing expand factor until inset size is meaningful (at least about 2 degrees of longitude)
      while(((maxx+(xlen*expand))-(minx-(xlen*expand)))<1.9){expand = expand+0.1}

      ext.inset <- sf::st_polygon(list(matrix(c(minx-(xlen*expand),miny-(ylen*expand),maxx+(xlen*expand),miny-(ylen*expand),maxx+(xlen*expand),maxy+(ylen*expand),minx-(xlen*expand),maxy+(ylen*expand),minx-(xlen*expand),miny-(ylen*expand)),ncol = 2, byrow = T)))
      ext.inset_sf <- sf::st_sfc(ext.inset, crs = 4326)
      ext.inset_sf <- sf::st_sf(ext.inset_sf)
      sf::st_crs(ext.inset_sf) <- 4326
    }

    set_defaults(ext.inset, map_service = "mapbox",map_type = "satellite",
                 map_token = map.token, map_dir = tempdir())

    inset <- basemap_raster(ext.inset_sf, map_res = 0.9) #### mapbox's default datum is mercator (3857)

    ## reproject raster to change back to 4326, this can cause some colour problems when graphing later (raster package has some masking issues with sf, so just use ::)
    inset <- raster::projectRaster(inset,  crs = 4326)

    ##get dimensions infromation of inset for graph placement
    inset.lim <- sf::st_bbox(inset)
    inset.ylen = as.numeric(inset.lim$ymax-inset.lim$ymin)
    inset.xlen = as.numeric(inset.lim$xmax-inset.lim$xmin)
    inset.ratio = inset.xlen/inset.ylen


    }

    ## get dimensions information of base
    limits <- sf::st_bbox(base)
    ylen = as.numeric(limits$ymax-limits$ymin)
    xlen = as.numeric(limits$xmax-limits$xmin)
    left <- as.numeric(limits$xmin)
    right <- as.numeric(limits$xmax)
    top <- as.numeric(limits$ymax)
    bottom <- as.numeric(limits$ymin)

    ## graphing
    ## since reprojecting the raster may have created problems with colour values, try normalizing these if graphing doesn't work the first time

    ## function to normalize the colour values of a reprojected raster brick object for graphing with with gg_raster or other raster graphing functions
    normalize_raster_brick <- function(raster_brick) {
      # Ensure the input is a raster brick
      if (!inherits(raster_brick, "RasterBrick")) {
        stop("The input must be a RasterBrick object.")
      }

      # Extract the individual bands
      red_band <- raster_brick[[1]]
      green_band <- raster_brick[[2]]
      blue_band <- raster_brick[[3]]

      # Get the values from each band
      red_values <- raster::getValues(red_band)
      green_values <- raster::getValues(green_band)
      blue_values <- raster::getValues(blue_band)

      # Normalize the values to the range [0, 1]
      red_values_normalized <- (red_values - min(red_values, na.rm = TRUE)) / (max(red_values, na.rm = TRUE) - min(red_values, na.rm = TRUE))
      green_values_normalized <- (green_values - min(green_values, na.rm = TRUE)) / (max(green_values, na.rm = TRUE) - min(green_values, na.rm = TRUE))
      blue_values_normalized <- (blue_values - min(blue_values, na.rm = TRUE)) / (max(blue_values, na.rm = TRUE) - min(blue_values, na.rm = TRUE))

      # Replace the original values in the bands with the normalized values
      values(red_band) <- red_values_normalized
      values(green_band) <- green_values_normalized
      values(blue_band) <- blue_values_normalized

      # Combine the normalized bands back into a raster brick
      normalized_raster_brick <- brick(red_band, green_band, blue_band)

      return(normalized_raster_brick)
    }

    ## main map
    base <- normalize_raster_brick(base)

    a <- gg_raster(base, maxpixels=max.pixels)+
      ggspatial::annotation_scale(data = sf_rel, bar_cols = c("grey", "white"), text_col = "white")+
      ggtitle(paste(map.by,"-",i))
    if(add.paths){
      p <- geom_sf(data = path_sf, colour = "yellow")
      a <- a+p
    }
if(is.na(group.by)){
  a <- a+geom_sf(data=sf_rec, size=1.5, aes(color = "Recaptures"))+
    geom_sf(data=sf_rel, size=1.5,aes(colour = "Releases"))+
    coord_sf(ylim=as.numeric(c(limits$ymin,limits$ymax)), xlim = as.numeric(c(limits$xmin,limits$xmax)), expand = F)+
    theme(plot.margin = margin(t = 73))
}else{
  if(tab %in% c("releases","RELEASES")){
    a <- a+
      geom_sf(data=sf_rel, size=1.5,aes(colour = .data[[group.by]]))+
      coord_sf(ylim=as.numeric(c(limits$ymin,limits$ymax)), xlim = as.numeric(c(limits$xmin,limits$xmax)), expand = F)+
      theme(plot.margin = margin(t = 73))
  }
  if(tab %in% c("recaptures","RECAPTURES")){
    a <- a+
      geom_sf(data=sf_rec, size=1.5,aes(colour = .data[[group.by]]))+
      coord_sf(ylim=as.numeric(c(limits$ymin,limits$ymax)), xlim = as.numeric(c(limits$xmin,limits$xmax)), expand = F)+
      theme(plot.margin = margin(t = 73))
  }
}


if(inset.map){
    ## inset map
    inset <- normalize_raster_brick(inset)
    b <- gg_raster(inset, maxpixels=300000)+
      geom_sf(data = ext, colour = "red", alpha = 0.1)+
      coord_sf(expand = F)+
      theme(axis.title = element_blank(),
            axis.text  = element_blank(),
            axis.ticks = element_blank(),
            plot.margin = unit(c(0.2,0.1,0.0,0.0),'lines'))


    b1 <- ggplotGrob(b)

    ##set positioning of inset while maintaining its ratios
    inset.top = top+ylen/5
    inset.bottom = inset.top-ylen/3
    inset.height = inset.top-inset.bottom
    inset.right = right+xlen/10
    inset.width = inset.height*inset.ratio

    outplot <- a +
      annotation_custom(grob=b1, xmin = inset.right-inset.width, xmax = inset.right, ymin = inset.bottom, ymax = inset.top)
}else{outplot <- a}
    #annotation_custom(grob=b1, xmin = unit(0.5, "npc") - unit(0.2, "npc"), xmax = unit(1, "npc"), ymin = unit(1, "npc") - unit(0.2, "npc"), ymax = unit(1, "npc"))
    # annotation_custom(grob=b1, xmin = right-ylen/2, xmax = right+ylen/25, ymax = top+ylen/5, ymin = top-ylen/3)
    name.by =ifelse(!is.null(map.by),map_fact,"")
    ggsave(filename = paste0(tab," ",name.by," ",i,".pdf"), path = output.location, plot = outplot, width = 11, height = 10)


  }



  # library(dplyr)
  # library(sf)
  # library(ggplot2)
  # library(ggsflabel)
  # library(basemaps)
  # library(svDialogs)
  # library(RSQLite)
  # library(DBI)
  # library(raster)





}








