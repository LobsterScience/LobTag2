
#' @title generate_maps
#' @import dplyr sf ggplot2 ggsflabel basemaps svDialogs RSQLite DBI raster
#' @description creates maps of tag movement for participants
#' @export

generate_maps <- function(people=NULL, all.people = FALSE, tag.IDs = NULL, all.tags = FALSE, map.token = mapbox.token, db = "local", output.location = NULL,
                          oracle.user = oracle.personal.user, oracle.password = oracle.personal.password,
                          oracle.dbname = oracle.personal.server){

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
  result <- dlgMessage(type = "yesno", message = "Would you like to manually draw the area for the inset map? If No, the inset map will be sized automatically")

  ### open db connection
  if(db %in% "Oracle"){
    tryCatch({
      drv <- DBI::dbDriver("Oracle")
      conn <- ROracle::dbConnect(drv, username = oracle.user, password = oracle.password, dbname = oracle.dbname)
    }, warning = function(w) {
    }, error = function(e) {
      return(toJSON("Connection failed"))
    }, finally = {
    })
  }else{
    conn <- dbConnect(RSQLite::SQLite(), "C:/LOBTAG/LOBTAG.db")
  }



## bring in paths
sql = paste0("SELECT * FROM LBT_PATH")
path <- dbSendQuery(conn, sql)
path <- fetch(path)
sql = paste0("SELECT * FROM LBT_PATHS")
paths <- dbSendQuery(conn, sql)
paths <- fetch(paths)

## can't seem to trust Oracle to maintain sorting for all tag events, so do a safety re-sort
paths <- paths %>% arrange(TID,as.numeric(CID),as.numeric(POS))

if(all.people){
  ## get all names who've recaptured tags
  sql = paste0("SELECT * FROM LBT_RECAPTURES")
  rec <- dbSendQuery(conn, sql)
  rec <- fetch(rec)
  people <- unique(rec$PERSON)
}
if(all.tags){
  ## get all names who've recaptured tags
  sql = paste0("SELECT * FROM LBT_RECAPTURES")
  rec <- dbSendQuery(conn, sql)
  rec <- fetch(rec)
  tags <- unique(rec$TAG_ID)
}

##
dbDisconnect(conn)

if(!is.null(tag.IDs)){
  paths <- paths %>% filter(TID %in% tag.IDs)
  people <- unique((paths %>% filter(!REC_PERSON %in% NA))$REC_PERSON)
}

if(is.null(people) & is.null(tag.IDs)){base::message("No tags or people chosen to make maps for!")}
## loops if there's more than one person
for (p in people){
  person = p

if(is.null(person)){base::message("No person chosen to make maps for!")}else{

  ### filter path data for chosen tags or tags recaptured by chosen person
  path.pers <- paths %>% filter(TID %in% (paths %>% filter(REC_PERSON %in% person))$TID)

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
                 map_token = map.token)

    base <- basemap_raster(ext_sf) #### forces basemap crs to be in 3857

    ## change back to 4326 (raster package has some masking issues with sf, so just use ::)
    base <- raster::projectRaster(base,  crs = 4326)

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
                 map_token = map.token)

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
      a <- gg_raster(base, maxpixels=400000)+
        ggtitle(paste(person,"-",i))+
        geom_sf(data=path_sf, colour = "red", linewidth=1.6, arrow = arrow(type = "open", length = unit(0.3, "inches")))+
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
    inset.right = right+xlen/10
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












