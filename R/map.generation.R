
#' @title generate_maps
#' @import dplyr sf ggplot2 ggsflabel basemaps svDialogs ROracle DBI
#' @description creates maps of tag movement for participants
#' @export

generate_maps <- function(map.token = "pk.eyJ1IjoiZWxlbWVudGpha2UiLCJhIjoiY2xxZmh2bGFiMHhyNTJqcHJ0cDBqcW83ZiJ9.mDd49ObNcNdG6MzH3yc2dw",
                          person=NULL, output.location = "C:/Users/ELEMENTG/Documents/Rsaves", all.people = FALSE,
                          username = oracle.personal.user, password = oracle.personal.password, dbname = oracle.personal.server){

  oracle.personal.user<<-username
  oracle.personal.password<<-password
  oracle.personal.server<<-dbname

  #### load large base map for inset
  ## Allow user to choose data file to upload or manually draw extent
  result <- dlgMessage(type = "yesno", message = "Would you like to manually draw the area for the inset map? If No, the default basemap will be used")
  if(result$res %in% "yes"){ext <- draw_ext()}else{ext <- readRDS(system.file("data", "NS_extent", package = "LobTag2"))}

  set_defaults(ext, map_service = "mapbox",map_type = "satellite",
               map_token = map.token)

  inset <- basemap_raster(ext) #### forces basemap crs to be in 3857

  ## change back to 4326 (raster package has some masking issues with sf, so just use ::)
  inset <- raster::projectRaster(inset,  crs = 4326)



  tryCatch({
    drv <- DBI::dbDriver("Oracle")
    conn <- ROracle::dbConnect(drv, username = oracle.personal.user, password = oracle.personal.password, dbname = oracle.personal.server)
  }, warning = function(w) {
  }, error = function(e) {
    return(toJSON("Connection failed"))
  }, finally = {
  })

## bring in paths
sql = paste0("SELECT * FROM ",oracle.personal.user,".LBT_PATH")
path <- ROracle::dbSendQuery(conn, sql)
path <- ROracle::fetch(path)
sql = paste0("SELECT * FROM ",oracle.personal.user,".LBT_PATHS")
paths <- ROracle::dbSendQuery(conn, sql)
paths <- ROracle::fetch(paths)

## can't seem to trust Oracle to maintain sorting for all tag events, so do a safety re-sort
paths <- paths %>% arrange(TID,as.numeric(CID),as.numeric(POS))

people = person
if(all.people){
  ## get all names who've recaptured tags
  sql = paste0("SELECT * FROM ",oracle.personal.user,".LBT_RECAPTURES")
  rec <- ROracle::dbSendQuery(conn, sql)
  rec <- ROracle::fetch(rec)
  people <- unique(rec$PERSON)
}

##
ROracle::dbDisconnect(conn)

## loops if there's more than one person
for (p in people){
  person = p

if(is.null(person)){print("No person chosen to make maps for")}else{

  ### filter path data for tags recaptured by chosen person
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
                 map_token = map.token)

    base <- basemap_raster(ext_sf) #### forces basemap crs to be in 3857

    ## change back to 4326 (raster package has some masking issues with sf, so just use ::)
    base <- raster::projectRaster(base,  crs = 4326)

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
    rec.lab <- path.pers %>% filter(TID  %in% i, !REC_PERSON %in% "NA") %>% dplyr::select(TID,REC_DATE,LAT,LON,REC_PERSON)
    rec.lab <- rec.lab %>% mutate(name = ifelse(REC_PERSON %in% person,paste("You Captured:",REC_DATE),paste("Other Fisher Captured:",REC_DATE)))

    map.labs <- rbind(rel.lab %>% dplyr::select(TID,LAT,LON,name),rec.lab %>% dplyr::select(TID,LAT,LON,name))

    labs.sf <- sf::st_as_sf(map.labs, coords = c("LON","LAT"))
    sf::st_crs(labs.sf) <- sf::st_crs(4326)


    a <- gg_raster(base)+
      ggtitle(paste(person,"-",i))+
      geom_sf(data=path_sf, colour = "red", linewidth=1.6, arrow = arrow(type = "open", length = unit(0.3, "inches")))+
      geom_sf(data=labs.sf, colour = "yellow")+
      geom_sf_label_repel(data = labs.sf, aes(label=name),colour="blue",show.legend=F,nudge_y=0, alpha=0.8,max.overlaps = 20, size=4)+
      coord_sf(ylim=as.numeric(c(limits$ymin,limits$ymax)), xlim = as.numeric(c(limits$xmin,limits$xmax)), expand = F)+
      theme(plot.margin = margin(t = 73))


    #### make inset with NS map
    b <- gg_raster(inset)+
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



