#' @title generate_maps
#' @import dplyr basemaps svDialogs shiny ROracle DBI shinyjs
#' @description creates maps of tag movement for participants
#' @export

generate_maps <- function(map_token = "pk.eyJ1IjoiZWxlbWVudGpha2UiLCJhIjoiY2xxZmh2bGFiMHhyNTJqcHJ0cDBqcW83ZiJ9.mDd49ObNcNdG6MzH3yc2dw",person=NULL){

  # The lable colors that are later randomized, opted for the following colors that are color blind friendly
  collist <- c("#E69F00", "#56B4E9", "#009E73",
               "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


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

  ## make table with all recaptures and their release information
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

  # Prepare recaptures
  rec <- rec %>% mutate(lat = LAT_DD) %>% mutate(lon = LON_DD)
  rec <- rec %>% mutate(trip = paste(lat,lon,REC_DATE))
  rec <- rec %>% mutate(REC_DATE = as.Date(REC_DATE, format= "%d/%m/%Y"))
  rec <- rec %>% mutate(REC_DATE = format(REC_DATE, "%d-%b-%Y"))
  rec <- rec %>% dplyr::select(TAG_ID,REC_DATE,YEAR,LAT_DD,LON_DD,PERSON,trip)
  rec <- rec %>% rename(REC_YEAR = YEAR, REC_LAT= LAT_DD,REC_LON = LON_DD)

  # Prepare releases
  rel <- rel %>% rename(REL_LAT = LAT_DD, REL_LON = LON_DD, REL_YEAR = YEAR)
  rel <- rel %>% mutate(REL_DATE = as.Date(REL_DATE, format= "%d/%m/%Y"))
  rel <- rel %>% mutate(REL_DATE = format(REL_DATE, "%d-%b-%Y"))

dat <- left_join(rec,rel)


if(is.null(person)){print("No person chosen to make maps for")}else{

  pers.dat <- dat %>% filter(PERSON %in% person)

  outlist = c()
  clist = list()
  tx = split(pers.dat, pers.dat$TAG_ID)

  for(i in 1:length(tx)){
    collist = collist[sample.int(7, 7)]
    curt = tx[[i]]
    path = get.pathdata.tid(tid = curt$TAG_ID[1])
    path = path[order(as.numeric(path$CID), as.numeric(path$POS)),]
    path$LON = as.numeric(path$LON)
    path$LAT = as.numeric(path$LAT)
    globalextent = raster::extent(min(path$LON), max(path$LON), min(path$LAT), max(path$LAT))
    #globalextent = extent(-60.2216, -60.2048, 45.718, 45.719) #extent is part of the raster package...

    linlats = cbind(curt$REL_LAT)
    linlats = rbind(linlats, curt$REC_LAT[nrow(curt)])
    linlons = cbind(curt$REL_LON)
    linlons = rbind(linlons, curt$REC_LON[nrow(curt)])

    linestring = SpatialLines(list(Lines(Line(cbind(as.numeric(linlons),as.numeric(linlats))), ID=person)))
    bb = data.frame(linestring@bbox)

    coords1 <- as.matrix(data.frame(cbind(c(bb[1,1], bb[1,1], bb[1,2], bb[1,2], bb[1,1]), c(bb[2,1], bb[2,2], bb[2,2], bb[2,1], bb[2,1]))))
    bbox.polyc = Polygon(coords = coords1, hole = F)
    bbox.polyc = Polygons(list(bbox.polyc),1)
    bbox.polyc = SpatialPolygons(list(bbox.polyc))

    s = (bb[1,2] - bb[1,1])/2
    coords2 <- as.matrix(data.frame(cbind(c(bb[1,1]-s, bb[1,1]-s, bb[1,2]+s, bb[1,2]+s, bb[1,1]-s), c(bb[2,1]-s, bb[2,2]+s, bb[2,2]+s, bb[2,1]-s, bb[2,1]-s))))
    bbox.polyc2 = Polygon(coords = coords2, hole = F)
    bbox.polyc2 = Polygons(list(bbox.polyc2),1)
    bbox.polyc2 = SpatialPolygons(list(bbox.polyc2))

    clisind = length(clist)+1

    if(length(clist) > 0){
      #loop through each persons data to test if tags can be combined with the map of the current tag
      for(j in 1:length(clist)){
        # Test if bounding boxes intersect")
        if(gIntersects(clist[[j]]$bbox.poly, bbox.polyc2)){
          # Test if the bounding boxes are reasonably proportional to one annother
          if(min(gArea(clist[[j]]$bbox.poly), gArea(bbox.polyc))/max(gArea(bbox.polyc),gArea(clist[[j]]$bbox.poly)) > .083333 ){
            intersects = F
            #loop through current tag line lists to test overlaps
            for(k in 1:length(clist[[j]]$linelist)){
              if(gIntersects(linestring, clist[[j]]$linelist[[k]])){
                intersects = T
              }
            }
            #commented out to keep all tags on their own chart
            #if(!intersects) clisind = j #if we get here tag can be combined at this location
          }
        }
      }
    }
    # If we adding to a current map we need to do the following
    if(clisind <= length(clist)){
      temp = clist[[clisind]]

      temp$linelist[[length(temp$linelist)+1]] = linestring
      temp$pathlist[[length(temp$pathlist)+1]] = path
      temp$datalist[[length(temp$datalist)+1]] = curt
      temp$globextent = extent(min(temp$globextent@xmin, globalextent@xmin), max(temp$globextent@xmax, globalextent@xmax), min(temp$globextent@ymin, globalextent@ymin), max(temp$globextent@ymax, globalextent@ymax))

      clist[[clisind]] = temp
      # If we did not find an good map to add to we need to add a new entry
    } else{
      #clist[[clisind]] = list(bbox.poly = bbox.polyc, linelist = list(linestring), pathlist = "", datalist = list(curt), globextent = globalextent)
      clist[[clisind]] = list(bbox.poly = bbox.polyc, linelist = list(linestring), pathlist = list(path), datalist = list(curt), globextent = globalextent)
    }
  }


  for(i in 1:length(clist)){
    mdata = clist[[i]]

    xmin = xmin(mdata$globextent)
    xmax = xmax(mdata$globextent)
    ymin = ymin(mdata$globextent)
    ymax = ymax(mdata$globextent)

    xlen = xmax - xmin
    ylen = ymax - ymin

    #Make plotting region visually more square
    while(xlen < ylen){
      xmax = xmax+.1
      xmin = xmin-.1
      xlen =  xmax - xmin
    }
    while(ylen < xlen){
      ymax = ymax+.1
      ymin = ymin-.1
      ylen =  ymax - ymin
    }

    while(xlen < ylen){
      xmax = xmax+.001
      xmin = xmin-.001
      xlen =  xmax - xmin
    }
    while(ylen < xlen){
      ymax = ymax+.001
      ymin = ymin-.001
      ylen =  ymax - ymin
    }
    #visually scale plotting area a bit wider
    scale = (ymax-ymin)/100
    xmax = xmax+scale
    xmin = xmin-scale

    xlim = c(xmin, xmax)
    ylim = c(ymin, ymax)

    #rasters for plot backgrounds
    #Git hub dosnt allow large files user will need to have these stored charts folder
    #C:\bio.data\bio.lobster\data\tagging
    #l1 = brick(file.path('c:', 'bio', '801_LL_WGS84.tif'))
    #l2 = brick(file.path('c:', 'bio', 'atl_merged.tif'))

    l1 = brick(file.path('c:', 'bio.data', 'bio.lobster', 'data', 'tagging', '801_LL_WGS84.tif'))
    l2 = brick(file.path('c:', 'bio.data', 'bio.lobster', 'data', 'tagging', 'atl_merged.tif'))

    #Expand region
    xmin = xmin - ylen/4
    xmax = xmax + ylen/4
    ymin = ymin - ylen/4
    ymax = ymax + ylen/4
    ylen = ymax-ymin
    xlen = xmax-xmin

    e = extent(xmin, xmax, ymin, ymax)
    e2 = e #e2 will be representing inset plot boundary, needs to be larger geographic area than main plot
    ear = xlen*ylen
    e2ar = ear
    while(e2ar/20 < ear){
      e2@xmin = e2@xmin - .1
      e2@xmax = e2@xmax + .1
      e2@ymin = e2@ymin - .1
      e2@ymax = e2@ymax + .1
      e2ar = (e2@xmax-e2@xmin) * (e2@ymax-e2@ymin)
    }
    # Also needs to be significantly large enough to get an idea on where in the world we are
    while(e2ar < 20){
      e2@xmin = e2@xmin - .1
      e2@xmax = e2@xmax + .1
      e2@ymin = e2@ymin - .1
      e2@ymax = e2@ymax + .1
      e2ar = (e2@xmax-e2@xmin) * (e2@ymax-e2@ymin)
    }

    rectan = data.frame(x = c(e@xmin, e@xmin, e@xmax, e@xmax), ex = c(e@xmin, e@xmax, e@xmax, e@xmin), y = c(e@ymin, e@ymax, e@ymax, e@ymin), ey = c(e@ymax, e@ymax, e@ymin, e@ymin))
    #Get distance for plotting scalebar
    dist = round(distHaversine(c(xmin, ymin), c(xmax, ymin), r=6378137)/1000/8, -1)
    #Round differently for smaller areas
    if(dist < 10){
      dist = ceiling(distHaversine(c(xmin, ymin), c(xmax, ymin), r=6378137)/1000/8)
    }
    hei = ylen/100




      mp = ggRGB(l1, r=1, g=2, b=3, maxpixels = 800000, ext = e, ggObj = T)+
        ggtitle(paste(name, i, sep = "-")) + xlab("Longitude") + ylab("Latitude")+
        ggsn::scalebar(x.min = xmin, x.max = xmax, y.min = ymin, y.max = ymax, dist_unit= "km", location = "bottomleft", anchor = c(x = xmin+2*hei, y=ymin+5*hei), transform = T, dist = dist, st.size=4, height=hei, model = 'WGS84')+
        ggsn::north(x.min = xmin, x.max = xmax, y.min = ymin, y.max = ymax, location = "topleft", scale = 0.06, symbol = 10, anchor = c(x = xmin+hei, y=ymax-hei))+
        scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0))+
        geom_hline(yintercept=ymax, color = "red", size = 2)+
        geom_vline(xintercept=xmax, color = "red", size = 2)+
        theme(plot.margin=unit(c(1,.5,.5,-.2),"cm"),
              title = element_text(size=8),
              axis.title = element_text( size=15, face=2),
              axis.line = element_line(size = 1, colour = "red", linetype=1),
              axis.text.x = element_text( angle = 0, size=15),
              axis.text.y = element_text( angle = 0, size=15))

      #Add movement paths for each tag in current plot
      #j=2
      for(j in 1:length(mdata$pathlist)){
        pl = mdata$pathlist[[j]]
        #loop through each pach segment
        #l = 1
        for(l in unique(pl$CID)){
          seg = pl[which(pl$CID == l),]
          #if start and end points are so close together, don't show an arrow...
          distancedf = seg[seg$POS == 1 | seg$POS == max(seg$POS),]
          path_distance = distm(c(distancedf$LON[1], distancedf$LAT[1]), c(distancedf$LON[2], distancedf$LAT[2]), fun = distHaversine)
          if(path_distance < 100){
            #print(paste("short path: ", person, sep = ""))
            alpha = 0
          } else {
            alpha = 0.85
          }

          mp = mp+geom_path(data=seg, aes(x = as.numeric(LON), y = as.numeric(LAT)),
                            arrow = arrow(length = unit(0.5, "cm")), colour = 'red', alpha = alpha, size = 1.0)
        }
      }
      # Randomized the label colors, make each plot look a bit more unique
      collist = collist[sample.int(length(collist), length(collist))]

      labelframe = NULL
      dat = NULL
      coords = NULL
      #Create a dataframe of all labels for this plot. This is needed for
      #so that the label repel works on all labels for this plot
      #j = 2
      for(j in 1:length(mdata$datalist)){
        reldata = mdata$datalist[[j]][1,]
        reldata$REL_LON = as.numeric(reldata$REL_LON)
        reldata$REL_LAT = as.numeric(reldata$REL_LAT)
        labelframe = rbind(labelframe, cbind(reldata$REL_LON, reldata$REL_LAT, collist[j],  paste("Tag: ", reldata$TAG_ID," ",as.character(reldata$REL_DATE), sep="")))
        mdata$datalist[[j]]$label = as.character(mdata$datalist[[j]]$REC_DATE)
        mdata$datalist[[j]]$label[which(mdata$datalist[[j]]$PERSON == person)] = paste(you_caught_lobster, mdata$datalist[[j]]$label[which(mdata$datalist[[j]]$PERSON == person)], sep = "")
        mdata$datalist[[j]]$label[which(mdata$datalist[[j]]$PERSON != person)] = paste(other_caught_lobster, mdata$datalist[[j]]$label[which(mdata$datalist[[j]]$PERSON != person)], sep = "")
        labelframe = rbind(labelframe, cbind(as.numeric(mdata$datalist[[j]]$REC_LON), as.numeric(mdata$datalist[[j]]$REC_LAT), collist[j], as.character(mdata$datalist[[j]]$label)))
      }





      labelframe = as.data.frame(labelframe)
      names(labelframe) = c("x", "y","colID", "lab")
      labelframe$ID = 1:nrow(labelframe)
      labelframe$x = as.numeric(as.character(labelframe$x))
      labelframe$y = as.numeric(as.character(labelframe$y))
      labelframe$colID = as.character(labelframe$colID)
      labelframe$lab = as.character(labelframe$lab)

      #add points to plot from other captures.... (use label plot df)
      mp = mp + geom_point(data = labelframe, aes(x = x, y = y), color = "black")

      #if only no person tags are of interest to make points for make new df from labelframe (you'll have to match the label column with 'other fisher captured')


      #Add labels to main plot
      mp = mp+geom_label_repel(data=labelframe, aes(x = x, y = y, label = lab),
                               fill = alpha(labelframe$colID,0.5), fontface = "bold",
                               color="black", segment.color = 'black', segment.size = .7,
                               family = "Helvetica",
                               box.padding = unit(0.35, "lines"),
                               point.padding = unit(0.5, "lines"))

      #Inset plot
      ip = ggplotGrob(
        ggRGB(l2, r=1, g=2, b=3, maxpixels = 100000, ext = e2, ggObj = T)+
          theme_map()+
          theme(panel.border = element_rect(colour = "black", fill = NA),
                plot.margin=unit(c(0,0,0,0),"cm"))+
          scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0))+
          geom_segment(data=rectan, aes(x = x, y = y, xend = ex, yend = ey),colour=c("red"))
      )
      #Output plot, a combination of the main plot with inset plot positioned correctly
      g3 <- mp +
        annotation_custom(grob = ip, xmin = e@xmax-((e@xmax-e@xmin)/4), xmax = e@xmax+((e@xmax-e@xmin)/20),
                          ymin = e@ymax-((e@ymax-e@ymin)/4), ymax = e@ymax +((e@ymax-e@ymin)/20))


      ggsave(filename = paste0(person,i,".pdf"), path = "C:/Users/ELEMENTG/Documents/Rsaves", plot = g3, width = 11, height = 10)

    }


  }


}












# library(dplyr)
# library(basemaps)
# library(svDialogs)
# library(shiny)










#' @title  get.pathdata.tid
#' @description  Return calculated paths by supplied TID
#' @import ROracle
#' @return dataframe
#' @export
get.pathdata.tid = function(region = "ScotianShelf", tid = ""){
  gstring = ""

  drv <- DBI::dbDriver("Oracle")
  con <- ROracle::dbConnect(drv, username = oracle.lobster.user, password = oracle.lobster.password, dbname = oracle.lobster.server)

  da = NULL

  pathsdb = paste("ELEMENTG",".","LBT_PATHS", sep ="")

  #new lobster code
  query = paste("SELECT * FROM ", pathsdb , " where ", pathsdb, ".TID = '", tid, "'", sep = "")

  resbio <- ROracle::dbSendQuery(con, query)
  da <- ROracle::fetch(resbio)

  da = da[order(da$CID, da$POS),]
  ROracle::dbDisconnect(con)

  return(da)
}
####SCRAP
#### load base map
## Allow user to choose data file to upload or manually draw extent
# result <- dlgMessage(type = "yesno", message = "Would you like to mannually draw the mapping area? If no, the default basemap will be used")
# if(result$res %in% "yes"){ext <- draw_ext()}else{ext <- readRDS("C:/bio/LobTag2/app.files/NS_extent")}
#
# set_defaults(ext, map_service = "mapbox",map_type = "satellite",
#              map_token = map_token)
#
# base <- basemap_raster(ext) #### forces basemap crs to be in 3857
#
# ## change back to 4326 (raster package has some masking issues with sf, so just use ::)
# base <- raster::projectRaster(base,  crs = 4326)
#
#
