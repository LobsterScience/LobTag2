
#' @title draw_land
#' @import dplyr sf shiny terra leaflet leaflet.extras
#' @description Allows user to draw and save polygons that get automatically added as land when they use LobTag2 pathing functions
#' @export

draw_land <- function(){

ui <- fluidPage(
  leafletOutput("mymap"),
  actionButton("save_polygons", "Save Polygons"),
  tableOutput("polygon_coords")
)

server <- function(input, output, session) {
  # Initialize leaflet map with drawing options
  output$mymap <- renderLeaflet({
    leaflet(options = leafletOptions(crs = leafletCRS(crsClass = "L.CRS.EPSG4326"))) %>%
      addTiles() %>%
      addDrawToolbar(
        targetGroup = "drawn",
        polylineOptions = FALSE,
        polygonOptions = drawPolygonOptions(showArea = TRUE),
        rectangleOptions = FALSE,
        circleOptions = FALSE,
        markerOptions = FALSE,
        editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions())
      )
  })

  # Reactive list to store drawn features
  drawn_polygons <- reactiveVal(list())

  # Observe the drawn shapes and add to reactive list
  observeEvent(input$mymap_draw_new_feature, {
    drawn_polygons(c(drawn_polygons(), list(input$mymap_draw_new_feature)))
  })

  # Save and process polygons on button click
  observeEvent(input$save_polygons, {
    polygons <- drawn_polygons()
    coords <- do.call(rbind, lapply(seq_along(polygons), function(i) {
      feature <- polygons[[i]]
      lnglat <- matrix(unlist(feature$geometry$coordinates[[1]]), ncol = 2, byrow = TRUE)
      data.frame(polygon_id = i, lon = lnglat[, 1], lat = lnglat[, 2])
    }))
    ## print in shiny window
    output$polygon_coords <- renderTable(coords)

    ## bring in any already saved polygons and add new polygons to these, then resave
    if(file.exists("C:/LOBTAG/data/new_land_coords.rds")){
      old.coords <- readRDS("C:/LOBTAG/data/new_land_coords.rds")
      new.coords <- coords %>% mutate(polygon_id = polygon_id+max(old.coords$polygon_id))
      new.coords <- rbind(old.coords,new.coords)
      saveRDS(new.coords, file = "C:/LOBTAG/data/new_land_coords.rds")
    }else{saveRDS(coords, file = "C:/LOBTAG/data/new_land_coords.rds")}


  })
}

# Run the app
shinyApp(ui, server)



}
