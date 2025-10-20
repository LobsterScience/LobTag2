#' @title draw_land
#' @import dplyr sf shiny leaflet leaflet.extras
#' @description Allows user to draw and save polygons that get automatically added as land when they use LobTag2 pathing functions
#' @export

draw_land <- function() {

  ui <- fluidPage(
    leafletOutput("mymap"),
    actionButton("save_polygons", "Save Polygons"),
    tableOutput("polygon_coords")
  )

  server <- function(input, output, session) {

    # Initialize leaflet map with drawing options
    output$mymap <- renderLeaflet({
      leaflet(options = leafletOptions(worldCopyJump = FALSE)) %>%
        addProviderTiles(providers$OpenStreetMap, options = providerTileOptions(noWrap = TRUE)) %>%
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

    # Observe new shapes drawn on the map
    observeEvent(input$mymap_draw_new_feature, {
      drawn_polygons(c(drawn_polygons(), list(input$mymap_draw_new_feature)))
    })

    # Save polygons when button is clicked
    observeEvent(input$save_polygons, {
      polygons <- drawn_polygons()

      # If no polygons drawn, do nothing
      if (length(polygons) == 0) {
        showNotification("No polygons drawn yet.", type = "warning")
        return(NULL)
      }

      # Extract coordinates
      coords <- do.call(rbind, lapply(seq_along(polygons), function(i) {
        feature <- polygons[[i]]
        lnglat <- matrix(unlist(feature$geometry$coordinates[[1]]), ncol = 2, byrow = TRUE)
        df <- data.frame(polygon_id = i, lon = as.numeric(lnglat[, 1]), lat = as.numeric(lnglat[, 2]))

        # Correct longitudes that wrapped past -180/180
        df$lon[df$lon < -180] <- df$lon[df$lon < -180] + 360
        df$lon[df$lon > 180]  <- df$lon[df$lon > 180] - 360

        df
      }))

      # Show table in app
      output$polygon_coords <- renderTable(coords)

      # Save polygons to file
      save_path <- "C:/LOBTAG/data/new_land_coords.rds"

      if (file.exists(save_path)) {
        old.coords <- readRDS(save_path)
        new.coords <- coords %>% mutate(polygon_id = polygon_id + max(old.coords$polygon_id))
        new.coords <- rbind(old.coords, new.coords)
        saveRDS(new.coords, file = save_path)
      } else {
        saveRDS(coords, file = save_path)
      }

      showNotification("Polygons saved successfully.", type = "message")
    })
  }

  # Run the app
  shinyApp(ui, server)
}

