#' @title draw_land
#' @import dplyr sf shiny leaflet leaflet.extras
#' @description Allows user to draw and save polygons that get automatically added as land when they use LobTag2 pathing functions
#' @export

draw_land <- function() {

  ui <- fluidPage(
    leafletOutput("mymap"),
    actionButton("save_polygons", "Save Polygons"),
    actionButton("delete_polygon", "Delete Selected Polygon"),
    tableOutput("polygon_coords"),
    verbatimTextOutput("selected_poly")
  )

  server <- function(input, output, session) {

    save_path <- "C:/LOBTAG/data/new_land_coords.rds"

    # Reactive values to hold polygons and selected polygon ID
    polygons_data <- reactiveVal(NULL)       # will hold all polygons from file + new ones when saved
    drawn_polygons <- reactiveVal(list())    # polygons drawn in current session
    selected_polygon <- reactiveVal(NULL)    # polygon_id selected for deletion

    # Load polygons on startup
    if (file.exists(save_path)) {
      polygons_data(readRDS(save_path))
    } else {
      polygons_data(NULL)
    }

    # Convert to sf for existing polygons display
    get_existing_sf <- reactive({
      df <- polygons_data()
      if (is.null(df)) return(NULL)
      df %>%
        group_by(polygon_id) %>%
        summarise(do_union = FALSE,
                  geometry = st_sfc(st_polygon(list(cbind(lon, lat)))), .groups = "drop") %>%
        st_as_sf(crs = 4326)
    })

    output$mymap <- renderLeaflet({
      m <- leaflet(options = leafletOptions(worldCopyJump = FALSE)) %>%
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

      existing_sf <- get_existing_sf()
      if (!is.null(existing_sf)) {
        m <- m %>% addPolygons(
          data = existing_sf,
          color = "blue",
          fillOpacity = 0.4,
          weight = 2,
          group = "existing",
          layerId = ~as.character(polygon_id),  # assign layerId for click events
          popup = ~paste("Polygon", polygon_id)
        )
      }
      m
    })

    # Observe polygons drawn in current session
    observeEvent(input$mymap_draw_new_feature, {
      drawn_polygons(c(drawn_polygons(), list(input$mymap_draw_new_feature)))
    })

    # Observe clicks on existing polygons to select one for deletion
    observeEvent(input$mymap_shape_click, {
      clicked_id <- input$mymap_shape_click$id
      if (!is.null(clicked_id)) {
        selected_polygon(as.numeric(clicked_id))
      }
    })

    # Show selected polygon id in UI
    output$selected_poly <- renderText({
      pid <- selected_polygon()
      if (is.null(pid)) {
        "No polygon selected"
      } else {
        paste("Selected polygon ID for deletion:", pid)
      }
    })

    # Save newly drawn polygons on button click
    observeEvent(input$save_polygons, {
      polygons <- drawn_polygons()
      if (length(polygons) == 0) {
        showNotification("No polygons drawn yet.", type = "warning")
        return(NULL)
      }

      coords <- do.call(rbind, lapply(seq_along(polygons), function(i) {
        feature <- polygons[[i]]
        lnglat <- matrix(unlist(feature$geometry$coordinates[[1]]), ncol = 2, byrow = TRUE)
        df <- data.frame(polygon_id = i, lon = as.numeric(lnglat[, 1]), lat = as.numeric(lnglat[, 2]))
        # Correct longitudes
        df$lon[df$lon < -180] <- df$lon[df$lon < -180] + 360
        df$lon[df$lon > 180] <- df$lon[df$lon > 180] - 360
        df
      }))

      # Assign unique polygon_id starting after max existing id
      old_coords <- polygons_data()
      max_id <- if (is.null(old_coords)) 0 else max(old_coords$polygon_id)
      coords <- coords %>%
        mutate(polygon_id = polygon_id + max_id)

      # Combine old and new polygons and save
      if (is.null(old_coords)) {
        all_coords <- coords
      } else {
        all_coords <- bind_rows(old_coords, coords)
      }
      saveRDS(all_coords, save_path)

      # Update reactive data and reset drawn polygons list
      polygons_data(all_coords)
      drawn_polygons(list())

      # Show table in UI
      output$polygon_coords <- renderTable(all_coords)
      showNotification("Polygons saved successfully.", type = "message")

      # Redraw map to reflect saved polygons (refresh leaflet)
      leafletProxy("mymap") %>% clearGroup("existing") %>% {
        if (!is.null(all_coords)) {
          existing_sf <- all_coords %>%
            group_by(polygon_id) %>%
            summarise(do_union = FALSE,
                      geometry = st_sfc(st_polygon(list(cbind(lon, lat)))), .groups = "drop") %>%
            st_as_sf(crs = 4326)
          addPolygons(., data = existing_sf,
                      color = "blue", fillOpacity = 0.4, weight = 2,
                      group = "existing",
                      layerId = as.character(existing_sf$polygon_id),
                      popup = paste("Polygon", existing_sf$polygon_id))
        } else {
          .
        }
      }
    })

    # Delete selected polygon from saved polygons on button click
    observeEvent(input$delete_polygon, {
      pid <- selected_polygon()
      if (is.null(pid)) {
        showNotification("No polygon selected for deletion.", type = "warning")
        return()
      }

      all_coords <- polygons_data()
      if (is.null(all_coords)) {
        showNotification("No polygons to delete.", type = "error")
        return()
      }

      # Remove polygon with selected id
      new_coords <- all_coords %>% filter(polygon_id != pid)
      saveRDS(new_coords, save_path)
      polygons_data(new_coords)

      # Clear selection
      selected_polygon(NULL)

      # Update polygon table in UI
      output$polygon_coords <- renderTable(new_coords)

      # Update map: remove deleted polygon layer
      leafletProxy("mymap") %>% clearGroup("existing") %>% {
        if (nrow(new_coords) > 0) {
          existing_sf <- new_coords %>%
            group_by(polygon_id) %>%
            summarise(do_union = FALSE,
                      geometry = st_sfc(st_polygon(list(cbind(lon, lat)))), .groups = "drop") %>%
            st_as_sf(crs = 4326)
          addPolygons(., data = existing_sf,
                      color = "blue", fillOpacity = 0.4, weight = 2,
                      group = "existing",
                      layerId = as.character(existing_sf$polygon_id),
                      popup = paste("Polygon", existing_sf$polygon_id))
        } else {
          .
        }
      }

      showNotification(paste("Polygon", pid, "deleted."), type = "message")
    })

    # Show initial polygons in table at startup
    output$polygon_coords <- renderTable({
      polygons_data()
    })

  }

  shinyApp(ui, server)
}
