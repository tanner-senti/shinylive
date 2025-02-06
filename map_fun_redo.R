library(leaflet)

buildMapAR_redo <-
  function(sites,
           sites_nodataAR = NULL,
           dragging = TRUE,
           ...) {
    if (missing(sites)) {
      stop("The 'sites' parameter is required.")
    }
    
    # Extract site coordinates and rename columns for clarity
    site_coords <- sites[, c(
      "AU",
      "MonitoringLocationIdentifier",
      "MonitoringLocationName",
      "MonitoringLocationTypeName",
      "LatitudeMeasure",
      "LongitudeMeasure"
    )]
    
    names(site_coords) <-
      c("AU",
        "locationID",
        "locationName",
        "locationType",
        "Latitude",
        "Longitude")
    
    # Handle optional "All Locations" data
    if (!is.null(sites_nodataAR)) {
      nodata_coords <- sites_nodataAR[, c(
        "AU",
        "MonitoringLocationIdentifier",
        "MonitoringLocationName",
        "MonitoringLocationTypeName",
        "LatitudeMeasure",
        "LongitudeMeasure"
      )]
      names(nodata_coords) <-
        c("AU",
          "locationID",
          "locationName",
          "locationType",
          "Latitude",
          "Longitude")
    }
    
    # Create a color palette for location types
    pal <- colorFactor(
      palette = colorRampPalette(RColorBrewer::brewer.pal(11, "Spectral"))(length(unique(
        site_coords$locationType
      ))),
      domain = site_coords$locationType
    )
    
    # Build the Leaflet map
    map <-
      leaflet(options = leafletOptions(preferCanvas = TRUE, dragging = dragging), ...)
    map <- addProviderTiles(
      map,
      "Esri.WorldImagery",
      group = "Satellite",
      options = providerTileOptions(updateWhenZooming = FALSE, updateWhenIdle = TRUE)
    )
    map <- addProviderTiles(
      map,
      "Esri.WorldTopoMap",
      group = "Topo",
      options = providerTileOptions(updateWhenZooming = FALSE, updateWhenIdle = TRUE)
    )
    map <- addMapPane(map, "markers", zIndex = 420)
    
    # Add always-visible default "Sites" layer
    map <- addCircleMarkers(
      map,
      lat = site_coords$Latitude,
      lng = site_coords$Longitude,
      group = NULL,
      # No specific group to keep this layer always visible
      color = pal(site_coords$locationType),
      opacity = 0.8,
      layerId = site_coords$locationID,
      options = pathOptions(pane = "markers"),
      popup = paste0(
        "Assessment Unit: ",
        site_coords$AU,
        "<br> Station ID: ",
        site_coords$locationID,
        "<br> Name: ",
        site_coords$locationName,
        "<br> Type: ",
        site_coords$locationType,
        "<br> Lat: ",
        site_coords$Latitude,
        "<br> Long: ",
        site_coords$Longitude
      )
    )
    
    # Add "All Locations" layer if provided
    if (!is.null(sites_nodataAR)) {
      map <- addCircleMarkers(
        map,
        lat = nodata_coords$Latitude,
        lng = nodata_coords$Longitude,
        group = "All Locations",
        color = pal(nodata_coords$locationType),
        opacity = 0.8,
        layerId = nodata_coords$locationID,
        options = pathOptions(pane = "markers"),
        popup = paste0(
          "Assessment Unit: ",
          nodata_coords$AU,
          "<br> Station ID: ",
          nodata_coords$locationID,
          "<br> Name: ",
          nodata_coords$locationName,
          "<br> Type: ",
          nodata_coords$locationType,
          "<br> Lat: ",
          nodata_coords$Latitude,
          "<br> Long: ",
          nodata_coords$Longitude
        )
      )
    }
    
    # Add "Labels" layer - conflicts with "all locations" - could not find fix
    # map <- addLabelOnlyMarkers(
    #   map,
    #   lat = site_coords$Latitude,
    #   lng = site_coords$Longitude,
    #   label = site_coords$locationName,
    #   group = "Labels",
    #   labelOptions = labelOptions(noHide = TRUE, textsize = "15px"),
    #   clusterOptions = markerClusterOptions(spiderfyOnMaxZoom = TRUE)
    # )
    
    # Add layers control
    if (!is.null(sites_nodataAR)) {
      overlay_groups <-
        c("All Locations") # change this to LAbels if using labels instead of all sites
    }
    
    map <- addLayersControl(
      map,
      position = "topleft",
      baseGroups = c("Topo", "Satellite"),
      overlayGroups = overlay_groups,
      options = layersControlOptions(collapsed = TRUE, autoZIndex = FALSE)
    )
    
    # Set "Labels" and "All Locations" layers off by default
    map <- hideGroup(map, "Labels")
    if (!is.null(sites_nodataAR)) {
      map <- hideGroup(map, "All Locations")
    }
    
    # Add legend
    map <- addLegend(
      map,
      position = "topright",
      colors = unique(pal(site_coords$locationType)),
      labels = unique(site_coords$locationType)
    )
    
    return(map)
  }
