#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny leaflet reactable plyr
#' @importFrom magrittr %>%
#' @noRd
app_server <- function(input, output, session) {

  # create the map
  output$map <- renderLeaflet({
    leaflet(data = ModelInventory::df_ins) %>%
      addTiles() %>%
      addCircleMarkers(lng = ~as.numeric(Lon), lat = ~as.numeric(Lat), color = "black", fillOpacity = 0.6, stroke=FALSE,
                       radius=5, popup = ~paste0("<strong>Institute</strong> ", InstituteName, "<br><strong>Country</strong> ",
                                                 Country,"<br><strong>Type</strong> ", InstituteType,
                                                 "<br><strong>Website</strong> ", Website))

  })

  # action that triggers change when map is zoomed or translated
  ChangeClick <- reactive({
    #return(c(input$map_zoom, input$shapefile, input$map_center))
    return(c(input$map_zoom, input$map_center))
  })

  # model to show based on the map extension
  model_to_show <- eventReactive(ChangeClick(),{
    #print(input$map_bounds)
    south <- print(input$map_bounds$south)
    north <- print(input$map_bounds$north)
    east <- print(input$map_bounds$east)
    west <- print(input$map_bounds$west)
    # subset data
    models <- dplyr::filter(ModelInventory::df_res, MaxLat <= north, MinLat >= south, MaxLon <= east, MinLon >= west)
  })

  # subset polygons
  reactive_polygons <- eventReactive(ChangeClick(),{
    ids <- model_to_show()$id
    # print(ids)
    #subset
    sublist_polygons <- ModelInventory::list_polygons[ids]
    sublist_polygons <- do.call(rbind,sublist_polygons)
  })

  observe({
    req(reactive_polygons())
    leafletProxy("map") %>%
      clearShapes() %>%
      addPolygons(data = reactive_polygons(), fill = F, stroke=T,
                  options = markerOptions(minZoom = 15, maxZoom = 20))
  })

  # print table
  output$model_table <- renderReactable({
    tmp <- dplyr::select(model_to_show(), InstituteName, InstituteType, ModelName, ModelCore,MinRes, VertCoord, NLevels)
    reactable(tmp)
  })

  # print Pie plot
  output$PiePlot <- renderPlot({
    PiePlot(model_to_show(),input$var, ModelInventory::dfvartype)
  })

  # print Bar plot
  output$BarPlot <- renderPlot({
    BarPlots(model_to_show(),input$var_type, ModelInventory::dfvartype)
  })

}
