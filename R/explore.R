#' Shiny app for exploring zika data
#'
#' @export
#' @examples
#' explore()
#'

explore <- function() {

  data(zika)
  data(latLonDat)

  z <- zika %>%
    # some locations (e.g. "Brazil-Amapa") seem to consistenly report NAs
    dplyr::filter(!is.na(value)) %>%
    dplyr::left_join(latLonDat, by = "location") %>%
    dplyr::filter(!is.na(lat)) %>%
    # column to track selections
    dplyr::mutate(region = "All Regions")

  ui <- fluidPage(
    leafletOutput("map"),
    tabsetPanel(
      tabPanel("Summary", plotlyOutput("summary"))
    )
  )

  server <- function(input, output, session, ...) {

    output$map <- renderLeaflet({
      leaflet(latLonDat) %>%
        addTiles() %>%
        fitBounds(~min(lng), ~min(lat), ~max(lng), ~max(lat)) %>%
        addCircleMarkers(
          lng = ~lng, lat = ~lat, label = ~location, layerId = ~location,
          color = "black", clusterOptions = markerClusterOptions()
        )
    })

    # A reactive expression that returns the locations that are
    # in bounds right now
    mapClickData <- reactive({
      print(input$map_marker_click)

    })

    mapZoomData <- reactive({
      if (is.null(input$map_bounds)) {
        return(data.frame())
      }
      bounds <- input$map_bounds
      latRng <- range(bounds$north, bounds$south)
      lngRng <- range(bounds$east, bounds$west)
      idx <- with(z, latRng[1] <= lat & lat <= latRng[2] & lngRng[1] <= lng & lng <= lngRng[2])
      if (all(idx)) {
        return(data.frame())
      }
      z %>% filter(idx) %>% mutate(region = "Inside Map")
    })


    retrieveSelection <- reactive({
      selection <- mapZoomData()
      rbind(z, selection)
    })


    output$summary <- renderPlotly({

      plot_area <- function(.) {
        # TODO: this needs to be fixed in plotly!
        cols <- if (length(unique(.$region)) > 1)  c("black", "green", "red") else "black"
        plot_ly(., x = ~x, ymax = ~y, color = ~region, colors = cols) %>%
          add_area() %>%
          layout(yaxis = list(title = ~unique(confirmed)))
      }

      data <- retrieveSelection()

      data %>%
        group_by(confirmed, region) %>%
        do(n = NROW(.), d = density(log(.$value), adjust = 3, n = 32)) %>%
        tidy(d) %>%
        ungroup() %>%
        group_by(confirmed) %>%
        do(p = plot_area(.)) %>%
        .[["p"]] %>%
        subplot(nrows = 2, shareX = TRUE, titleY = TRUE) %>%
        layout(xaxis = list(title = "log(number of cases per week)"))

    })

  }

  shinyApp(ui, server)

}
