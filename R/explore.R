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

  zSD <- SharedData$new(z, ~location)

  zDiff <- z %>%
    group_by(location, country, region, report_type) %>%
    do(value = c(0, diff(.$value))) %>%
    unnest() %>%
    mutate(report_date = z$report_date) %>%
    ungroup()

  zDiffSD <- SharedData$new(zDiff, ~location)

  countries <- unique(z[["country"]])
  countriesInSubplot <- setdiff(countries, "Colombia")
  locations <- unique(z[["location"]])

  # coloring palette for report types
  pal <- c(confirmed = "#e41a1c", suspected = "#377eb8")

  ui <- fluidPage(
    fluidRow(
      column(
        5,
        leafletOutput("map", height = 600)
      ),
      column(
        7,
        checkboxInput("cumulative", "Show cumulative counts", value = TRUE),
        tabsetPanel(
          tabPanel(
            "Time Series", plotlyOutput("timeSeries", height = 1000), value = "all"
          ),
          tabPanel("Colombia", plotlyOutput("colombia"), value = "colombia"),
          tabPanel("Densities", plotlyOutput("densities")),
          id = "tabset"
        ))
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

    getZikaData <- reactive({
      need(input$cumulative, "Choose cumulative value")
      if (identical(input$cumulative, FALSE)) zDiffSD else zSD
    })

    output$timeSeries <- renderPlotly({
      # update map & possibly prompt google search on click
      res <- fitMapToLocation()
      googleSearch()


      base <- getZikaData() %>%
        plot_ly(x = ~report_date, y = ~value, color = ~report_type,
                colors = pal, alpha = 0.5, text = ~location,
                source = "timeSeriesSubplot") %>%
        group_by(location)

      plots <- lapply(countriesInSubplot, function(cntry) {
        base %>%
          filter(country %in% cntry) %>%
          add_trace(type = "scatter", mode = "markers+lines",
                    hoverinfo = "x+y+text+name", marker = list(size = 6)) %>%
          layout(
            xaxis = list(title = ""),
            yaxis = list(
              title = cntry,
              titlefont = list(size = 14),
              tickfont = list(size = 12)
            )
          )
      })
      subplot(plots, nrows = length(plots), shareX = TRUE, titleY = TRUE) %>%
        crosstalk("plotly_hover") %>%
        layout(dragmode = "zoom")
    })

    # open a google search on click
    googleSearch <- reactive({
      d <- event_data("plotly_click", "timeSeriesSubplot")
      if (isTRUE(d$key %in% unique(zika$location))) {
        browseURL(sprintf("http://google.com/#q=%s", d$key))
      }
      invisible()
    })

    # A reactive expression that returns the locations that are
    # in bounds right now
    mapClickData <- reactive({
      id <- input$map_marker_click$id
      if (is.null(id)) {
        return(NULL)
      }
      getZikaData()$origData() %>% filter(location %in% id) %>% mutate(region = id)
    })

    fitMapToLocation <- reactive({
      d <- if (identical(input$tabset, "colombia")) {
        filter(z, country %in% "Colombia")
      } else if (identical(input$tabset, "timeSeriesSubplot")) {
        filter(z, !country %in% "Colombia")
      }
      # if we click on a location's time-series, zoom to that location
      eventData <- event_data("plotly_click", "timeSeriesSubplot")
      if (isTRUE(eventData$key %in% locations)) {
        d <- filter(d, location %in% eventData$key)
      }
      latRng <- range(d$lat)
      lngRng <- range(d$lng)
      leafletProxy("map", session) %>%
        fitBounds(lngRng[1], latRng[1], lngRng[2], latRng[2])
    })

    output$colombia <- renderPlotly({
      res <- fitMapToLocation()

      getZikaData()$origData() %>%
        filter(country %in% "Colombia") %>%
        group_by(location) %>%
        plot_ly(x = ~report_date, y = ~value, alpha = 0.3) %>%
        add_trace(hoverinfo = "text", color = ~report_type, colors = pal,
                  marker = list(size = 6), mode = "markers+lines") %>%
        toWebGL()
    })

    # reactive that returns the zika data which is within the map bounds
    mapZoomData <- reactive({
      bounds <- input$map_bounds
      if (is.null(bounds)) {
        return(NULL)
      }
      latRng <- range(bounds$north, bounds$south)
      lngRng <- range(bounds$east, bounds$west)
      idx <- with(z, latRng[1] <= lat & lat <= latRng[2] & lngRng[1] <= lng & lng <= lngRng[2])
      if (all(idx)) {
        return(NULL)
      }
      getZikaData()$origData() %>% filter(idx) %>% mutate(region = "Inside Map")
    })

    retrieveSelection <- reactive({
      zoomSelection <- mapZoomData()
      clickSelection <- mapClickData()
      d <- getZikaData()$origData()
      rbind(d, zoomSelection, clickSelection)
    })

    output$densities <- renderPlotly({

      plot_area <- function(.) {
        pal <- c(`All Regions` = "black", `Inside Map` = "red")
        plot_ly(., x = ~x, ymax = ~y, color = ~region, colors = pal) %>%
          add_area(alpha = 0.3) %>%
          layout(yaxis = list(title = ~unique(report_type)))
      }

      data <- retrieveSelection()

      data %>%
        group_by(report_type, region) %>%
        do(n = NROW(.), d = density(log(.$value), adjust = 3, n = 32)) %>%
        tidy(d) %>%
        ungroup() %>%
        #filter(y > 10^-3) %>%
        group_by(report_type) %>%
        do(p = plot_area(.)) %>%
        .[["p"]] %>%
        subplot(nrows = 2, shareX = TRUE, titleY = TRUE) %>%
        layout(xaxis = list(title = "log(number of cases per week)"))

    })

  }

  shinyApp(ui, server)

}
