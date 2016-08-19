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

  varsToShow <- c("location", "report_date", "value", "report_type")
  txt <- Reduce(paste, lapply(z[varsToShow], function(x) paste(x, "<br />")))

  z <- z %>% mutate(txt = txt)

  zDiff <- z %>%
    group_by(location, country, region, report_type) %>%
    do(value = c(0, diff(.$value))) %>%
    unnest() %>%
    mutate(report_date = z$report_date) %>%
    ungroup() %>% left_join(z)

  zDiffSD <- SharedData$new(zDiff, ~location)

  countries <- unique(zDiff$country)
  countriesInSubplot <- setdiff(countries, "Colombia")

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
      # make sure we're showing a "global" map view
      res <- fitMapBounds()

      # modify attachKey to respect crosstalk
      #googleSearch()
      base <- getZikaData() %>%
        plot_ly(x = ~report_date, y = ~value, color = ~report_type, text = ~location) %>%
        group_by(location)

      plots <- lapply(countriesInSubplot, function(cntry) {
        base %>%
          filter(country %in% cntry) %>%
          add_trace(hoverinfo = "x+y+text+name", marker = list(size = 6), mode = "markers+lines") %>%
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
        crosstalk("plotly_hover", "plotly_deselect") %>%
        layout(dragmode = "zoom")
    })

    # open a google search on click
    googleSearch <- reactive({
      d <- event_data("plotly_click")
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

    fitMapBounds <- reactive({
      d <- if (identical(input$tabset, "colombia")) {
        filter(z, country %in% "Colombia")
      } else {
        filter(z, !country %in% "Colombia")
      }
      latRng <- range(d$lat)
      lngRng <- range(d$lng)
      leafletProxy("map", session) %>%
        fitBounds(lngRng[1], latRng[1], lngRng[2], latRng[2])
    })

    output$colombia <- renderPlotly({
      res <- fitMapBounds()

      getZikaData()$origData() %>%
        filter(country %in% "Colombia") %>%
        group_by(location) %>%
        plot_ly(x = ~report_date, y = ~value, text = ~txt, alpha = 0.3) %>%
        add_trace(hoverinfo = "text", color = ~report_type,
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
        # TODO: this needs to be fixed in plotly!
        cols <- if (length(unique(.$region)) > 1)  c("black", "green", "red") else "black"
        plot_ly(., x = ~x, ymax = ~y, color = ~region, colors = cols) %>%
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
