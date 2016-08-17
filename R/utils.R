#' Plot a running 5 number summary of zika data
#'
#' @param p a plotly visualization
#' @param location a regular expression matching the location column in the \link{zika} dataset.
#' @param a valid R colour.
#' @export
#' @examples
#'
#' brazil <- dplyr::filter(zika, country == "Brazil")
#'
#' plot_ly(zika, x = ~report_date) %>%
#'   add_summary() %>%
#'   add_summary(data = brazil, "red")

add_summary <- function(p, data = NULL, color = "black") {

  data <- if (!is.null(data)) data else plotly_data(p)

  d <- data %>%
    group_by(report_date) %>%
    summarise(
      min = min(value, na.rm = TRUE),
      q1 = quantile(value, 0.25, na.rm = TRUE),
      med = median(value, na.rm = TRUE),
      q3 = quantile(value, 0.75, na.rm = TRUE),
      max = max(value, na.rm = TRUE)
    )

    # # http://stackoverflow.com/questions/22523131/dplyr-summarise-equivalent-of-drop-false-to-keep-groups-with-zero-length-in
    # tidyr::complete(report_date, fill = list(min = 0, q1 = 0, med = 0, q3 = 0, max = 0))

  add_data(p, d) %>%
    add_ribbons(
      ymin = ~min, ymax = ~max, name = "Range", hoverinfo = "none",
      fillcolor = toRGB(color, 0.1), line = list(color = color)
    ) %>%
    add_ribbons(
      ymin = ~q1, ymax = ~q3, name = "IQR", hoverinfo = "none",
      fillcolor = toRGB(color, 0.5), line = list(color = color)
    ) %>%
    add_lines(y = ~med, line = list(color = color), name = "median")
}
