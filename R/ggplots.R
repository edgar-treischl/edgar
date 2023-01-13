#' Make a pie chart
#'
#' @param data A character vector with one element.
#' @param fill The variable to fill the pie.
#' @param legend The default does not return a legend.
#'
#' @return A ggplot2 object.
#' @export
#'
#' @examples
#' ggpie(palmerpenguins::penguins, fill = island)

ggpie <- function(data, fill, legend = "none") {
  check <- legend == "none"
  df <- data |>
    dplyr::group_by({{fill}}) |>
    dplyr::summarise(n = dplyr::n()) |>
    dplyr::mutate(sum = sum(n),
                  freq = round(n/sum * 100, 2))


  ggplot2::ggplot(df, ggplot2::aes(x="", y=freq, fill={{fill}}))+
    ggplot2::geom_col(width = 1)+
    ggplot2::coord_polar(theta = "y")+
    ggplot2::geom_text(ggplot2::aes(label = paste0({{fill}}, "\n", freq)),
                       color = "white",
                       position = ggplot2::position_stack(vjust = 0.5))+
    ggplot2::theme_void()+
    ggplot2::theme(legend.position=legend)



}





#' Make a slope chart.
#'
#' @param data Data
#' @param times Times
#' @param outcome Outcome
#' @param group Group
#' @param color Color
#' @param alpha Alpha
#' @param linetype Linetype
#' @param size Size
#'
#' @return A ggplot2 object.
#' @export
#'
#' @examples
#' df <- tidyr::tribble(
#' ~times, ~country,  ~gdp, ~inc,
#' "1990",   "A",  22.3, TRUE,
#' "2000",   "A",  44.6, TRUE,
#' "1990",   "B",  12.3, FALSE,
#' "2000",   "B",  4.6, FALSE
#' )

#' ggslope(df,
#'         times = times,
#'         outcome = gdp,
#'         group = country)

ggslope <- function(data, times, outcome, group,
                    color = NULL,
                    alpha = NULL,
                    linetype = NULL,
                    size = NULL) {

  ggplot2::ggplot(data, ggplot2::aes(x = {{times}},
                                     y = {{outcome}},
                                     group = {{group}},
                                     color = {{color}},
                                     size = {{size}},
                                     linetype = {{linetype}})) +
    ggplot2::geom_line()+
    # left side y axis labels
    ggrepel::geom_text_repel(
      data = data |>  dplyr::filter(times == min({{times}})),
      ggplot2::aes(label = {{group}}),
      hjust = "left",
      box.padding = 0.10,
      point.padding = 0.10,
      segment.color = "gray",
      segment.alpha = 0.6,
      fontface = "bold",
      size = 3,
      nudge_x = -1.95,
      direction = "y",
      force = .5,
      max.iter = 3000)+
    ggrepel::geom_text_repel(
      data = data |>  dplyr::filter(times == max({{times}})),
      ggplot2::aes(label = {{group}}),
      hjust = "right",
      box.padding = 0.10,
      point.padding = 0.10,
      segment.color = "gray",
      segment.alpha = 0.6,
      fontface = "bold",
      size = 3,
      nudge_x = 1.95,
      direction = "y",
      force = .5,
      max.iter = 3000)+
    ggplot2::geom_text(ggplot2::aes(label = {{outcome}}),
                       size = 2.5)
}


utils::globalVariables(c("freq", "n", "."))

