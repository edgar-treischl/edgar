
showplot <- function(variables) {
  gapminder::gapminder |>
    dplyr::filter (year == 2007) |>
    dplyr::mutate(pop=pop/1000000) |>
    dplyr::arrange(desc(pop)) |>
    ggplot2::ggplot(ggplot2::aes(x = log(gdpPercap),
               y = lifeExp,
               size = pop,
               color = continent)) +
    ggplot2::geom_point(alpha = 0.7)+
    ggplot2::scale_size(range = c(.1, 25),
                        guide = "none")+
    #scale_x_continuous(limits = c(5, 12.5)) +
    ggplot2::scale_y_continuous(limits = c(30, 90))+
    viridis::scale_color_viridis(
      discrete = TRUE, name = "Region", option = "viridis")+
    ggplot2::labs(x = "GDP per capita (Log)",
         y = "Life expectancy (2007)",
         caption = "Data: Gapminder") +
    ggplot2::theme_minimal(base_size = 16)+
    ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(size = 4),
                                reverse=TRUE))

}

showplot()
