# library(tidyverse)
# library(patchwork)
# simpson_scatter################
# library(showtext)


showplot <- function() {
  sysfonts::font_add_google("Patua One", "Patua")
  showtext::showtext_auto()

  set.seed(1)
  a <- data.frame(x = 5 + rnorm(100),
                  y = 5 + rnorm(100)) |> dplyr::mutate(y = y - x / 4)
  c <- a |>
    dplyr::mutate(x = x + 2) |>
    dplyr::mutate(y = y + 2)
  simps_df <- do.call(rbind, list(a, c))
  simps_df <- simps_df |> dplyr::mutate(Sex = rep(c("Men", "Women"), each = 100))

  p1 <- ggplot2::ggplot(simps_df, ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_point(size = 2, color = "black") +
    ggplot2::theme_minimal(base_size = 14) +
    ggplot2::geom_smooth(method = "lm", formula = y ~ x,
                color = "#C51717", se = FALSE)+
    ggplot2::labs(title = "Simpson's Paradox")+
    ggplot2::theme(text= ggplot2::element_text(family="Patua"))+
    ggplot2::theme(plot.title = ggplot2::element_text(size=24, face = "bold"))


  df_text <- data.frame(
    x = c(8, 8),
    y = c(5.5, 2.9),
    text = c("Women", "Men")
  )


  p2 <- ggplot2::ggplot(simps_df) +
    ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = Sex), size = 2) +
    ggplot2::geom_smooth(ggplot2::aes(x = x, y = y, color = Sex),
                method = "lm", formula = y ~ x, se = FALSE, fullrange = TRUE
    ) +
    ggplot2::scale_colour_manual(values = c("#C51717", "black")) +
    ggplot2::theme_minimal(base_size = 14) +
    ggplot2::theme(legend.position = "none") +
    ggplot2::geom_label(
      data = df_text,
      ggplot2::aes(x = x, y = y, label = text),
      fill = c("black", "#C51717"),
      size = 5,
      color = c("white", "white")
    )+
    ggplot2::theme(text= ggplot2::element_text(family="Patua"))



  cowplot::plot_grid(p1, p2)

}

showplot()

