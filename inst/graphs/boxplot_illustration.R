

showplot <- function(variables) {
  sysfonts::font_add_google("Lato", "lato")

  showtext::showtext_auto()

  median_mpg <- median(mtcars$mpg)
  min_mpg <- min(mtcars$mpg)
  max_mpg <- max(mtcars$mpg)


  intern_p <- ggplot2::ggplot(mtcars, ggplot2::aes(mpg)) +
    ggplot2::geom_boxplot() +
    ggplot2::coord_flip()


  dat <- ggplot2::ggplot_build(intern_p)$data[[1]]

  plot <- mtcars |>
    ggplot2::ggplot(ggplot2::aes(y = mpg)) +
    ggplot2::geom_boxplot(outlier.colour = "#C51717", colour = "black") +
    ggplot2::geom_segment(
      data = dat, ggplot2::aes(
        y = xmiddle, yend = xmiddle,
        x = ymin, xend = ymax
      ),
      colour = "#C51717", size = 1.5
    ) +
    ggplot2::coord_flip() +
    ggplot2::theme_minimal() +
    ggplot2::xlab("") +
    ggplot2::ylab("") +
    # median
    ggplot2::annotate("text", y = median_mpg, x = 0.42, label = "Median") +
    # max
    ggplot2::annotate("text",
      y = max_mpg - 1, x = -.52,
      label = "Maximum"
    ) +
    ggplot2::geom_segment(ggplot2::aes(
      x = -0.04, y = max_mpg - 1.3, # median
      xend = -.45, yend = max_mpg - 1.3
    ),
    arrow = ggplot2::arrow(length = ggplot2::unit(0.5, "cm")),
    size = 1
    ) +
    # outliers
    ggplot2::annotate("text",
      y = 36.5, x = 0,
      label = "Potential\noutliers",
      color = "#C51717",
      family = "lato"
    ) +
    # min
    ggplot2::annotate("text", y = min_mpg, x = -.52, label = "Minimum") +
    ggplot2::geom_segment(ggplot2::aes(
      x = -0.04, y = min_mpg, # median
      xend = -.45, yend = min_mpg
    ),
    arrow = ggplot2::arrow(length = ggplot2::unit(0.5, "cm")),
    size = 1
    ) +
    #geom_brace(aes(c(-.38, -0.48), c(15.425, 22.800), label = ""),
      #inherit.data = F, labelsize = 5, rotate = 270
    #) +
    ggplot2::annotate("text", y = 19.9, x = -.52, label = "Interquartile range") +
    ggplot2::ylim(5, 40) +
    ggplot2::xlim(-.65, .5) +
    ggplot2::theme(text = ggplot2::element_text(size = 16, family = "lato"))

  dots <- ggplot2::ggplot(mtcars, ggplot2::aes(x = mpg)) +
    ggplot2::geom_dotplot(
      binwidth = .65,
      fill = "#939191",
      color = "#939191"
    ) +
    ggplot2::scale_y_continuous(NULL, breaks = NULL) +
    ggplot2::theme_void()

  cowplot::ggdraw() +
    cowplot::draw_plot(plot,
      x = 0, y = .15,
      width = 1, height = .8
    ) +
    cowplot::draw_plot(dots,
      x = 0.21, y = 0,
      width = 0.642, height = .9
    )
}

showplot()

