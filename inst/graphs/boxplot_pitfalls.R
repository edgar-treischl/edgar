#Boxplot pitfalls
showplot <- function(variables) {
  sysfonts::font_add_google("Patua One", "Patua")
  showtext::showtext_auto()

  data <- data.frame(
    Group = c(rep("A", 100), rep("B", 500), rep("C", 15)),
    Outcome = c(rnorm(100, 12, 1), rnorm(500, 13, 1), rnorm(15, 20, 4))
  )

  sample_size <- data |>
    dplyr::group_by(Group) |>
    dplyr::summarize(num=dplyr::n())

  data <- data |>
    dplyr::left_join(sample_size) |>
    dplyr::mutate(Groups = paste0(Group, "\n", "n=", num))

  p1 <- data |>
    ggplot2::ggplot(ggplot2::aes(x = Group, y = Outcome,
                        color = Group)) +
    ggplot2::geom_boxplot() +
    ggplot2::theme_minimal(base_size = 14)+
    ggplot2::scale_color_manual(values = c(rep("#969696", 4))) +
    ggplot2::theme(legend.position = "none") +
    ggplot2::ggtitle("A: A boxplot ...")+
    ggplot2::theme(text=ggplot2::element_text(family="Patua"))


  # Plot B
  p2 <- data |>
    ggplot2::ggplot(ggplot2::aes(x = Groups,
                                 y = Outcome,
                                 color = Group)) +
    ggplot2::geom_boxplot() +
    ggplot2::geom_jitter(color = "#d62828", size = 0.5, alpha = 0.6) +
    ggplot2::theme_minimal(base_size = 14)+
    ggplot2::scale_color_manual(values = c(rep("#969696", 4))) +
    ggplot2::theme(legend.position = "none") +
    ggplot2::ggtitle("B: A jitter boxplot")+
    ggplot2::theme(text=ggplot2::element_text(family="Patua"))

  #gridExtra::grid.arrange(p1, p2, ncol = 2)
  cowplot::plot_grid(p1, p2)


}

showplot()


