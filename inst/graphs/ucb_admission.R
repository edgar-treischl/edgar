# library(tidyverse)
# library(patchwork)
# library(showtext)



showplot <- function(variables) {
  sysfonts::font_add_google("Patua One", "Patua")
  showtext::showtext_auto()

  df <- as.data.frame(UCBAdmissions)

  df2 <- df |>
    dplyr::group_by(Gender, Admit) |>
    dplyr::summarise(
      Freq = sum(Freq)
    ) |>
    dplyr::mutate(Percent = Freq / sum(Freq) * 100)


  txt <- c("Admitted", "Rejected", "Admitted", "Rejected")
  num <- paste(round(df2$Percent, 2), "%")

  p1 <- df2 |>
    ggplot2::ggplot(ggplot2::aes(x = Gender, y = Percent, group = Gender, fill = Admit)) +
    ggplot2::geom_bar(stat = "identity", position = ggplot2::position_stack()) +
    ggplot2::geom_text(ggplot2::aes(label = paste(txt, num, sep = "\n" )),
              position = ggplot2::position_stack(vjust = 0.5),
              colour = "white", size = 4, fontface = "bold"
    ) +
    ggplot2::theme_minimal(base_size = 14) +
    ggplot2::scale_fill_manual(values = c("#264653", "#C51717")) +
    ggplot2::guides(fill = ggplot2::guide_legend(reverse = TRUE))+
    ggplot2::labs(title = "UCB Admission Rates",
         alt = "UCB Admission Rates by Edgar Treischl")+
    ggplot2::theme(text= ggplot2::element_text(family="Patua"))+
    ggplot2::theme(plot.title = ggplot2::element_text(size=20))+
    ggplot2::theme(legend.position="none")


  df_dep <- df |>
    dplyr::group_by(Dept) |>
    dplyr::mutate(Percent = Freq / sum(Freq) * 100)

  Dept.labs <- c(
    `A` = "Department A",
    `C` = "Department C",
    `F` = "Department F"
  )

  p2 <- df_dep |>
    dplyr::filter(Dept == "A" | Dept == "F") |>
    ggplot2::ggplot(ggplot2::aes(
      x = Gender, y = Percent, group = Gender,
      fill = Admit
    )) +
    ggplot2::geom_bar(stat = "identity", position = ggplot2::position_stack()) +
    ggplot2::geom_text(ggplot2::aes(label = round(Percent, 2)),
              position = ggplot2::position_stack(vjust = 0.5),
              colour = "white", size = 3, fontface = "bold"
    ) +
    ggplot2::theme_minimal(base_size = 14) +
    ggplot2::scale_fill_manual(values = c("#264653", "#C51717")) +
    ggplot2::facet_wrap(. ~ Dept,
               ncol = 1,
               labeller = ggplot2::as_labeller(Dept.labs)
    ) +
    ggplot2::theme(strip.text.x = ggplot2::element_text(
      size = 12, color = "black", face = "bold"
    )) +
    ggplot2::guides(fill = ggplot2::guide_legend(reverse = TRUE))+
    ggplot2::theme(text= ggplot2::element_text(family="Patua"))+
    ggplot2::theme(legend.position="none")

  cowplot::plot_grid(p1, p2)


}

showplot()



