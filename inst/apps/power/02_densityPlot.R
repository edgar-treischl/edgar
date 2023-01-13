densityUI <- function(id) {
  tagList(
    fixedRow(
      column(width = 6,
             includeMarkdown("txt/effect.md")
      ),
      column(width = 6,
             sliderInput(NS(id, "distmean"), h4("Mean differences:"),
                         min = 0, max = 60,
                         value = 30, step = 10, 
                         width = "100%"),
             plotOutput(NS(id, "distPlot")),
             sliderInput(NS(id, "distsd"), h4("Standard deviation:"),
                         min = 10, max = 60,
                         value = 30, step = 10, 
                         width = "100%")
      )
    )
  )
}


densityServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    output$distPlot <- renderPlot({
      thematic::thematic_on()
      
      x <- 1 + rnorm(1000, 80, 1 + input$distsd)
      y <- 1 + rnorm(1000, 80 + input$distmean, 1 + input$distsd)
      z <- rep(c("A", "B"), each = 500)
      
      df <- data.frame(x, y , z)
      
      df2 <- df |>
        tidyr::pivot_longer(!z, names_to = "Group", values_to = "outcome")
      
      
      ggplot2::ggplot(df2, ggplot2::aes(x = outcome, color = Group, fill = Group))+
        ggplot2::geom_density(alpha=0.5)+
        ggplot2::theme_bw(base_size = 20)+
        ggplot2::theme(legend.position="bottom")
      
    }, res = 96)
    
  })
}
