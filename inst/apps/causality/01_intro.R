introUI <- function(id) {
  tagList(
    fixedRow(
      column(width = 6,
             includeMarkdown("text/sim_text.Rmd")
      ),
      column(width = 6,
             tabsetPanel(
               tabPanel("Histogram",
                        plotOutput(NS(id, "HistPlot"))
               ),
               div(style="display: inline-block; width: 45%",
                   sliderInput(NS(id, "sim_y"), h4("Income:"),
                               min = 1000, max = 3000,
                               value = 2000, step = 200,
                               width = "100%")
               ),
               div(style="display: inline-block; width: 45%",
                   sliderInput(NS(id, "sim_bonus"), h4("Male bonus:"),
                               min = 0, max = 1500,
                               value = 500, step = 250, 
                               width = "100%")
               )
             )
      )
    )
  )
}


introServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    output$HistPlot <-renderPlot({
      y <- input$sim_y
      bonus <- input$sim_bonus
      df <- simulated_data(y = y, bonus = bonus)
      thematic::thematic_on()
      
      
      ggplot2::ggplot(df, ggplot2::aes(x=income, fill = sex)) +   
        ggplot2::geom_histogram()+
        ggplot2::theme_minimal()
      
      
    }, res = 96)
    
  })
}
