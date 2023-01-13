scatterplotUI <- function(id) {
  tagList(
    fixedRow(
      column(width = 6,
             includeMarkdown("text/start.Rmd")
      ),
      column(width = 6,
             tabsetPanel(
               tabPanel("Scatterplot",
                        plotOutput(NS(id, "corPlot"))
               ),
               sliderInput(NS(id, "corr"), h4("Correlation:"),
                           min = 0, max = 1,
                           value = 0.5, step = 0.1, width = "100%")
             )
      )
    )
  )
}


scatterplotServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    output$corPlot <-renderPlot({
      set.seed(6687)
      x <- 1 + stats::rnorm(1000, 100, 500)
      df <- data.frame(x)
      df$random1<- stats::runif(nrow(df),min=min(df$x),max=max(df$x))
      df$y <- df$x*input$corr+ df$random1*(1 - input$corr)
      
      thematic::thematic_on()
      
      
      ggplot2::ggplot(df, ggplot2::aes(x=x, y = y))+
        ggplot2::geom_jitter()+
        ggplot2::geom_smooth(method = lm, se = FALSE)+
        ggplot2::theme_bw(base_size = 20)+
        ggplot2::annotate(x=0, y=-1700, 
                 label=paste("r = ", round(stats::cor(df$x, df$y),2)), 
                 geom="text", size=5)
      
    }, res = 96)
    
  })
}

utils::globalVariables(c("lm"))
