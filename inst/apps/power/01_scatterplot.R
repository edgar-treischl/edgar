scatterplotUI <- function(id) {
  tagList(
    fixedRow(
      column(width = 6,
             includeMarkdown("txt/start.Rmd")
      ),
      column(width = 6,
             sliderInput(NS(id, "corr"), h4("Correlation:"),
                         min = 0, max = 1,
                         value = 0.5, step = 0.1, width = "100%"),
             plotOutput(NS(id, "corPlot"))
      )
    )
  )
}



scatterplotServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    output$corPlot <-renderPlot({
      set.seed(6687)
      x <- 1 + rnorm(1000, 100, 500)
      df <- data.frame(x)
      df$random1<- runif(nrow(df),min=min(df$x),max=max(df$x))
      df$y <- df$x*input$corr+ df$random1*(1 - input$corr)
      
      ggplot2::ggplot(df, ggplot2::aes(x=x, y = y))+
        ggplot2::geom_jitter()+
        ggplot2::geom_smooth(method = lm, se = FALSE)+
        ggplot2::theme_bw(base_size = 20)+
        ggplot2::annotate(x=0, y=-1700, 
                 label=paste("R = ", round(cor(df$x, df$y),2)), 
                 geom="text", size=5)
      
    }, res = 96)
    
  })
}
