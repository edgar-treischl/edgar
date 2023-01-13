



estimateUI <- function(id) {
  tagList(
    fixedRow(
      column(width = 6,
             includeMarkdown("txt/estimate_text.md"),
             verbatimTextOutput(NS(id, "model"))
      ),
      column(width = 6,
             sliderInput(NS(id, "strength"), h4("Effect (d):"),
                         min = 0.1, max = 1,
                         value = 0.5, step = 0.1, width = "100%"),
             plotOutput(NS(id, "Nplot")),
             sliderInput(NS(id, "power"), h4("Power:"),
                         min = 0.1, max = 0.99,
                         value = 0.8, step = 0.1, width = "100%")
             )
    )
  )
}

estimateServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    output$Nplot <- renderPlot({
      
      
      df <- pwr::pwr.t.test(d=input$strength, sig.level=0.05, power=input$power, n=NULL,
                       type = "two.sample", alternative = "two.sided")
      
      n <- round(df$n)
      
      x <- "N"
      y <- runif(n)
      
      df2 <- data.frame(x, y, n)
      
      df2 |>
        dplyr::mutate(x = paste0(x, "\n", "n=", n))|>
        ggplot2::ggplot(ggplot2::aes(x=x, y=y, color = y))+
        ggbeeswarm::geom_quasirandom(size = 3)+
        ggplot2::theme_bw(base_size = 24)+
        ggplot2::theme(legend.position = "none")+
        ggplot2::xlab("")+
        ggplot2::ylab("")+
        ggplot2::theme(axis.title.y=ggplot2::element_blank(), 
              axis.text.y=ggplot2::element_blank(),
              axis.ticks.y=ggplot2::element_blank())
      
    }, res = 96)
    
    output$model <- renderPrint({
      #pwr.r.test(r=input$strength, sig.level=0.05, power=input$power, n=NULL, 
      #alternative = "two.sided")
      
      pwr::pwr.t.test(d=input$strength, 
                      sig.level=0.05, 
                      power=input$power, 
                      n=NULL,
                      type = "two.sample", 
                      alternative = "two.sided")
      
      
      
    })
    
  })
}
