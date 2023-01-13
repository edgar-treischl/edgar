#' @import ggplot2
#' @import shiny
#' 
#' 

meadiatorplotUI <- function(id) {
  tagList(
    fixedRow(
      column(width = 6,
             includeMarkdown("text/mediator_text.Rmd"),
             h4("To include the mediator:"),
             checkboxInput(NS(id, "boolmediator"), "Mediator included", 
                           value = FALSE),
      ),
      column(width = 6,
             tabsetPanel(
               tabPanel("Dotplot",
                        plotOutput(NS(id, "powerPlot"))
               ),
               tabPanel("Regression Output", verbatimTextOutput(NS(id, "mediator"))
                        )
             ),
             div(style="display: inline-block; width: 45%",
                 sliderInput(NS(id, "m_mediator"), h4("Effect of X via M:"),
                             min = 0, max = 1,
                             value = .4, step = .1, 
                             width = "100%")
             ),
             div(style="display: inline-block; width: 45%",
                 sliderInput(NS(id, "w_mediator"), h4("Direct effect of X:"),
                             min = 0, max = 1,
                             value = 0.4, step = 0.1,
                             width = "100%")
                 )
      )
    )
  )
}


#' @import ggplot2
#' @import shiny
#' 

mediatorplotServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    output$mediator <- renderPrint({
      
      fallzahl <- 750
      #input$n_mediator
      mediation <- input$m_mediator
      xeffect <- input$w_mediator
      #n <- input$n_mediator
      #m <- input$m_mediator
      #w <- input$w_mediator
      df_mediator <- mediator_new(fallzahl, mediation, xeffect)
      
      
      if (input$boolmediator == TRUE) {
        reg_mediator <- summary(stats::lm(y~x + mediator, data=df_mediator))
        reg_mediator
      } else {
        reg_mediator <- summary(stats::lm(y~x, data=df_mediator))
        reg_mediator
      }
    })
    
    output$powerPlot <- renderPlot({
      
      n <- 750
      #input$n_mediator
      m <- input$m_mediator
      w <- input$w_mediator
      df_mediator <- mediator(n, m, w)
      
      thematic::thematic_on()
      
      
      
      if (input$boolmediator == TRUE) {
        dotwhisker::dwplot(stats::lm(y ~ x + mediator, data = df_mediator), 
               vline = ggplot2::geom_vline(xintercept = 0, 
                                  colour = 'grey50', 
                                  linetype = 2), 
               dot_args = list(size = 3, pch = 21))+
          ggplot2::theme_minimal(base_size = 26)+
          ggplot2::theme(legend.position="none")
      } else {
        dotwhisker::dwplot(stats::lm(y ~ x, data = df_mediator), 
               vline = ggplot2::geom_vline(xintercept = 0, 
                                  colour = 'grey50', 
                                  linetype = 2), 
               dot_args = list(size = 3, pch = 21))+
          ggplot2::theme_minimal(base_size = 26)+
          ggplot2::theme(legend.position="none")
      }
      
    }, res = 96)
    
    
  })
}
