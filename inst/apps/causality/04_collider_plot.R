#' @import ggplot2
#' @import shiny
#' @import patchwork
#' 

colliderplotUI <- function(id) {
  tagList(
    fixedRow(
      column(width = 6,
             includeMarkdown("text/collider_txt.Rmd"),
                          h4("You must not include a collider, but you can do so to see the consequence!"),
             checkboxInput(NS(id, "boolcollider"), "Include Collider", 
                           value = FALSE),
             includeMarkdown("text/collider2_txt.Rmd")
             ),
      column(width = 6,
             tabsetPanel(
               tabPanel("Dot plot",
                        plotOutput(NS(id, "collider2"))
               ),
               tabPanel("Regression output",
                        verbatimTextOutput(NS(id, "model"))
                        ),
               tabPanel("Bivariate plots",
                        plotOutput(NS(id, "collider"))
               )
             ),
             div(style="display: inline-block; width: 45%",
                 sliderInput(NS(id, "coll_x"), h4("Effect of X on C:"),
                             min = 0, max = 1,
                             value = 0.5, step = 0.1, width = "100%")),
             div(style="display: inline-block; width: 45%",
                 sliderInput(NS(id, "coll_y"), h4("Effect of Y on C:"),
                             min = 0, max = 1,
                             value = 0.5, step = 0.1, width = "100%"))
      )
    )
  )
}

#' @import ggplot2
#' @import shiny
#' @import patchwork
#' 

colliderplotServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    output$model <- renderPrint({
      n <- 750
      x <- input$coll_x
      y <- input$coll_y
      
      df_coll <- collider(n = n, rx = x, ry = y)
      
      
      
      if (input$boolcollider == TRUE) {
        reg_collider <- summary(stats::lm(y~x + collider, data=df_coll))
        reg_collider
      } else {
        reg_collider <- summary(stats::lm(y~x, data=df_coll))
        reg_collider
      }
      
    })
    
    output$collider <- renderPlot({
      
      n <- 750
      x <- input$coll_x
      y <- input$coll_y
      
      df_coll <- collider(n = n, rx = x, ry = y)
      
      thematic::thematic_on()
      
      
      p1 <- ggplot2::ggplot(df_coll, ggplot2::aes(x=x, y=y)) +
        ggplot2::geom_point() + 
        ggplot2::geom_smooth(method=stats::lm, se=TRUE)+
        ggplot2::theme_minimal()
      
      p2 <-  ggplot2::ggplot(df_coll, ggplot2::aes(x=x, y=collider)) +
        ggplot2::geom_point() + 
        ggplot2::geom_smooth(method=stats::lm, se=TRUE)+
        ggplot2::theme_minimal()
      
      p3 <- ggplot2::ggplot(df_coll, ggplot2::aes(x=collider, y=y)) +
        ggplot2::geom_point() + 
        ggplot2::geom_smooth(method=lm, se=TRUE)+
        ggplot2::theme_minimal()
      
      
      p1 / (p2| p3)
      
    }, res = 96)
    
    output$collider2 <- renderPlot({
      
      n <- 750
      x <- input$coll_x
      y <- input$coll_y
      
      df_coll <- collider(n = n, rx = x, ry = y)
      
      thematic::thematic_on()
      
      
      if (input$boolcollider == TRUE) {
        dotwhisker::dwplot(stats::lm(y ~ x + collider, data = df_coll), 
                           vline = ggplot2::geom_vline(xintercept = 0, 
                                              colour = 'grey50', 
                                              linetype = 2), 
                           dot_args = list(size = 3, pch = 21))+
          ggplot2::theme_minimal(base_size = 26)+
          ggplot2::theme(legend.position="none")
      } else {
        dotwhisker::dwplot(stats::lm(y ~ x, data = df_coll), 
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

utils::globalVariables(c("lm"))
