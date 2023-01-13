
shoesizeUI <- function(id) {
  tagList(
    fixedRow(
      column(width = 6,
             includeMarkdown("text/shoes.Rmd"),
             h4("Control for Sex:"),
             checkboxInput(NS(id, "boolmethod"), "Sex controlled.", 
                           value = FALSE),
             includeMarkdown("text/shoes2.Rmd")
      ),
      column(width = 6,
             tabsetPanel(
               tabPanel("Scatterplot",
                        plotOutput(NS(id, "distPlot"))
               ),
               tabPanel("Regression Output", 
                        verbatimTextOutput(NS(id, "model2"))
                        )
             ),
             div(style="display: inline-block; width: 45%",
                 sliderInput(NS(id, "bonus"), h4("Male bonus:"),
                             min = 0, max = 1000,
                             value = 500, step = 200, 
                             width = "100%")
             ),
             div(style="display: inline-block; width: 45%",
                 sliderInput(NS(id, "confounderSD"), h4("SD:"),
                             min = 100, max = 600,
                             value = 300, step = 50,
                             width = "100%")
                 )
             )
      
      )
    )

}

shoesizeServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    output$distPlot <- renderPlot({
      
      
      thematic::thematic_on()
      #input$confounderN
      df_confounder <- var_confounder(n = 750, 
                                      y = 2000, 
                                      sd = input$confounderSD,
                                      bonus = input$bonus)
      
      model <- stats::lm(income~schoesize, data=df_confounder)
      modelr2 <- performance::r2(model)
      modelr2 <- round(modelr2$R2, 2)
        
      lm.with<-stats::lm(income~schoesize + sex, data=df_confounder)
      lm.without<-stats::update(lm.with, ~. - schoesize)
      r_update <- round(asbio::partial.R2(lm.without,lm.with), 2)
      
      
      if (input$boolmethod == TRUE) {
        ggplot2::ggplot(data=df_confounder, 
                        ggplot2::aes(x=schoesize, y=income, color=sex)) +
          ggplot2::geom_smooth(method = lm)+
          ggplot2::geom_point()+
          ggplot2::theme_bw(base_size = 18)+
          ggplot2::theme(legend.position = "none")+
          ggplot2::labs(caption = paste("R-Squared = ", r_update))
      } else {
        ggplot2::ggplot(data=df_confounder, 
                        ggplot2::aes(x=schoesize, y=income)) +
          ggplot2::geom_smooth(method = lm)+
          ggplot2::geom_point()+
          ggplot2::theme_bw(base_size = 18)+
          ggplot2::labs(caption = paste("R-Squared =", modelr2))
        
      }
      
    }, res = 96)
    
    #input$confounderN
    output$model2 <- renderPrint({
      df_confounder <- var_confounder(n = 750, 
                                      y = 2000, 
                                      sd = input$confounderSD,
                                      bonus = input$bonus)
      
      if (input$boolmethod == TRUE) {
        summary(stats::lm(income~schoesize + sex, data=df_confounder))
      } else {
        summary(stats::lm(income~schoesize, data=df_confounder))
      }
    })
    
  })
}

utils::globalVariables(c("lm"))

