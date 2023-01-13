

library(shiny)

source("utils.R")



ui <- shinyUI(fixedPage(
    theme = bslib::bs_theme(bootswatch = "flatly"),
    
    waiter::use_waiter(),
    waiter::waiter_show_on_load(html = waiter::spin_flower()),
    
    # Sidebar with a slider input for number of bins 
    navbarPage("Who survived the Titanic?", collapsible = TRUE,
               tabPanel("00 Start", icon = icon("play"),
                        fixedRow(
                          column(width = 6,
                                 includeMarkdown("txt/start.md"),
                                 div(style="display: inline-block; width: 150px;",
                                     actionButton("code", "Source Code", 
                                                  onclick ="window.open('https://github.com/edgar-treischl/logistic-regression-app', '_blank')", 
                                                  icon = icon("github"))),
                                 div(style="display: inline-block; width: 150px;",
                                     actionButton("download", "Download", 
                                                  onclick ="window.open('https://github.com/edgar-treischl/titanic-app/blob/main/report.pdf')", 
                                                  icon = icon("file")))
                          ),
                          column(width = 6,
                                 tabsetPanel(type = "tabs", 
                                             tabPanel("Survival",
                                                      h4("Survival of the Titanic:"),  
                                                      plotOutput("survivedplot")),
                                             tabPanel("Sex", 
                                                      h4("Survival by Sex:"),
                                                      plotOutput("sexplot1")),
                                             tabPanel("Class",  
                                                      h4("Survival by Class:"),
                                                      plotOutput("classplot")),
                                             tabPanel("Age",  
                                                      h4("Survival by Age:"),
                                                      plotOutput("ageplot"))
                                 )
                          )
                        )
               ),
               tabPanel("01 Idea", icon = icon("lightbulb"),
                        fixedRow(
                          column(width = 6,
                                 includeMarkdown("txt/idea.md"),
                                 h4("Insert the logit in the scatterplot:"),
                                 checkboxInput("boolmethod", "Logit!", 
                                               value = FALSE)
                          ),
                          column(width = 6,
                                 tabsetPanel(type = "tabs",
                                             tabPanel("Scatterplot", plotOutput("scatter_log")),
                                             tabPanel("Function", plotOutput("funcplot"))
                                 )
                          )
                        )
               ),
               tabPanel("02 Variables", icon = icon("chart-pie"),
                        fixedRow(
                          column(width = 6,
                                 includeMarkdown("txt/variables.md")
                          ),
                          column(width = 6,
                                 HTML(paste(h4("Add a variable:"))),
                                 div(style="display: inline-block; width: 150px;",
                                     checkboxInput("booladults", "Age", value = FALSE)),
                                 div(style="display: inline-block; width: 150px;",
                                     checkboxInput("boolclass", "Class", value = FALSE)),
                                 plotOutput("alluvialplot")
                          )
                        )
               ),
               tabPanel("03 Model", icon = icon("robot"),
                        fixedRow(
                          column(width = 5,
                                 includeMarkdown("txt/model.md")
                          ),
                          column(width = 7,
                                 h4("Logistic regression results:"),
                                 p("Pick the independent variables:"),
                                 div(style="display: inline-block; width: 100px;",
                                     checkboxInput("bool1", "Sex", value = FALSE)),
                                 div(style="display: inline-block; width: 100px;",
                                     checkboxInput("bool2", "Class", value = FALSE)),
                                 div(style="display: inline-block; width: 100px;",
                                     checkboxInput("bool3", "Age", value = FALSE)),
                                 verbatimTextOutput("model")
                          )
                        )
               ),
               tabPanel("04 Odds Ratio", icon = icon("otter"),
                        fixedRow(
                          column(width = 6,
                                 includeMarkdown("txt/odds.md"),
                          ),
                          column(width = 6,
                                 tabsetPanel(type = "tabs",
                                             tabPanel("Barplot", plotOutput("sexplot2")),
                                             tabPanel("Calculator", verbatimTextOutput("odds"),
                                                      verbatimTextOutput("orperHand"))
                                 ),
                                 HTML(paste(h4("All the OR from the model you picked in the last pane:"))),
                                 plotOutput("modelor")
                          )
                        )
               ),
               tabPanel("05 Prediction", icon = icon("magic"),
                        fixedRow(
                          includeMarkdown("txt/prediction.md"),
                          column(width = 5,
                                 radioButtons("psex", h4("Sex"),
                                              choices = list("Male" = "male", "Female" = "female"), 
                                              selected = "female"),
                                 sliderInput("Age", 
                                             h4("Age"),
                                             min = 1,
                                             max = 100,
                                             value = 50),
                                 sliderInput("pclass", 
                                             h4("Class"),
                                             min = 1,
                                             max = 3,
                                             value = 2),
                                 div(style="display: inline-block; width: 300px;")
                          ),
                          column(width = 7,
                                 h4("The prediction result:"),
                                 plotOutput("predictionplot")
                          )
                        )
               ),
               tabPanel("06 Performance", icon = icon("hat-wizard"),
                        fixedRow(
                          column(width = 6,
                                 includeMarkdown("txt/performance.md"),
                          ),
                          column(width = 6,
                                 h4("Classification:"),
                                 plotOutput("performanceplot"),
                                 h4("ROC Plot:"),
                                 plotOutput("ROC")
                          )
                        )
               )
    )
  ))


train_df<- titanic::titanic_train

train_df$Survived <- factor(train_df$Survived, 
                            levels = c(0, 1),
                            labels = c("Not survived", "Survived")) 

train_df$Pclass <- factor(train_df$Pclass, 
                          levels = c(1, 2 , 3),
                          labels = c("First class", "Second class", "Third class")) 

glm_fit <- glm(Survived ~ Sex + Age + Pclass , family = binomial(link = 'logit'), data = train_df)


df <- expand.grid(Sex = c("female"), 
                  Age = c(18),
                  Pclass = 3)


#Server################
server <- shinyServer(function(input, output) {
  thematic::thematic_shiny()
  Sys.sleep(3) # do something that takes time
  waiter::waiter_hide()
  
  
  #Surivivalplots#####
  output$survivedplot <- renderPlot({
    ggplot2::ggplot(train_df, ggplot2::aes(x= Survived)) + 
      ggplot2::geom_bar(aes(y = (..count..)/sum(..count..)), fill=c("#999999"))+
      ggplot2::ylab("Percent")+
      ggplot2::theme_bw(base_size = 18)
    
  })
  
  output$sexplot1 <- renderPlot({
    ggplot2::ggplot(train_df, ggplot2::aes(x= Sex, fill = Survived)) + 
      ggplot2::geom_bar(aes(y = (..count..)/sum(..count..)))+
      ggplot2::theme_bw()+
      ggplot2::ylab("Percent")+
      ggplot2::theme(legend.position="bottom")+
      ggplot2::theme(text = ggplot2::element_text(size=18))+
      ggplot2::scale_fill_manual(values=c("#009E73","#E69F00"))
    
  })
  
  output$ageplot <- renderPlot({
    ggplot2::ggplot(train_df, ggplot2::aes(x = Age, fill = Survived)) + 
      ggplot2::geom_histogram()+
      ggplot2::theme_bw(base_size = 18)+
      ggplot2::ylab("")+
      ggplot2::theme(legend.position="bottom")+
      ggplot2::scale_fill_manual(values=c("#009E73","#E69F00"))
    
  })
  
  
  output$classplot <- renderPlot({
    ggplot2::ggplot(train_df, ggplot2::aes(x= Pclass, fill = Survived)) + 
      ggplot2::geom_bar(ggplot2::aes(y = (..count..)/sum(..count..)))+
      ggplot2::theme_bw(base_size = 18)+
      ggplot2::ylab("Percent")+
      ggplot2::theme(legend.position="bottom")+
      ggplot2::scale_fill_manual(values=c("#009E73","#E69F00"))
    
  })
  
  #IDEA: functional plots#####
  output$funcplot <- renderPlot({
    sigmoid <- function(x){
      return(1/(1 + exp(-x)))
    }
    
    probit <- function(x){
      return(pnorm(x))
    }
    
    ggplot2::ggplot(data.frame(x = c(-5, 5)), ggplot2::aes(x = x)) +
      ggplot2::stat_function(fun = sigmoid, color="#008080", size = 1)+
      ggplot2::stat_function(fun = probit, color="#2C3E50", size = 1)+
      ggplot2::annotate("text", x = 3, y = 0.85, label = c("Logit"), colour = "#008080", size = 6)+
      ggplot2::annotate("text", x = 0.85, y = 0.95, label = c("Probit"), colour = "#2C3E50", size = 6)+
      ggplot2::theme_bw(base_size = 18) +
      ggplot2::ggtitle("")
    
  })
  
  
  
  output$distplot2 <- renderPlot({
    set.seed(2)
    y <- rep(c(0,1), 500)
    x <- 10 + rnorm(250, 3, 3)+ rnorm(250, 10, 3)*y
    
    data <- data.frame(x, y) |> 
      tidyr::as_tibble()
    
    
    
    ggplot2::ggplot(data, ggplot2::aes(x = x, y = y)) + 
      ggplot2::geom_point(color = "gray")+
      ggplot2::geom_smooth(method='lm', formula= y~x, colour = "#008080")+
      ggplot2::theme_bw(base_size = 18)
    
  })
  
  
  output$scatter_log <- renderPlot({
    set.seed(2)
    y <- rep(c(0,1), 500)
    x <- 10 + rnorm(250, 3, 3)+ rnorm(250, 10, 3)*y
    
    data <- data.frame(x, y) |> 
      tidyr::as_tibble()
    
    if (input$boolmethod == TRUE) {
      ggplot2::ggplot(data, ggplot2::aes(x= x, y = y)) + 
        ggplot2::geom_point(color = "gray")+
        ggplot2::geom_smooth(method = "glm", 
                             method.args = list(family = "binomial"), colour = "#008080")+
        ggplot2::theme_bw(base_size = 18)
    } else {
      ggplot2::ggplot(data, ggplot2::aes(x = x, y = y)) + 
        ggplot2::geom_point(color = "gray")+
        ggplot2::geom_smooth(method='lm', formula= y~x, colour = "#008080")+
        ggplot2::theme_bw(base_size = 18)
      
    }
    
  })
  
  #Alluvialplot#####
  output$alluvialplot <- renderPlot({
    
    thematic::thematic_off()
    
    if (input$booladults == TRUE & input$boolclass == FALSE) {
      make_alluvial(Sex, Adults)
    } else if (input$booladults == FALSE & input$boolclass == TRUE) {
      make_alluvial(Sex, Pclass)
    } else if (input$booladults == TRUE & input$boolclass == TRUE) {
      make_alluvial(Sex, Adults, Pclass)
    } else {
      make_alluvial(Sex)
    }
    
    
  })
  
  
  
  #Model: output########
  output$model <- renderPrint({
    if (input$bool1 == TRUE & input$bool2 == FALSE &  input$bool3 == FALSE) {
      summary(model_call("m1"))
    } else if (input$bool1 == FALSE & input$bool2 == TRUE & input$bool3 == FALSE) {
      summary(model_call("m2"))
    } else if (input$bool1 == FALSE & input$bool2 == FALSE & input$bool3 == TRUE) {
      summary(model_call("m3"))
    } else if (input$bool1 == TRUE & input$bool2 == TRUE & input$bool3 == FALSE) {
      summary(model_call("m4"))
    } else if (input$bool1 == TRUE & input$bool2 == TRUE & input$bool3 == TRUE) {
      summary(model_call("m5"))
    } else if (input$bool1 == TRUE & input$bool2 == FALSE & input$bool3 == TRUE) {
      summary(model_call("m6"))
    } else if (input$bool1 == FALSE & input$bool2 == TRUE & input$bool3 == TRUE) {
      summary(model_call("m7"))
    } else {
      print("Pick at least one independent variable, my friend .-)")
    }
  })
  
  #ODDS RATIO#####
  output$sexplot2 <- renderPlot({
    thematic::thematic_on()
    
    ggplot2::ggplot(train_df, ggplot2::aes(x= Sex, fill = Survived)) + 
      ggplot2::geom_bar()+
      ggplot2::geom_text(stat='count', aes(label=..count..), vjust=-1,
                         position = ggplot2::position_stack(vjust = 0), size = 6)+
      ggplot2::theme_bw(base_size = 18)+
      ggplot2::ylab("Count")+
      ggplot2::theme(legend.position="bottom")
  })
  
  
  output$odds <- renderText({ 
    "Men's odds: 109/469:  0.232906 \nWomen's odds: 233/81: 2.876543 \nMen's odds ratio to survive: 0.232906/2.876543"
    
  })
  
  
  output$orperHand <- renderPrint({
    Oddswoman <- 233/81
    Oddswoman
    
    Oddsman <- 109/468
    Oddsman
    
    ORman <- round(Oddsman/Oddswoman, 3)
    ORman
    
  })
  
  
  #OR for each model#####
  output$modelor <- renderPlot({
    thematic::thematic_off()
    
    if (input$bool1 == TRUE & input$bool2 == FALSE & input$bool3 == FALSE) {
      plotOR("m1")
    } else if (input$bool1 == FALSE & input$bool2 == TRUE & input$bool3 == FALSE) {
      plotOR("m2")
    } else if (input$bool1 == FALSE & input$bool2 == FALSE & input$bool3 == TRUE) {
      plotOR("m3")
    } else if (input$bool1 == TRUE & input$bool2 == TRUE & input$bool3 == FALSE) {
      plotOR("m4")
    } else if (input$bool1 == TRUE & input$bool2 == TRUE & input$bool3 == TRUE) {
      plotOR("m5")
    } else if (input$bool1 == TRUE & input$bool2 == FALSE & input$bool3 == TRUE) {
      plotOR("m6")
    } else if (input$bool1 == FALSE & input$bool2 == TRUE & input$bool3 == TRUE) {
      plotOR("m7")
    } else {
      print("Still, you have to pick at least one independent variable, my friend .-)")
    }
    
  })
  
  
  #Prediction#####
  output$predictionplot <- renderPlot({
    glm_fit <- glm(Survived ~ Sex + Age + as.numeric(Pclass) , family = binomial(link = 'logit'), data = train_df)
    df$Age <- input$Age
    df$Sex <- input$psex
    df$Pclass <- input$pclass
    df$total <- 100
    df$prediction <- round(predict(glm_fit, df, type = 'response'),3)*100
    
    thematic::thematic_off()
    
    
    ggplot2::ggplot(df, ggplot2::aes(x=Age, y=prediction)) + 
      ggplot2::geom_col(fill = "#E69F00") +
      ggplot2::geom_col(ggplot2::aes(y = total), alpha = 0.01, colour = "gray")+
      ggplot2::geom_text(ggplot2::aes(label = paste(prediction, "%")), 
                         hjust = 0, color="#2C3E50", size = 8)+
      ggplot2::coord_flip()+
      ggplot2::theme_minimal(base_size = 18) +
      ggplot2::ylab("Predicted probability of survival")+
      ggplot2::xlab("")+
      ggplot2::theme(axis.title.y=ggplot2::element_blank(), 
                     axis.text.y=ggplot2::element_blank(),
                     axis.ticks.y=ggplot2::element_blank())
  })
  
  
  #Performance#####
  output$performanceplot <- renderPlot({
    thematic::thematic_off()
    pred_plot
  })
  
  
  #ROC plot#####
  output$ROC <- renderPlot({
    thematic::thematic_off()
    roc_curve
    
  })
  
  
  
})

shinyApp(ui = ui, server = server)




