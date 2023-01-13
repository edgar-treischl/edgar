source("utils.R")
source("01_intro.R")
source("00_scatter_plot.R")
source("02_shoesize_plot.R")
source("03_mediator_plot.R")
source("04_collider_plot.R")

library(shiny)
library(patchwork)

ui <- fixedPage(

  #theme = material,
  theme = bslib::bs_theme(bootswatch = "journal"),

  # Application title
  #titlePanel("Power Analysis"),

  # Sidebar with a slider input for number of bins
  navbarPage("Causality", collapsible = TRUE,
             tabPanel("Start", icon = icon("play"),
                      scatterplotUI("scatter")
             ),
             tabPanel("Simulation", icon = icon("rocket"),
                      introUI("histogram")
             ),
             tabPanel("Shoe Size and Income", icon = icon("shoe-prints"),
                      shoesizeUI("shoes")
             ),
             tabPanel("Mediator", icon = icon("medium-m"),
                      meadiatorplotUI("mediator")
             ),
             tabPanel("Collider", icon = icon("cuttlefish"),
                      colliderplotUI("collider")
             )
  )
)

server <- function(input, output, session) {

  Sys.sleep(3) # do something that takes time
  scatterplotServer("scatter")
  introServer("histogram")
  shoesizeServer("shoes")
  mediatorplotServer("mediator")
  colliderplotServer("collider")


}

shinyApp(ui = ui, server = server)




