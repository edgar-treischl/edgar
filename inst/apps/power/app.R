#Power App#######
source("01_scatterplot.R")
source("02_densityPlot.R")
source("03_powerPlot.R")
source("04_estimatePlot.R")
source("05_participantsPlot.R")




# Run the application 

    ui <- fixedPage(
        theme = bslib::bs_theme(bootswatch = "minty"),
        waiter::use_waiter(), 
        waiter::waiter_show_on_load(html = waiter::spin_fading_circles()),
        
        navbarPage("Power Analysis",
                   tabPanel("Start", icon = icon("play"),
                            scatterplotUI("scatter")
                   ),
                   tabPanel("Effect size", icon = icon("balance-scale"),
                            densityUI("density")
                   ),
                   tabPanel("Power", icon = icon("plug"),
                            powerUI("power")
                   ),
                   tabPanel("Estimate", icon = icon("robot"),
                            estimateUI("estimate")
                   ),
                   tabPanel("Low Power", icon = icon("battery-quarter"),
                            particiantsUI("participants")
                   )
        )
        
    )

    server <- function(input, output, session) {
        scatterplotServer("scatter")
        densityServer("density")
        powerServer("power")
        estimateServer("estimate")
        particiantsServer("participants")
        
        waiter::waiter_hide()
        
    }

shinyApp(ui, server)  
    


