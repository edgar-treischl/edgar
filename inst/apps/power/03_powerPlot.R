powerUI <- function(id) {
  tagList(
    fixedRow(
      column(width = 6,
             includeMarkdown("txt/power_text.md")
      ),
      column(width = 6,
             sliderInput(NS(id, "decimal2"), h4("Effect size group 1:"),
                         min = 0, max = 1,
                         value = 0.2, step = 0.1, 
                         width = "100%"),
             plotOutput(NS(id, "powerPlot")),
             sliderInput(NS(id, "decimal"), h4("Effect size group 2:"),
                         min = 0, max = 1,
                         value = 0.8, step = 0.1,
                         width = "100%")
      )
    )
  )
}

powerServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    output$powerPlot <- renderPlot({
      
      ptab <- cbind(NULL, NULL)       
      
      for (i in seq(4, 500)){
        pwrt1 <- pwr::pwr.r.test(r=input$decimal, sig.level=0.05, power=NULL, n=i, alternative = "two.sided")
        pwrt2 <- pwr::pwr.r.test(r=input$decimal2, sig.level=0.05, power=NULL, n=i, alternative = "two.sided")
        ptab <- rbind(ptab, cbind(pwrt1$power, pwrt1$n,
                                  pwrt2$power, pwrt2$n))
      }
      
      #Combine data and name it
      ptab <- cbind(seq_len(nrow(ptab)), ptab)
      
      colnames(ptab) <- c("id","r = 0,1.power","r = 0,1.n",
                          "r = 0,2.power","r = 0,2.n")
      
      # get data into right format for ggplot2
      temp <- ptab |>
        as.data.frame() |>
        tidyr::gather(key = name, value = val, 2:5) |>
        tidyr::separate(col = name, into = c("group", "var"), sep = "\\.") |>
        tidyr::spread(key = var, value = val)
      
      temp$group <- factor(temp$group, 
                           levels = c("r = 0,1", "r = 0,2"))
      #Color Scheme
      cbPalette <- c("#C69472", "#8A8FA1", "#29303B", "#0057AD", "#C70C0B")
      
      # Plot it
      ggplot2::ggplot(temp, ggplot2::aes(x = `n`, y = power , color = group, linetype = group))+
        ggplot2::geom_hline(yintercept = 0.80, linetype = 1, size=1.2, color="darkgray")+
        ggplot2::geom_line(size=1) +
        ggplot2::theme_bw(base_size = 20) +
        ggplot2::geom_label(data = temp |> dplyr::filter(`n` == 50),
                            ggplot2::aes(label = paste0("d: ", c(input$decimal, input$decimal2)), fill=group),
                   #hjust = -1.0,
                   #vjust = 1.5,
                   size = 5,
                   color="white")+
        ggplot2::ylab("Power")+
        ggplot2::xlab("N")+
        ggplot2::theme(legend.position="none",
              #legend.direction="horizontal",
              legend.title = ggplot2::element_blank())
      
      
    }, res = 96)
    
  })
}
