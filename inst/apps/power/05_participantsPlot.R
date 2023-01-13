particiantsUI <- function(id) {
  tagList(
    fixedRow(
      column(width = 6,
             includeMarkdown("txt/lowpower_text.md")
      ),
      column(width = 6,
             plotOutput(NS(id, "participants")),
             sliderInput(NS(id, "cases"), h4("Number of observations:"),
                         min = 50,
                         max = 2000,
                         value = 100, 
                         width = "100%")
      )
    )
  )
}

particiantsServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    output$participants <- renderPlot({
      
      ptab <- cbind(NULL, NULL)       
      
      for (i in seq(4, input$cases)){
        pwrt1 <- pwr::pwr.r.test(r=0.1, sig.level=0.05, power=NULL, n=i, alternative = "two.sided")
        pwrt2 <- pwr::pwr.r.test(r=0.2, sig.level=0.05, power=NULL, n=i, alternative = "two.sided")
        pwrt3 <- pwr::pwr.r.test(r=0.3, sig.level=0.05, power=NULL, n=i, alternative = "two.sided")
        ptab <- rbind(ptab, cbind(pwrt1$power, pwrt1$n,
                                  pwrt2$power, pwrt2$n,
                                  pwrt3$power, pwrt3$n))
      }
      
      #Combine data and name it
      ptab <- cbind(seq_len(nrow(ptab)), ptab)
      
      colnames(ptab) <- c("id","d = 0,1.power","d = 0,1.n",
                          "d = 0,2.power","d = 0,2.n",
                          "d = 0,3.power","d = 0,3.n")
      
      # get data into right format for ggplot2
      temp <- ptab |>
        as.data.frame() |>
        tidyr::gather(key = name, value = val, 2:7) |>
        tidyr::separate(col = name, into = c("group", "var"), sep = "\\.") |>
        tidyr::spread(key = var, value = val)
      
      temp$group <- factor(temp$group, 
                           levels = c("d = 0,1", "d = 0,2", "d = 0,3"))
      #Color Scheme
      cbPalette <- c("#C69472", "#8A8FA1", "#29303B", "#0057AD", "#C70C0B")
      
      # Plot it
      ggplot2::ggplot(temp, ggplot2::aes(x = `n`, y = power , color = group, linetype = group))+
        ggplot2::geom_hline(yintercept = 0.80, linetype = 1, color="darkgray", size=1.2)+
        ggplot2::geom_line(size=1) +
        ggplot2::theme_bw(base_size = 20) +
        ggplot2::geom_label(data = temp |> dplyr::filter(`n` == 50),
                            ggplot2::aes(label = group, fill=group),
                   #hjust = -1.0,
                   #vjust = 1.5,
                   size = 5,
                   color="white")+
        ggplot2::ylab("Power")+
        ggplot2::xlab("N")+
        #scale_color_manual(values=cbPalette)+
        #scale_fill_manual(values = cbPalette)+
        ggplot2::theme(legend.position="none",
              #legend.direction="horizontal",
              legend.title = ggplot2::element_blank())
      
    }, res = 96)
    
  })
}
