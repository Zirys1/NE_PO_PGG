#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(tibble)
library(formattable)
common_ops <-  list(theme(plot.title = element_text(size=8),
                                              axis.text.y = element_text(size = 14),
                                              axis.text.x = element_text(size = 14),
                                              axis.title = element_text(size = 14),
                                              strip.text = element_text(size = 14),
                                              legend.position="bottom",
                                              panel.grid.major=element_blank(),
                                              panel.grid.minor=element_blank(),
                                              panel.border=element_blank(),
                                              axis.line=element_line(),
                                              text=element_text()))

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("NE and PO in linear PGG"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput("n",
                  "Number of subjects per group",
                  min = 1,
                  max = 20,
                  value = 4,
                  step = 1)
      ,
      sliderInput("k",
                  "Number of subjects paid",
                  min = 1,
                  max = 20,
                  value = 4,
                  step = 1)
      ,
      checkboxInput("y",
                    "Manual choice menu input",
                    value = FALSE)
      ,
      conditionalPanel(
        condition = "input.y == 1",
        textInput("g_i",
                  "Choice menu: Payment options to public good",
                  value = "300,250,200,150,100,50,0")
        ,
        textInput("p_i",
                  "Choice menu: Payment options to private good",
                  value = "0,90,150,175,200,209,220") 
      )
      ,
      conditionalPanel(
        condition = "input.y == 0",
        sliderInput("cs",
                    "Private choice sets",
                    min = 1,
                    max = 8,
                    value = 1,
                    step = 1) 
      )
    ),

    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("Plot"),
      formattableOutput("table"),
      tableOutput("table2")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$Plot <- renderPlot({

    p1 <- as.numeric(unlist(strsplit(input$p_i, ","))) 
    g1 <- as.numeric(unlist(strsplit(input$g_i, ","))) 
   
     if(input$y == FALSE){  
      
    if(input$cs == 1){    
    p <- as.numeric(c(0,100,210,300,350,365,390)) 
    g <- as.numeric(c(600,500,400,300,200,100,0)) 
    }
    if(input$cs == 2){    
      p <- as.numeric(c(0,100,210,300,350,370,390)) 
      g <- as.numeric(c(600,500,400,300,200,100,0)) 
    }
    if(input$cs == 3){    
      p <- as.numeric(c(0,100,210,305,350,365,390)) 
      g <- as.numeric(c(600,500,400,300,200,100,0)) 
    }
    if(input$cs == 4){    
      p <- as.numeric(c(0,100,210,305,350,370,390)) 
      g <- as.numeric(c(600,500,400,300,200,100,0)) 
    }
    if(input$cs == 5){    
      p <- as.numeric(c(0,105,210,300,350,365,390)) 
      g <- as.numeric(c(600,500,400,300,200,100,0)) 
    }
        if(input$cs == 6){    
         p <- as.numeric(c(0,105,210,300,350,370,390)) 
         g <- as.numeric(c(600,500,400,300,200,100,0)) 
       }
                if(input$cs == 7){    
     p <- as.numeric(c(0,105,210,305,350,365,390)) 
     g <- as.numeric(c(600,500,400,300,200,100,0)) 
   }
   if(input$cs == 8){    
     p <- as.numeric(c(0,105,210,305,350,370,390)) 
     g <- as.numeric(c(600,500,400,300,200,100,0)) 
   }
  }
    
    n <- input$n 
    k <- input$k
    if(input$y == TRUE){
      p <- p1
      g <- g1
    }
    seq <- c(1:length(p))
    cum <- p + g*n/k # PO
    diffG <- (g[seq] - g[seq+1])/k 
    diffP <- p[seq+1] - p[seq] 
    PGK <- diffP-diffG
    SGN <- (n-1)*diffG
    
    df <- tibble(seq, p, g, cum, PGK)


    # ne <- df$seq[diffG > diffP & !is.na(diffG > diffP)] # NE
    ne <- df$seq[p+(g/k) == max(p+(g/k))]
    #ne <- df$seq[PGK == min(PGK, na.rm = T) & !is.na(PGK)]

    
    ggplot() +
      geom_line(aes(x = seq, y = p), linetype = "dashed",color = "#FA614B") +
      geom_line(aes(x = seq, y = g), linetype = "dotted", color = "#4A7023") +
      geom_line(aes(x = seq, y = cum), linetype = "solid", color = "black") +
      #  geom_line(aes(x = seq, y = ne), linetype = "solid", color = "red") +
      scale_x_continuous(breaks = c(seq)) +
      geom_text(label ="NE", aes(x = ne, y = p[ne]), nudge_y = 3, size = 6) +
      geom_text(label ="PO", aes(x = df$seq[df$cum == max(df$cum)], y = max(df$cum)), nudge_y = 3, size = 6) +
      labs(title = "", subtitle = "dotted = public; dashed = private; solid = aggregated") +
      xlab("Option") +
      ylab("Contribution (private, public, acc, added") +
      theme_bw() +
      common_ops +
      theme(panel.grid.major.x = element_line(size=.1, colour = "grey"),
            panel.grid.major.y = element_blank())
  })
  
  
  output$table <- renderFormattable({
    p1 <- as.numeric(unlist(strsplit(input$p_i, ","))) 
    g1 <- as.numeric(unlist(strsplit(input$g_i, ","))) 
    if(input$y == FALSE){  
      
      if(input$cs == 1){    
        p <- as.numeric(c(0,100,210,300,350,365,390)) 
        g <- as.numeric(c(600,500,400,300,200,100,0)) 
      }
      if(input$cs == 2){    
        p <- as.numeric(c(0,100,210,300,350,370,390)) 
        g <- as.numeric(c(600,500,400,300,200,100,0)) 
      }
      if(input$cs == 3){    
        p <- as.numeric(c(0,100,210,305,350,365,390)) 
        g <- as.numeric(c(600,500,400,300,200,100,0)) 
      }
      if(input$cs == 4){    
        p <- as.numeric(c(0,100,210,305,350,370,390)) 
        g <- as.numeric(c(600,500,400,300,200,100,0)) 
      }
      if(input$cs == 5){    
        p <- as.numeric(c(0,105,210,300,350,365,390)) 
        g <- as.numeric(c(600,500,400,300,200,100,0)) 
      }
      if(input$cs == 6){    
        p <- as.numeric(c(0,105,210,300,350,370,390)) 
        g <- as.numeric(c(600,500,400,300,200,100,0)) 
      }
      if(input$cs == 7){    
        p <- as.numeric(c(0,105,210,305,350,365,390)) 
        g <- as.numeric(c(600,500,400,300,200,100,0)) 
      }
      if(input$cs == 8){    
        p <- as.numeric(c(0,105,210,305,350,370,390)) 
        g <- as.numeric(c(600,500,400,300,200,100,0)) 
      }
    }    
    n <- input$n 
    k <- input$k
    if(input$y == TRUE){
      p <- p1
      g <- g1
    }
    seq <- c(1:length(p))
    p_plus_g <- p + g*n/k # PO
#    diffG <- (g[seq] - g[seq+1])/k
#    diffP <- p[seq+1] - p[seq] 
#    PGK <- diffP-diffG
#    SGK <- (n-1)*diffG
    p_plus_g_div_k <- p + (g/k)
    
    df <- tibble(seq, p, g, p_plus_g, p_plus_g_div_k)
    colnames(df) <-     c("Option", "p_i", "g_i", "p_i+g_i", "p_i+(g_i)/k")
    df <- formattable(df, align = c("l", rep("r", ncol(df) - 1)),
                list("p_i+g_i" = proportion_bar(color= ifelse(p_plus_g == max(p_plus_g), "#457371", "#4A7023")),
                     "p_i+(g_i)/k" = proportion_bar(color = ifelse(p_plus_g_div_k == max(p_plus_g_div_k), "#457371", "#FA614B"))))
    # ne <- df$seq[diffG > diffP & !is.na(diffG > diffP)] # NE
    #ne <- df$seq[p+(g/n) == max(p+(g/n))]
    df
  })
  
  
  output$table2 <- renderTable({
    p1 <- as.numeric(unlist(strsplit(input$p_i, ","))) 
    g1 <- as.numeric(unlist(strsplit(input$g_i, ","))) 
    if(input$y == FALSE){  
      
      if(input$cs == 1){    
        p <- as.numeric(c(0,100,210,300,350,365,390)) 
        g <- as.numeric(c(600,500,400,300,200,100,0)) 
      }
      if(input$cs == 2){    
        p <- as.numeric(c(0,100,210,300,350,370,390)) 
        g <- as.numeric(c(600,500,400,300,200,100,0)) 
      }
      if(input$cs == 3){    
        p <- as.numeric(c(0,100,210,305,350,365,390)) 
        g <- as.numeric(c(600,500,400,300,200,100,0)) 
      }
      if(input$cs == 4){    
        p <- as.numeric(c(0,100,210,305,350,370,390)) 
        g <- as.numeric(c(600,500,400,300,200,100,0)) 
      }
      if(input$cs == 5){    
        p <- as.numeric(c(0,105,210,300,350,365,390)) 
        g <- as.numeric(c(600,500,400,300,200,100,0)) 
      }
      if(input$cs == 6){    
        p <- as.numeric(c(0,105,210,300,350,370,390)) 
        g <- as.numeric(c(600,500,400,300,200,100,0)) 
      }
      if(input$cs == 7){    
        p <- as.numeric(c(0,105,210,305,350,365,390)) 
        g <- as.numeric(c(600,500,400,300,200,100,0)) 
      }
      if(input$cs == 8){    
        p <- as.numeric(c(0,105,210,305,350,370,390)) 
        g <- as.numeric(c(600,500,400,300,200,100,0)) 
      }
    }    
    n <- input$n 
    k <- input$k
    if(input$y == TRUE){
      p <- p1
      g <- g1
    }
    seq <- c(1:length(p))
    p_plus_g <- p + g*n/k # PO
    diffG <- (g[seq] - g[seq+1])/k 
    diffP <- p[seq+1] - p[seq] 
    PGK <- diffP-diffG
    SGN <- (n-1)*diffG
    df <- tibble(seq, diffP,diffG, PGK, SGN)
    
    seq2 <- paste(seq+1, "->", seq)
    
    df <- tibble(seq2, diffP,diffG, PGK, SGN)
    df <- df[-nrow(df),]
    # ne <- df$seq[diffG > diffP & !is.na(diffG > diffP)] # NE
 #   ne <- df$seq[p+(g/n) == max(p+(g/n))]
    colnames(df) <-     c("Option change", "dP=p_(i+1) - p_i", "dG=(g_(i+1) - g_i)/k", "dP-dG", "(n-1)*dG")
    
    df
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

