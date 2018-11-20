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
                    "Choice menus slider?",
                    value = TRUE)
      ,
      textInput("g_i",
                "Choice menu: Payment options to public good",
                value = "300,250,200,150,100,50,0")
      ,
      textInput("p_i",
                "Choice menu: Payment options to private good",
                value = "0,90,150,175,200,209,220") 
      ,
      sliderInput("cs",
                  "Private choice sets",
                  min = 1,
                  max = 6,
                  value = 1,
                  step = 1)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("Plot"),
      tableOutput("table"),
      tableOutput("table2")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$Plot <- renderPlot({

    p1 <- as.numeric(unlist(strsplit(input$p_i, ","))) 
    g1 <- as.numeric(unlist(strsplit(input$g_i, ","))) 
    if(input$y == TRUE){   
    if(input$cs == 1){    
    p <- as.numeric(c(0,110,250,300,350,370,380)) 
    g <- as.numeric(c(600,500,400,300,200,100,0)) 
    }
    if(input$cs == 2){    
      p <- as.numeric(c(0,120,260,310,360,380,390)) 
      g <- as.numeric(c(600,500,400,300,200,100,0)) 
    }
    if(input$cs == 3){    
      p <- as.numeric(c(0,130,270,320,370,390,400)) 
      g <- as.numeric(c(600,500,400,300,200,100,0)) 
    }
    if(input$cs == 4){    
      p <- as.numeric(c(0,140,280,330,380,400,410)) 
      g <- as.numeric(c(600,500,400,300,200,100,0)) 
    }
    if(input$cs == 5){    
      p <- as.numeric(c(0,150,290,340,390,410,420)) 
      g <- as.numeric(c(600,500,400,300,200,100,0)) 
    }
    if(input$cs == 6){    
      p <- as.numeric(c(0,160,300,350,400,420,430)) 
      g <- as.numeric(c(600,500,400,300,200,100,0)) 
    }
  }
    n <- input$n 
    k <- input$k
    if(input$y == FALSE){
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
      geom_line(aes(x = seq, y = p), linetype = "dashed",color = "red") +
      geom_line(aes(x = seq, y = g), linetype = "dotted", color = "green") +
      geom_line(aes(x = seq, y = cum), linetype = "solid", color = "black") +
      #  geom_line(aes(x = seq, y = ne), linetype = "solid", color = "red") +
      scale_x_continuous(breaks = c(seq)) +
      geom_text(label ="NE", aes(x = ne, y = p[ne]), nudge_y = 3) +
      geom_text(label ="PO", aes(x = df$seq[df$cum == max(df$cum)], y = max(df$cum)), nudge_y = 5) +
      labs(title = "", subtitle = "dotted = public; dashed = private; solid = aggregated") +
      xlab("Option") +
      ylab("Contribution (private, public, acc, added") +
      theme_bw()
  })
  
  
  output$table <- renderTable({
    p1 <- as.numeric(unlist(strsplit(input$p_i, ","))) 
    g1 <- as.numeric(unlist(strsplit(input$g_i, ","))) 
    if(input$y == TRUE){   
      if(input$cs == 1){    
        p <- as.numeric(c(0,110,250,300,350,370,380)) 
        g <- as.numeric(c(600,500,400,300,200,100,0)) 
      }
      if(input$cs == 2){    
        p <- as.numeric(c(0,120,260,310,360,380,390)) 
        g <- as.numeric(c(600,500,400,300,200,100,0)) 
      }
      if(input$cs == 3){    
        p <- as.numeric(c(0,130,270,320,370,390,400)) 
        g <- as.numeric(c(600,500,400,300,200,100,0)) 
      }
      if(input$cs == 4){    
        p <- as.numeric(c(0,140,280,330,380,400,410)) 
        g <- as.numeric(c(600,500,400,300,200,100,0)) 
      }
      if(input$cs == 5){    
        p <- as.numeric(c(0,150,290,340,390,410,420)) 
        g <- as.numeric(c(600,500,400,300,200,100,0)) 
      }
      if(input$cs == 6){    
        p <- as.numeric(c(0,160,300,350,400,420,430)) 
        g <- as.numeric(c(600,500,400,300,200,100,0)) 
      }
    }
    n <- input$n 
    k <- input$k
    seq <- c(1:length(p))
    p_plus_g <- p + g*n/k # PO
#    diffG <- (g[seq] - g[seq+1])/k
#    diffP <- p[seq+1] - p[seq] 
#    PGK <- diffP-diffG
#    SGK <- (n-1)*diffG
    
    df <- tibble(seq, p, g, p_plus_g)
    # ne <- df$seq[diffG > diffP & !is.na(diffG > diffP)] # NE
    #ne <- df$seq[p+(g/n) == max(p+(g/n))]
    df
  })
  
  
  output$table2 <- renderTable({
    p1 <- as.numeric(unlist(strsplit(input$p_i, ","))) 
    g1 <- as.numeric(unlist(strsplit(input$g_i, ","))) 
    if(input$y == TRUE){   
      if(input$cs == 1){    
        p <- as.numeric(c(0,110,250,300,350,370,380)) 
        g <- as.numeric(c(600,500,400,300,200,100,0)) 
      }
      if(input$cs == 2){    
        p <- as.numeric(c(0,120,260,310,360,380,390)) 
        g <- as.numeric(c(600,500,400,300,200,100,0)) 
      }
      if(input$cs == 3){    
        p <- as.numeric(c(0,130,270,320,370,390,400)) 
        g <- as.numeric(c(600,500,400,300,200,100,0)) 
      }
      if(input$cs == 4){    
        p <- as.numeric(c(0,140,280,330,380,400,410)) 
        g <- as.numeric(c(600,500,400,300,200,100,0)) 
      }
      if(input$cs == 5){    
        p <- as.numeric(c(0,150,290,340,390,410,420)) 
        g <- as.numeric(c(600,500,400,300,200,100,0)) 
      }
      if(input$cs == 6){    
        p <- as.numeric(c(0,160,300,350,400,420,430)) 
        g <- as.numeric(c(600,500,400,300,200,100,0)) 
      }
    }
    n <- input$n 
    k <- input$k
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
    df
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

