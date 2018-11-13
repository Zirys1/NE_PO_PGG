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
library(plotly)
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
      textInput("g_i",
                "Choice menu: Payment options to public good",
                value = "300,250,200,150,100,50,0")
      ,
      textInput("p_i",
                "Choice menu: Payment options to private good",
                value = "0,90,150,175,200,209,220") 
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("Plot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$Plot <- renderPlot({
    
    p <- as.numeric(unlist(strsplit(input$p_i, ","))) #c(0,90,150,175,200,209,220)
    g <- as.numeric(unlist(strsplit(input$g_i, ","))) # c(300,250,200,150,100,50,0)  #as.numeric(unlist(strsplit(input$vec1,",")))
    n <- input$n 
    k <- input$k
    seq <- c(1:length(p))
    cum <- p + g*n/k # PO
    
    df <- tibble(seq, p, g, cum)
    diffG <- (g[seq] - g[seq+1])/n 
    diffP <- p[seq+1] - p[seq] 
    # ne <- df$seq[diffG > diffP & !is.na(diffG > diffP)] # NE
    ne <- df$seq[p+(g/n) == max(p+(g/n))]
    
    
    ggplot() +
      geom_line(aes(x = seq, y = p), linetype = "dashed",color = "red") +
      geom_line(aes(x = seq, y = g), linetype = "dotted", color = "green") +
      geom_line(aes(x = seq, y = cum), linetype = "solid", color = "black") +
      #  geom_line(aes(x = seq, y = ne), linetype = "solid", color = "red") +
      scale_x_continuous(breaks = c(seq)) +
      geom_text(label ="NE", aes(x = ne, y = p[ne]), nudge_y = 3) +
      geom_text(label ="PO", aes(x = df$seq[df$cum == max(df$cum)], y = max(df$cum)), nudge_y = 5) +
      labs(title = "", subtitle = "dotted = public; dashed = private; solid = aggregat") +
      xlab("Option") +
      ylab("Contribution (private, public, acc, added") +
      theme_bw()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

