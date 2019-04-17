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
library(reshape)
# Define UI for application
ui <- fluidPage(
  
  # Application title
  titlePanel("Investment Modalities"),
  
  # Sidebar  
  fluidRow(
    column(4,
      sliderInput("initial",
                  label = "Initial Amount",
                  min = 0,
                  max = 100000,
                  value = 1000,
                  pre = "$"),
      sliderInput("acontrib",
                  label = "Annual Contribution",
                  min = 0,
                  max = 50000,
                  value = 2000,
                  pre = "$")),
    column(4,
      sliderInput("r",
                   label = "Return r(in %)",
                   min = 0,
                   max = 20,
                   value = 5
                   ),
      sliderInput("g",
                  label = "g r(in %)",
                  min = 0,
                  max = 20,
                  value = 2)),
    column(4,
      sliderInput("yr",
                  label = "yrs",
                  min = 0,
                  max = 50,
                  value = 20),
      selectInput("facet",
                  label = "Facet?",
                  choices = c("No","Yes")
                  ))
      
      
    ),
    
    # Show a plot of the relative frequencies
    mainPanel(
      h4(textOutput("plot_sec_title")),
      plotOutput("timeline_plot", width = "150%"),
      h4(textOutput("balances_sec_title")),
      tableOutput("balances")
    )
  )

  
  

# Define server logic required to draw the plot
server <- function(input, output) {
      
  future_value <- function(amount, r, yrs){
    result <- amount*(1+r)^yrs
    return(result)
  }
  annuity <- function(contrib, r, yrs){
    result <- contrib*(((1+r)^yrs - 1)/r)
    return(result)
  }
  growing_annuity <- function(contrib, r, g, yrs){
    result <- contrib*(((1+r)^yrs - (1+g)^yrs)/(r-g))
    return(result)
  }
  
  # Fill in the spot we created for a plot
  
  output$timeline_plot <- renderPlot({
     yr <- 0:input$yr
     nocontrib <- rep(0,input$yr+1)
     fixedcontrib <- rep(0,input$yr+1)
     growingcontrib <- rep(0,input$yr+1)
     #calculating no contribution model
     for (i in 0:input$yr){
       nocontrib[i+1] <- future_value(amount = input$initial, r = input$r/100, yrs = i)
     }
     #calculating fixed contribution model
     for (i in 0:input$yr){
       fixedcontrib[i+1] <- future_value(amount = input$initial, r = input$r/100, yrs = i) + 
         annuity(contrib = input$acontrib, r = input$r/100, yrs = i)
     }
     #calculating growing contribution model
     for (i in 0:input$yr){
       growingcontrib[i+1] <- future_value(amount = input$initial, r = input$r/100, yrs = i) + 
         growing_annuity(contrib =  input$acontrib, r = input$r/100, g = input$g/100, yrs = i)
     }
   
      untidy <- data.frame(yr = yr, nocontrib = nocontrib, fixedcontrib = fixedcontrib, growingcontrib = growingcontrib)
      modalities <- melt(untidy,"yr")
      options(scipen=10000)
      original <- ggplot(data = modalities, aes(x=yr,y=value,color=variable)) +
      geom_line(size = 2) +
      geom_point(shape = 16, size = 6)+
      ggtitle("Three Modes of Investing")
      if (input$facet == "No") {
        print(original)
      }
      else{
        facetted <- original + facet_wrap(~variable)
        print(facetted)
      }
        
  })
  output$plot_sec_title <- renderText("Timelines")
  output$balances_sec_title <- renderText("Balances")
  output$balances <- renderTable({
    yr <- 0:10
    nocontrib <- rep(0,11)
    fixedcontrib <- rep(0,11)
    growingcontrib <- rep(0,11)
    #calculating no contribution model
    for (i in 0:10){
      nocontrib[i+1] <- future_value(amount = input$initial, r = input$r/100, yrs = i)
    }
    #calculating fixed contribution model
    for (i in 0:10){
      fixedcontrib[i+1] <- future_value(amount = input$initial, r = input$r/100, yrs = i) + 
        annuity(contrib = input$acontrib, r = input$r/100, yrs = i)
    }
    #calculating growing contribution model
    for (i in 0:10){
      growingcontrib[i+1] <- future_value(amount = input$initial, r = input$r/100, yrs = i) + 
        growing_annuity(contrib =  input$acontrib, r = input$r/100, g = input$g/100, yrs = i)
    }
    modalities <- data.frame(yr = yr, nocontrib = nocontrib, fixedcontrib = fixedcontrib, growingcontrib = growingcontrib)
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
