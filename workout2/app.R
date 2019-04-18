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
library('reshape2')
# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Growth over time for investing methods"),
   
   # Sidebar with a slider input for number of bins 
   fluidRow(
     column(4,
            sliderInput("initial",
                        "Initial Amount",
                        min = 0,
                        max = 100000,
                        value = 1000,
                        step = 500,
                        pre = "$"),
            sliderInput("annual",
                        label = "Annual Contribution",
                        min = 0,
                        max = 50000,
                        value = 2000,
                        step = 500,
                        pre = "$")
            
        ),
     column(4,
            sliderInput("rrate",
                        label = "Return Rate (in %)",
                        min = 0,
                        max = 20,
                        value = 5,
                        step = .1),
            sliderInput("grate",
                        label = "Growth Rate (in %)",
                        min = 0,
                        max = 20,
                        value = 2,
                        step = .1)
        ),
     column(4,
            sliderInput("years",
                        label = "Years",
                        min = 0,
                        max = 50,
                        value = 20,
                        step = 1),
            selectInput('facet', 'Facet?', list("No", "Yes"))
     )
     
   ),
   
   hr(),
   
   h4("Timelines"),
   
   plotOutput("modalities_graph"),
   
   h4("Balances"),
   
   verbatimTextOutput("table")
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  future_value <- function(amount, rate, years) {
    
    fv <- amount * (1+rate)^years
    return(fv)
  }
  
  annuity <- function(contrib, rate, years) {
    
    fva <- contrib * ( ((1+rate)^years - 1)/rate)
    return(fva)
  }
  
  growing_annuity <- function(contrib, rate, growth, years) {
    
    fvga <- contrib * (((1+rate)^years - (1+growth)^years)/(rate - growth))
    return(fvga)
  }
  
  
  graphs <- reactive({

    year <- numeric(input$years + 1)
    no_contrib <- numeric(input$years + 1)
    fixed_contrib <- numeric(input$years + 1)
    growing_contrib <- numeric(input$years + 1)
    for (i in 1:(input$years + 1)) {
      year[i] <- i-1
      no_contrib[i] <- future_value(input$initial, input$rrate * .01, i-1)
      fixed_contrib[i] <- future_value(input$initial, input$rrate * .01, i-1) + annuity(input$annual, input$rrate * .01, i-1)
      growing_contrib[i] <- future_value(input$initial, input$rrate * .01, i-1) + growing_annuity(input$annual, input$rrate * .01, input$grate * .01, i-1)
    }
    modalities <- data.frame(year, no_contrib, fixed_contrib, growing_contrib)
    modalities

  })
   
  output$modalities_graph <- renderPlot({
    # generate bins based on input$bins from ui.
    
    melted = melt(graphs(), id.vars="year")
    if (input$facet == "No") {
      ggplot(data=melted, aes(x=year, y=value, group=variable, colour=variable)) + geom_point() + geom_line() + ggtitle("Three modes of investing") + theme_bw() + ylab("balance") + labs(color = "modality")
    }
    else {
      ggplot(data=melted, aes(x=year, y=value, group=variable, colour=variable)) + geom_area(aes(fill=variable), alpha=0.5) + geom_point() + geom_line() + facet_grid(~variable) + ggtitle("Three modes of investing") + theme_bw() + ylab("balance")
    }
    
    
  })
  
  output$table <- renderPrint({ graphs() })
}

# Run the application 
shinyApp(ui = ui, server = server)

