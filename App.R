library(shiny)
library(ggplot2)

dt <- data.frame(x=runif(1000), y=runif(1000))



ui <-  fluidPage(titlePanel("Brownsey's Gantt Creator"),
  
  fluidRow(
    
    
    column(2,
           selectInput("pesticide", "Pesticide Level:",
                       c("Low" = "low",
                         "High" = "high")),
    selectInput("insecticide", "Insecticide Level:",
                c("Low" = "low",
                  "High" = "high")),
  selectInput("thinner", "Thinner Level:",
              c("Low" = "low",
                "High" = "high")),
           actionButton("refresh","Refresh Plot:")
           
    ),
    
    # area for displaying the gantt diagram
    column(10, plotOutput("plot")
           
    )
    
  )
)

server <- function(input, output) {

  
  
  pp <- eventReactive(c(input$refresh),{
    ggplot(dt, aes(x,y)) + 
      geom_point()
  })
  
  output$plot <- renderPlot({
      pp()
    })
  }
  
  


shinyApp(ui, server)
