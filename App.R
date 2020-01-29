library(shiny)
library(tidyverse)

data <- read_csv("agglom_summary.csv")



ui <-  fluidPage(titlePanel("Qualitive Bees Shiny App"),
  
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
  
  # pp <- eventReactive(c(input$refresh),{
  #   pesticide <- input$pesticide
  #   insecticide <- input$insecticide
  #   thinner <- input$thinner
  #   
  #   subset_data <- data %>%
  #     filter(pest_rating == pesticide) %>%
  #     filter(insect_rating == insecticide) #%>%
  #     #filter(thinner_rating == thinner)
  #   
  #   
  #   ggplot(subset_data, aes(x,y)) + 
  #     geom_bar()
  # })
  pp <- eventReactive(c(input$refresh),{
    ggplot(mtcars) +
    geom_point(aes(x = wt, y = mpg))
    
  })
  
  output$plot <- renderPlot({
      pp()
    })
  }
  
  


shinyApp(ui, server)
