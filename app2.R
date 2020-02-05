library(shiny)
library(tidyverse)

data <- read_csv("agglom_summary.csv")
dummy <- tibble(bee_abundance = c(40,50,60,70), bee_rich = c(1,2,3,4),
                pest = c("low","low", "high", "high"), insect =  c("low","high","low","high"), id = c(1,1,1,1))
basic <- tibble(id = c(1), bee_abundance = c(55), bee_rich = c(2.5) )

d <- reactiveValues(data = basic)

#two stage process for thinner option.
#either thinner button disappear or input changes?
#compare two options - two dropdown list selection tasks
#Transpose into a /100 value but reread for defn of abundance and richness.
#background rectangle - to display typical ranges and display this dataset on top
#Flowchart for how a user can go through the app.
#barchart builder upper

ui <-  fluidPage(titlePanel("Qualitive Bees Shiny App"),
                 
                 fluidRow(
                   
                   
                   column(2,
                          selectInput("pesticide", "Pesticide Level Protocol 1:",
                                      c("Low" = "low",
                                        "High" = "high")),
                          selectInput("insecticide", "Insecticide Level Protocol 1:",
                                      c("Low" = "low",
                                        "High" = "high")),
                          selectInput("thinner", "Thinner Level Protocol 1:",
                                      c("Low" = "low",
                                        "High" = "high")),
                          selectInput("pesticide1", "Pesticide Level Protocol 2:",
                                      c("Low" = "low",
                                        "High" = "high")),
                          selectInput("insecticide1", "Insecticide Level Protocol 2::",
                                      c("Low" = "low",
                                        "High" = "high")),
                          selectInput("thinner1", "Thinner Level Protocol 2::",
                                      c("Low" = "low",
                                        "High" = "high")),
                          actionButton("refresh","Refresh Plot:")
                          
                   ),
                   
                   # area for displaying the gantt diagram
                   column(5, plotOutput("plot"),
                          column(5, plotOutput("plot1"))
                          
                   )
                   
                 )
)

server <- function(input, output) {
  
  plot_abundance <- eventReactive(c(input$refresh),{
    pesticide <- input$pesticide
    insecticide <- input$insecticide
    thinner <- input$thinner
    
    pesticide1 <- input$pesticide1
    insecticide1 <- input$insecticide1
    thinner1 <- input$thinner1
    
    
    dummy %>%
      filter(pest == pesticide & insect  == insecticide | pest == pesticide1 & insect  == insecticide1) %>%
      mutate(id = if_else(pest == pesticide1 & insect == insecticide1, 2, 1)) %>%
      ggplot(aes(bee_abundance, x = factor(id))) +
      geom_bar(stat="identity", position = "dodge", aes(fill = id))

  })
  
  plot_richness <- eventReactive(c(input$refresh),{
    pesticide <- input$pesticide
    insecticide <- input$insecticide
    thinner <- input$thinner
    
    pesticide1 <- input$pesticide1
    insecticide1 <- input$insecticide1
    thinner1 <- input$thinner1
    
    
    dummy %>%
      filter(pest == pesticide & insect  == insecticide | pest == pesticide1 & insect  == insecticide1) %>%
      mutate(id = if_else(pest == pesticide1, 2, 1)) %>%
      ggplot(aes(bee_abundance, x = factor(id))) +
      geom_bar(stat="identity", position = "dodge", aes(fill = id))
    
  })
  
  output$plot <- renderPlot({
    plot_abundance()
  })
  
  
  output$plot1 <- renderPlot({
    plot_richness()
  })
}


shinyApp(ui, server)