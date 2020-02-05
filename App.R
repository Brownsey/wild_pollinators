library(shiny)
library(tidyverse)

data <- read_csv("agglom_summary.csv")
dummy <- tibble(bee_abundance = c(40,50,60,70), bee_rich = c(1,2,3,4),
                pest = c("low","low", "high", "high"), insect =  c("low","high","low","high"))
basic <- tibble(id = c(1), bee_abundance = c(55), bee_rich = c(2.5) )



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
  
  
  #observeEvent()

  pp <- eventReactive(c(input$refresh),{
    # ggplot(mtcars) +
    # geom_point(aes(x = wt, y = mpg))
    
    
      pesticide <- input$pesticide
      insecticide <- input$insecticide
      thinner <- input$thinner
      
    temp <- tibble(id = (nrow(basic) + 1), 
                   bee_abundance = dummy %>% 
                     filter(pest == pesticide) %>%
                     filter(insect == insecticide) %>%
                     select(bee_abundance) %>%
                     as_vector(), 
                   bee_rich = dummy %>% 
                     filter(pest == pesticide) %>%
                     filter(insect == insecticide) %>%
                     select(bee_rich) %>%
                     as_vector())
    basic <- basic %>%
      bind_rows(temp)
    
    
    # basic <- basic %>%
    #   bind_rows(id = max(basic$id) + 1, 
    #             bee_abundance = dummy %>% 
    #               filter(pest == input$pesticide) %>%
    #               filter(insect == input$insecticide))
      basic %>%
      ggplot(aes(bee_abundance, x = factor(id))) +
      geom_bar(stat="identity", position = "dodge", aes(fill = id))
    
  })
  
  output$plot <- renderPlot({
      pp()
    })
  }
  
  


shinyApp(ui, server)
