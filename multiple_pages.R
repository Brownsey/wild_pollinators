library(shiny)
library(tidyverse)
library(gridExtra)

#put protocol 1 at top then options
#then do the sme for protoco l2
#change titles and make them bigger
#can add other pages to show relationships I guess. Would make sense to allow eda plots on another page.
#fancy plots on a 2nd page
data <- read_csv("agglom_summary.csv")
dummy <- tibble(bee_abundance = c(40,50,60,70), bee_rich = c(1,2,3,4),
                pest = c("low","low", "high", "high"), insect =  c("low","high","low","high"), id = c(1,1,1,1))
basic <- tibble(id = c(1), bee_abundance = c(55), bee_rich = c(2.5) )


load("ShinyData.Rdata")

d <- reactiveValues(data = basic)


#two stage process for thinner option.
#either thinner button disappear or input changes?
#compare two options - two dropdown list selection tasks
#Transpose into a /100 value but reread for defn of abundance and richness.
#background rectangle - to display typical ranges and display this dataset on top
#Flowchart for how a user can go through the app.
#barchart builder upper

ui <-  navbarPage(title = "Wild Pollinators Application", 
             tabPanel("Protocol Summaries",
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
                          selectInput("insecticide1", "Insecticide Level Protocol 2:",
                                      c("Low" = "low",
                                        "High" = "high")),
                          selectInput("thinner1", "Thinner Level Protocol 2:",
                                      c("Low" = "low",
                                        "High" = "high")),
                          actionButton("refresh","Refresh Plot:")
                          
                   ),
                   
                   # area for displaying the gantt diagram
                   column(10, plotOutput("plot")
                          
                          
                   )
                   
                 )
            ),
            navbarMenu("Data Analysis",
                       tabPanel("Relationships",
                               
                       fluidRow(
                         
                         column(2,
                                selectInput("plot_type", "Plot type :",
                                            c("Low" = "low",
                                              "High" = "high")),
                                actionButton("refresh","Refresh Plot:")
                                
                         ),
                         
                         # area for displaying the gantt diagram
                         column(10, plotOutput("eda_plot"))
                         
                       )
            ), tabPanel("Distance Metric and Linkage Function selection",
                        fluidRow(
                          
                          column(2,
                                 selectInput("standardised", "Data Standardised?",
                                             c("Yes" = "yes",
                                               "No" = "no")),
                                 
                                 actionButton("refresh_metric_output","Refresh Plot")       
                          ),
                          
                          
                          # area for displaying the gantt diagram
                          column(10, tableOutput("metric_df"))
                          )),
            
            tabPanel("Clustering",
                      fluidRow(
                       column(3,
                              selectInput("clustering", "Select clustering method",
                                          c("Euclidean Agglomerative" = "euc_aggl",
                                            "Maximum Agglomerative" = "max_aggl",
                                            "Decision Step Agglomerative" = "dec_aggl",
                                            "Cross-Validated Kmeans" = "cv_kmeans")),
                              selectInput("standardised_cluster", "Data Standardised?",
                                          c("Yes" = "yes",
                                            "No" = "no")),
                              actionButton("refresh_cluster_output","Refresh Plot")
                       ),
                       
                       # area for displaying the gantt diagram
                       column(9, tableOutput("cluster_df")
                              
                       )
                       
                     
            ))),

tabPanel("About Page",
         h4("This Project was undertaken by Stephen Brownsey and Supervised by Julia Brettschneider from the University of Warwick"),
         h4("Each tab of the application demonstrates various aspects of the analysis undertaken"),
         h4(HTML(paste("A full pdf guide on how to use the app can be found at:", a(href="https://github.com/Brownsey/wild_pollinators/INSERTLINK", "link"), "."))),
         h4(HTML(paste("Thanks for using the application. Reference code available at the following", a(href="https://github.com/Brownsey/wild_pollinators", "link"), "."))),
         h4(HTML(paste("The original paper on which the analysis is based on can be found at:", a(href="https://royalsocietypublishing.org/doi/pdf/10.1098/rspb.2015.0299", "link"), ".")))
         

))

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
      geom_bar(stat="identity", position = "dodge", aes(fill = id))+
      theme_bw() +
      labs(title = "Abundance Comparison of Orchard Protocols",
           x = "Protocol Number", y = "Bee Abundance")+
      theme(legend.position = "none")
    
  })
  #Probably just show one for this clustering options, need to edit to include the "Low/High Options in select list"
  plot_richness <- eventReactive(c(input$refresh),{
    pesticide <- input$pesticide
    insecticide <- input$insecticide
    thinner <- input$thinner
    
    pesticide1 <- input$pesticide1
    insecticide1 <- input$insecticide1
    thinner1 <- input$thinner1
    
    dummy %>%
      filter(pest == pesticide & insect  == insecticide | pest == pesticide1 & insect  == insecticide1) %>%
      mutate(id = if_else(pest == pesticide1 & insect == insecticide1, 2, 1)) %>%
      ggplot(aes(bee_rich, x = factor(id))) +
      geom_bar(stat="identity", position = "dodge", aes(fill = id)) +
      theme_bw() +
      labs(title = "Richness Comparison of Orchard Protocols",
           x = "Protocol Number", y = "Bee Richness")+
      theme(legend.position = "none")
    
  })
  
  output$plot <- renderPlot({
    grid.arrange(plot_abundance(), plot_richness(), nrow = 1)
  })
  
  
  cluster_df <- eventReactive(c(input$refresh_cluster_output),{
  d1 <- reactiveValues(data = agglom_summary)  
  standardised_cluster <- input$standardised_cluster
  clustering <- input$clustering
  
  if(clustering == "euc_aggl" && standardised_cluster == "yes"){
    d1 <- euclidean_final_standardised
  }else if (clustering == "max_aggl" && standardised_cluster == "yes"){
    d1 <- maximum_final_standardised
  }else if (clustering == "dec_aggl" && standardised_cluster == "yes"){
    d1 <- agglom_summary_standardised
  }else if (clustering == "cv_kmeans" && standardised_cluster == "yes"){
    d1 <- kmeans_summary_standardised
  }else if (clustering == "max_aggl" && standardised_cluster == "no"){
    d1 <- maximum_final
  }else if (clustering == "dec_aggl" && standardised_cluster == "no"){
    d1 <- agglom_summary
  }else if (clustering == "cv_kmeans" && standardised_cluster == "no"){
    d1 <- kmeans_summary
  }else{
    d1 <- euclidean_final
  }
  d1
  
})
  
  output$cluster_df <- renderTable({
  cluster_df()
  })
  
  
  
  metric_df <- eventReactive(c(input$refresh_metric_output),{#could add click button for this too
    output <- reactiveValues(data = standardised_clust_comps)
    if(input$standardised == "yes"){
      output <- standardised_clust_comps
    }else{
      output <- clust_comps
    }
    output

  })
  

  output$metric_df <- renderTable({
    metric_df()
  })
  
  
  
  output$eda_plot <- renderPlot(({
    
  }))
  
}


shinyApp(ui, server)