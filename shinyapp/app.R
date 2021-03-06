library(shiny)
library(shinythemes)
library(tidyverse)
library(gridExtra)
library(rsconnect)

#put protocol 1 at top then options
#then do the sme for protoco l2
#change titles and make them bigger
#can add other pages to show relationships I guess. Would make sense to allow eda plots on another page.
#fancy plots on a 2nd page


#load("shinydata.Rdata")
load("shinydata.RData")
data_2012 <- data_2012 %>%
  mutate(wild_logged = log(wildAbF + 1)) %>%
  mutate(social_logged = log(socialRichF + 1))

standardised_clust_comps <- standardised_clust_comps %>%
  as_tibble() %>%
  slice(-(5)) %>%
  mutate(distance_methods = c("Euclidean", "Maximum", "Manhattan", "Canberra")) %>%
  select(distance_methods,average, single, complete, ward)
clust_comps <- clust_comps %>%
  as_tibble() %>%
  slice(-(5)) %>%
  mutate(distance_methods = c("Euclidean", "Maximum", "Manhattan", "Canberra")) %>%
  select(distance_methods,average, single, complete, ward)

#two stage process for thinner option.
#either thinner button disappear or input changes?
#compare two options - two dropdown list selection tasks
#Transpose into a /100 value but reread for defn of abundance and richness.
#background rectangle - to display typical ranges and display this dataset on top
#Flowchart for how a user can go through the app.
#barchart builder upper

ui <-  fluidPage(
        theme=shinytheme("simplex"),
        #themeSelector(),
  
  navbarPage(title = "Wild Pollinators Application", 
             tabPanel("Protocol Summaries",
                      h4("This page demonstrates the effects that differing pesticide protocols have on the bee abundance and richness observations for wild pollinators"),
                 fluidRow(
                   
                   column(2,
                          selectInput("protocol_logged", "Unlog Bee values?",
                                      c("No" = "no",
                                        "Yes" = "yes")),
                          selectInput("fungicide", "Fungicide Level Protocol 1:",
                                      c("Low" = "low",
                                        "High" = "high")),
                          selectInput("insecticide", "Insecticide Level Protocol 1:",
                                      c("Low" = "low",
                                        "High" = "high")),
                          selectInput("thinner", "Thinner Level Protocol 1:",
                                      c("Low" = "low",
                                        "High" = "high")),
                          selectInput("fungicide1", "Fungicide Level Protocol 2:",
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
                                h4("This page demonstrates the relationships between considered bee variables and both potential confounders and pesticides"), 
                       fluidRow(
                         
                         column(4,
                                selectInput("relationship_logged", "Log Transform Bee Variables?",
                                            c("Yes" = "yes",
                                              "No" = "no")),
                                selectInput("relationship_y", "Variable 1:",
                                            c("Wild Bee Abundance" = "wildAbF",
                                              "Social Bee Richness" = "socialRichF",
                                              "Temperature" = "temp",
                                              "Percentage Natural Surroundings" = "X2000nat",
                                              "Fungicide PUI before bloom" = "eiqB11F.pre",
                                              "Fungicide PUI during bloom" = "eiqB11F.blm",
                                              "Fungicide PUI after bloom" = "eiqB11F.pos",
                                              "Insecticide PUI before bloom" = "eiqB11I.pre",
                                              "Insecticide PUI during bloom" = "eiqB11I.pre",
                                              "Insecticide PUI after bloom" = "eiqB11I.pos"
                                              )),
                                selectInput("relationship_x", "Variable 2:",
                                            c("Wild Bee Abundance" = "wildAbF",
                                              "Social Bee Richness" = "socialRichF",
                                              "Temperature" = "temp",
                                              "Percentage Natural Surroundings" = "X2000nat",
                                              "Fungicide PUI before bloom" = "eiqB11F.pre",
                                              "Fungicide PUI during bloom" = "eiqB11F.blm",
                                              "Fungicide PUI after bloom" = "eiqB11F.pos",
                                              "Insecticide PUI before bloom" = "eiqB11I.pre",
                                              "Insecticide PUI during bloom" = "eiqB11I.pre",
                                              "Insecticide PUI after bloom" = "eiqB11I.pos"
                                            )),
                                
                                actionButton("refresh_relationships","Refresh Plot:")
                                
                         ),
                         
                         column(8, plotOutput("eda_plot"))
                         
                       )
            ), tabPanel("Distance Metric and Linkage Function selection",
                        h4("This page demonstrates the performance of different linkage functions and distance metrics on the orchard data, the closer the number is to 1 the better the performance of that particular combination."),
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
                     h4("This page shows the outputs for each clustering method considered"),
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
            
            
            # tabPanel("Custom",
            # 
            # 
            #          fluidRow(
            #            column(4,
            #                   textInput("pre_f", label = h3("Pre-Bloom Fungicide:"), value = "0"),
            #                   textInput("dur_f", label = h3("During-Bloom Fungicide:"), value = "0"),
            #                   textInput("post_f", label = h3("During-Bloom Fungicide:"), value = "0"),
            #                   textInput("pre_i", label = h3("Pre-Bloom Insecticide:"), value = "0"),
            #                   textInput("dur_i", label = h3("During-Bloom Insecticide:"), value = "0"),
            #                   textInput("post_i", label = h3("During-Bloom Insecticide:"), value = "0"),
            #                   textInput("dur_t", label = h3("During-Bloom Thinner:"), value = "0"),
            #                   textInput("post_t", label = h3("During-Bloom Thinner:"), value = "0"),
            #                   actionButton("refresh_knn","Refresh Plot")
            #                   ,
            # 
            #            # area for displaying the gantt diagram
            #            column(8, plotOutput("knn_output"))))
            # 
            # 
            # ),

tabPanel("About Page",
         h4("This Project was undertaken by Stephen Brownsey and supervised by Julia Brettschneider from the University of Warwick."),
         h4("Each tab of the application demonstrates various aspects of the analysis undertaken, with interactive plots and tables to showcase the findings."),
         h4(HTML(paste("All code used in the project, as well as for this application, can be found on the project's GitHub page:", a(href="https://github.com/Brownsey/wild_pollinators", "link"), "."))),
         h4(HTML(paste("The original paper on which the analysis is based on can be found at:", a(href="https://royalsocietypublishing.org/doi/pdf/10.1098/rspb.2015.0299", "link"), ".")))
        

)))

server <- function(input, output) {
  
  plot_abundance <- eventReactive(c(input$refresh),{
    fungicide <- input$fungicide
    insecticide <- input$insecticide
    thinner <- input$thinner
    
    fungicide1 <- input$fungicide1
    insecticide1 <- input$insecticide1
    thinner1 <- input$thinner1

    
    if(input$protocol_logged == "yes"){
      wild <- "unlogged_ab"
      social <- "unlogged_rich"
    }else{
      wild <- "mean_wild_ab"
      social <- "mean_social_rich"
    }
    
    #Need to take account of the two options which are not classified:
    #001 -> 000
    #111 -> 110
    if(fungicide == "low" && insecticide == "low" && thinner == "high"){
      thinner = "low"
    }
    
    if(fungicide1 == "low" && insecticide1 == "low" && thinner1 == "high"){
      thinner1 = "low"
    }
    
    if(fungicide == "high" && insecticide == "high" && thinner == "high"){
      thinner = "low"
    }
    
    if(fungicide1 == "low" && insecticide1 == "low" && thinner1 == "high"){
      thinner1 = "low"
    }

    protocol_summary %>%
      filter(fung_level == fungicide & insect_level  == insecticide & thinner_level == thinner|
               fung_level == fungicide1 & insect_level  == insecticide1 & thinner_level == thinner1) %>%
      mutate(id = if_else(fung_level == fungicide1 & insect_level == insecticide1 & thinner_level == thinner1, 2, 1)) %>%
      mutate(id = factor(id)) %>%
      ggplot(aes_string(wild, x = "id")) +
      geom_bar(stat="identity", position = "dodge", aes(fill = id))+
      theme_bw() +
      labs(title = "Abundance Comparison of Orchard Protocols",
           x = "Protocol Number", y = "Bee Abundance")+
      theme(legend.position = "none")

  })
  #Probably just show one for this clustering options, need to edit to include the "Low/High Options in select list"
  plot_richness <- eventReactive(c(input$refresh),{
    fungicide <- input$fungicide
    insecticide <- input$insecticide
    thinner <- input$thinner
    
    fungicide1 <- input$fungicide1
    insecticide1 <- input$insecticide1
    thinner1 <- input$thinner1
    
    
    if(input$protocol_logged == "yes"){
      wild <- "unlogged_ab"
      social <- "unlogged_rich"
    }else{
      wild <- "mean_wild_ab"
      social <- "mean_social_rich"
    }
    
    #Need to take account of the two options which are not classified:
    #001 -> 000
    #111 -> 110
    if(fungicide == "low" && insecticide == "low" && thinner == "high"){
      thinner = "low"
    }
    
    if(fungicide1 == "low" && insecticide1 == "low" && thinner1 == "high"){
      thinner1 = "low"
    }
    
    if(fungicide == "high" && insecticide == "high" && thinner == "high"){
      thinner = "low"
    }
    
    if(fungicide1 == "low" && insecticide1 == "low" && thinner1 == "high"){
      thinner1 = "low"
    }
    
    protocol_summary %>%
      filter(fung_level == fungicide & insect_level  == insecticide & thinner_level == thinner|
               fung_level == fungicide1 & insect_level  == insecticide1 & thinner_level == thinner1) %>%
      mutate(id = if_else(fung_level == fungicide1 & insect_level == insecticide1 & thinner_level == thinner1, 2, 1)) %>%
      mutate(id = factor(id)) %>%
      ggplot(aes_string(social, x = "id")) +
      geom_bar(stat="identity", position = "dodge", aes(fill = id))+
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
    d1 <- agglom_summary_standardised  %>%
      mutate(cluster = as.integer(cluster))
  }else if (clustering == "cv_kmeans" && standardised_cluster == "yes"){
    d1 <- kmeans_summary_standardised %>%
      mutate(cluster = as.integer(cluster))
  }else if (clustering == "max_aggl" && standardised_cluster == "no"){
    d1 <- maximum_final 
  }else if (clustering == "dec_aggl" && standardised_cluster == "no"){
    d1 <- agglom_summary  %>%
      mutate(cluster = as.integer(cluster))
  }else if (clustering == "cv_kmeans" && standardised_cluster == "no"){
    d1 <- kmeans_summary %>%
      mutate(cluster = as.integer(cluster))
  }else{
    d1 <- euclidean_final
  }
  d1
  
})
  
  output$cluster_df <- renderTable({
  cluster_df()
  })
  
  
  
  metric_df <- eventReactive(c(input$refresh_metric_output),{
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
  
  
  
  
  eda_plot <- eventReactive(c(input$refresh_relationships),{
    x <- input$relationship_x
    relationship_logged <- input$relationship_logged
    #Transforming if x is a bee variable and logged
    if(x == "wildAbF" && relationship_logged == "yes"){
      x = "wild_logged"
    }else if(x == "socialRichF" && relationship_logged == "yes"){
      x = "social_logged"
    }
    
    y <- input$relationship_y
    if(y == "wildAbF" && relationship_logged == "yes"){
      y = "wild_logged"
    }else if(y == "socialRichF" && relationship_logged == "yes"){
      y = "social_logged"
    }
    
    data_2012 %>%
      ggplot(aes_string(x = x, y = y)) + 
      geom_point() + 
      geom_smooth(se = FALSE) +
      geom_smooth(method = "lm", se = FALSE, colour = "red") +
      theme_bw()
  })
  
  
  
  output$eda_plot <- renderPlot(({
    eda_plot()
  }))
  
}


# knn_plot<- eventReactive(c(input$refresh_knn),{
# #Add in inputs into a "test" one row test dataset?
# #Commpare with clusters of one of the clusters with nearest 1 cluster
# #Probably would work but in theory this seems suboptimal
# #Maybe go back a set to 
#  knn_plot <- tibble(eiqB11F.pre = input$pre_f,  eiqB11I.pre= input$pref_i,  eiqB11F.blm = input$dur_f,  eiqB11I.blm = input$dur_i,
#   eiqB11T.blm = input$dur_t,  eiqB11F.pos = input$post_f,  eiqB11I.pos = input$post_i,  eiqB11T.pos = input$post_t)
#  
#  knn <- knn(agglom_summary %>% select(c(6:13)), knn_df, k =1, labels = agglom_summary %>% select(c(1)))
#  ggplot()
# })
# 
# output$knn_output <- renderPlot(({
#   knn_plot()
# }))


shinyApp(ui, server)