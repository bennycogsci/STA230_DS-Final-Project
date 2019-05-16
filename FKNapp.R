#UI
library('dplyr')
library(shiny)
library("ggplot2")
library("tidyr")
library("maps")
library("readr")
library("leaflet")
library("plotly")
library(rgdal)    # Updates deprecated maptools functions
library(maptools) # creates maps and work with spatial files
library(broom)    # assists with tidy data
library("cluster")
library("factoextra") 
library('DT')

# Choices for drop-downs
countryvar<-c("All",as.character(unique(centroids$country)))
yearvar<-c(1980:2016)
incgrp<-c(unique(dat2$incomegroup))
#For now only adding larger variables A) B); starred equals year conditional panel
# "World Bank Income Group" = "incomegroup",
varvar<-c("Gini Coefficient" = "gini",
          "Food Loss" = "GENfoodloss",
          "End-User Waste" = "GENendUserWaste",
          "Sustainable Agriculture" = "GENsustainagri",
          "General Water Use" = "GENwater",
          "Water Scarcity" = "waterscarcity",
          "Fishery Substainability" = "fisherysust",
          "General Land Use" = "GENlanduse",
          "Land Ownership" = "landownership",
          "General Air Quality" = "GENAirGHGemissions",
          "Efforts to Mitigate Climate Change" = "climchngmitigation",
          "Nutrition Challenges" = "GENnutchalleng",
          "Life Quality" = "lifequal")
methvar<-c("K-Means"="kmeans",
           "PAM",
           "FANNY"= "fanny")
cluvar<-c(2:10)
ui<- fluidPage(
  navbarPage("Food Sustainability and World Income Inequality Data Explorer", id="nav",
             tabPanel("Chloropleth Map",
                      sidebarLayout(
                        sidebarPanel(
                          width= 2,
                          h3("Chloropleth Map"),
                          selectInput("country", "View Country", countryvar),
                          selectInput("varvar", "Color World Map By...", varvar),
                          conditionalPanel(condition="input.varvar == 'gini'",
                                           p("The Gini Coefficient is a measure of income inequality, scored by an expert at the United Nations for the World Income Inequality Database."),
                                           selectInput("year", "Year of Interest", yearvar, selected = 2000)),
                          conditionalPanel(condition="input.varvar == 'GENfoodloss'",
                                           p("Food Loss is a measure from the Food Sustainability Index developed by the Economist Intelligence Unit with the Barilla Center for Food and Nutrition. Higher values indicate more Food Loss.")),
                          conditionalPanel(condition="input.varvar == 'GENendUserWaste'",
                                           p("End-User Waste is a measure from the Food Sustainability Index developed by the Economist Intelligence Unit with the Barilla Center for Food and Nutrition. Higher values indicate more End-User Waste.")),
                          conditionalPanel(condition="input.varvar == 'GENsustainagri'",
                                           p("Sustainable Agriculture is a measure from the Food Sustainability Index developed by the Economist Intelligence Unit with the Barilla Center for Food and Nutrition. Higher values indicate more Sustainable Agricultural practices.")),
                          conditionalPanel(condition="input.varvar == 'GENwater'",
                                           p("General Water Use is a measure from the Food Sustainability Index developed by the Economist Intelligence Unit with the Barilla Center for Food and Nutrition. Higher values indicate more sustainable Water Usage.")),
                          conditionalPanel(condition="input.varvar == 'waterscarcity'",
                                           p("Water Scarcity is a measure from the Food Sustainability Index developed by the Economist Intelligence Unit with the Barilla Center for Food and Nutrition. Higher values indicate more Water Scarcity.")),
                          conditionalPanel(condition="input.varvar == 'fisherysust'",
                                           p("Fishery Sustainability is a measure from the Food Sustainability Index developed by the Economist Intelligence Unit with the Barilla Center for Food and Nutrition. Higher values indicate more sustainable fisheries.")),
                          conditionalPanel(condition="input.varvar == 'GENlanduse'",
                                           p("General Land Use is a measure from the Food Sustainability Index developed by the Economist Intelligence Unit with the Barilla Center for Food and Nutrition. Higher values indicate more sustainable Land Use.")),
                          conditionalPanel(condition="input.varvar == 'landownership'",
                                           p("Land Ownership is a measure from the Food Sustainability Index developed by the Economist Intelligence Unit with the Barilla Center for Food and Nutrition. Higher values indicate more Land Ownership.")),
                          conditionalPanel(condition="input.varvar == 'GENAirGHGemissions'",
                                           p("General Air Quality is a measure from the Food Sustainability Index developed by the Economist Intelligence Unit with the Barilla Center for Food and Nutrition. Higher values indicate more quality Air (less emissions of many sorts).")),
                          conditionalPanel(condition="input.varvar == 'climchngmitigation'",
                                           p("Climate Change Mitigation is a measure from the Food Sustainability Index developed by the Economist Intelligence Unit with the Barilla Center for Food and Nutrition. Higher values indicate more policy and action to mitigate Climate Change.")),
                          conditionalPanel(condition="input.varvar == 'GENnutchalleng'",
                                           p("Nutrition Challenges is a measure from the Food Sustainability Index developed by the Economist Intelligence Unit with the Barilla Center for Food and Nutrition. Higher values indicate more less Nutritional Challenges.")),
                          conditionalPanel(condition="input.varvar == 'lifequal'",
                                           p("Life Quality is a measure from the Food Sustainability Index developed by the Economist Intelligence Unit with the Barilla Center for Food and Nutrition. Life quality captures circumstances such as infant mortality rates, life expectancies, general diet, and nutrition. Higher values indicate more higher quality of life."))
                          ), #End SidebarPanel
                        mainPanel(
                          leafletOutput("map", height = 900, width = 1500)), #End MainPanel
                        position="left",
                        fluid=TRUE
                        ) #End of SidebarLayout
                      ), #End of TabPanel
             #Start of new tab
             tabPanel("Clustering Explorer",
                      sidebarLayout(
                        sidebarPanel(
                          width= 4,
                          h3("Clustering Explorer"),
                          selectInput("method", "Clustering Method", methvar),
                          conditionalPanel(condition="input.method == 'kmeans'",
                                           p("K-means is one of the oldest methods of cluster analysis. The user must specify the number of clusters or 'k'. The clustering process begins by choosing k observations which will serve as centers for the clusters after which, distances of the observations from each of the cluster centers are calculated. Observations are put into the cluster whose center they are closest to. This process is repeated with recalculating the cluster center and moving around the other observations till no observations switches clusters.")),
                          conditionalPanel(condition="input.method == 'PAM'",
                                           p("A modern alternative to clustering with k-means, is clustering with PAM. 'PAM' is an acronym for Partitioning Around Medoids. Medoids are observations within a cluster for which the sum of distances between the medoid and all other points in the cluster is at a minimum. In this method, cluster centers are these medoids, which again are observations. This differs from k-means which uses distance-minimized cluster centers that are not necessarily observations. PAM requires the user to specify number of clusters.")),
                          conditionalPanel(condition="input.method == 'fanny'",
                                           p("FANNY refers to a soft-clustering method called fuzzy clustering (using c-means algorithm). Like k-means, this method uses differences between observations. However, unlike k-means, FANNY does not create definitive clusters. Rather, this technique calculates probabilities of cluster membership for each observation, hence the name 'soft' clustering.")),
                          conditionalPanel(condition="input.method == 'fanny'",
                                                              strong(p("The visualized clusters are clusters that are distinct as determined by these membership probabilities. The cluster membership probabilities are reported in table under the visualization."))),
                                           sliderInput("clusts", "Number of Clusters/Centers", min=2, max=20, value=2, step=1, ticks=TRUE, animate = animationOptions(interval = 2000, loop = FALSE)),
                                           plotOutput("clusterelbow"),
                                           plotOutput("clustersil")),
                        mainPanel(
                          plotOutput("cluster", height = 900, width = 1200),
                          conditionalPanel(condition="input.method == 'fanny'",
                          dataTableOutput("fanfantab", width = 1200))),
                        position="left",
                        fluid=TRUE
                        ) #End of SidebarLayout
                      ), #End of TabPanel
             #Start of new tab
             tabPanel("Cluster Map",
                      sidebarLayout(
                        sidebarPanel(
                          width= 4,
                          h3("K-Means Cluster Map"),
                          selectInput("numclumap", "Number of Clusters", cluvar, selected=2),
                          dataTableOutput("clutable")
                        ), #End SidebarPanel
                        mainPanel(
                          leafletOutput("clustmap", height = 900, width = 1200)
                          ), #End MainPanel
                        position="left",
                        fluid=TRUE
                        )
                      ) #End of TabPanel
             )  #End of navbarPage 
  ) #End Fluid page

#################Server##############################
server<-function(input, output, session) {
  
  ###MAP###
  
  output$map <- renderLeaflet({
    data1<-low[low$year>= 1980, ]
    data1<-left_join(as.data.frame(Worldshapes), data1, by=c("ADM0_A3"="c3"))
    pall<-colorNumeric(palette = "Spectral", domain=NULL, na.color = "lightgrey")
    
    if(input$country == "All"){
      lng=10} 
    else{
      lng<-subset(centroids, country %in% input$country)$long
    }
    
    if(input$country == "All"){
      lat=50} 
    else{
      lat<-subset(centroids, country==input$country)$lat
    }

    
    if ((input$varvar=="gini")){
      data2<-data1[data1$year==input$year,]
      data2<-data2[,c(input$varvar)]

      j<-leaflet() %>%
        addTiles() %>%
        setView(lng = lng, lat = lat, zoom = 5) %>%
        addPolygons(data=Worldshapes, 
                    fillColor = ~pall(data2), 
                    fillOpacity = .55, 
                    stroke = TRUE,
                    weight=2,
                    color="white", 
                    popup= paste(round(data2, 2), "<br>"),
                    popupOptions = popupOptions(maxWidth ="100%", closeOnClick = TRUE),
                    highlightOptions = highlightOptions(weight = 3,
                                                        color = "black",
                                                        fillOpacity = 0.7,
                                                        bringToFront = TRUE,
                                                        stroke = TRUE))%>% 
        addLegend(pal = pall, 
                  values = data2, 
                  opacity = 0.7, 
                  title = "Legend",
                  position = "bottomleft"
                  
                  )
                    
      }
    else {
      data3<-data1[, c(input$varvar)]
      j<-leaflet() %>%
      addTiles() %>%
      setView(lng = lng, lat = lat, zoom = 5) %>%
      addPolygons(data=Worldshapes, 
                  fillColor = ~pall(data3), 
                  fillOpacity = .55, 
                  stroke = TRUE,
                  weight=2,
                  color="white",
                  popup= paste(round(data3,2), "<br>"),
                  popupOptions = popupOptions(maxWidth ="100%", closeOnClick = TRUE),
                  highlightOptions = highlightOptions(weight = 3,
                                                      color = "black",
                                                      fillOpacity = 0.7,
                                                      bringToFront = TRUE,
                                                      stroke = TRUE)) %>%
        addLegend(pal = pall, 
                  values = data3, 
                  opacity = 0.7, 
                  title = "Legend",
                  position = "bottomleft"
                
                  )
      }
  })
  
  ###Clustering###

  output$cluster <- renderPlot({
    if (input$method=="kmeans"){
      k<-kmeans(othervars3, centers = input$clusts, nstart = 100)
      fviz_cluster(k, data=othervars3, stand=TRUE, show.clust.cent = TRUE)+
        theme(legend.position = "none", text = element_text(size=20))
    }
    else if (input$method=="PAM"){
      pm<-pam(othervars3, k=input$clusts)
      fviz_cluster(pm, data=othervars3, stand=TRUE, show.clust.cent = TRUE)+
        theme(legend.position = "none", text = element_text(size=20))
    }
    else if (input$method=="fanny"){
      fanfan<-fanny(othervars3, k=input$clusts, stand=TRUE)
      fviz_cluster(fanfan, data=othervars3, stand=TRUE, show.clust.cent = TRUE)+
        theme(legend.position = "none", text = element_text(size=20))
    }
  })
  
  output$clusterelbow <- renderPlot({
    if (input$method=="kmeans"){
      scale(othervars3)
      #Elbow
      fviz_nbclust(othervars3, kmeans, method="wss")
    }
    else if (input$method=="PAM"){
      scale(othervars3)
      #Elbow
      fviz_nbclust(othervars3, FUNcluster = pam, method="wss")
    }
    else if (input$method=="fanny"){
      scale(othervars3)
      #Elbow
      fviz_nbclust(othervars3, FUNcluster = fanny, method="wss")
    }
  })
  
  output$clustersil <- renderPlot({
    if (input$method=="kmeans"){
      scale(othervars3)
      #Elbow
      fviz_nbclust(othervars3, kmeans, method = "silhouette", k.max = 10)
    }
    else if (input$method=="PAM"){
      scale(othervars3)
      #Elbow
      fviz_nbclust(othervars3, pam, method = "silhouette", k.max = 10)
    }
    else if (input$method=="fanny"){
      scale(othervars3)
      #Elbow
      fviz_nbclust(othervars3, fanny, method = "silhouette", k.max = 10)
    }
  })
  
  output$fanfantab <- DT::renderDataTable({
    fanfan<-fanny(othervars3, k=input$clusts, stand=TRUE)
    fanfan<-fanfan$membership
    fanfan <-round(fanfan,3)
    DT::datatable(fanfan)})
  
  ###Cluster Map###
  output$clustmap <- renderLeaflet({
    data1<-low[low$year>= 1980, ]
    data1<-left_join(as.data.frame(Worldshapes), data1, by=c("ADM0_A3"="c3"))
    clu2<-clu1[c(1,as.numeric(input$numclumap))]
    colnames(clu2)<-c('country', 'cluster')
    data4<-left_join(data1, clu2, by='country')
    data4<-distinct(data4, data4$ADM0_A3, .keep_all = TRUE)
    pall<-colorNumeric(palette = "Spectral", domain=NULL, na.color = "lightgrey")
    
    l<-leaflet() %>%
      addTiles() %>%
      setView(lng = 0, lat = 0, zoom = 3) %>%
      addPolygons(data=Worldshapes, 
                  fillColor = ~pall(data4$cluster), 
                  fillOpacity = .55, 
                  stroke = TRUE,
                  weight=2,
                  color="white", 
                  popup= paste(data4$cluster, "<br>"),
                  popupOptions = popupOptions(maxWidth ="100%", closeOnClick = TRUE),
                  highlightOptions = highlightOptions(weight = 3,
                                                      color = "black",
                                                      fillOpacity = 0.7,
                                                      bringToFront = TRUE,
                                                      stroke = TRUE)) %>% 
      addLegend(pal = pall, 
                values = data4$cluster, 
                opacity = 0.7, 
                title = "Legend",
                bins = c(1:10),
                position = "bottomleft")
    

  })
  
  output$clutable <- DT::renderDataTable({
    DT::datatable(clu1[,c(1,as.numeric(input$numclumap))], colnames = c("Country", "Cluster"))})
}

shinyApp(ui = ui, server = server)