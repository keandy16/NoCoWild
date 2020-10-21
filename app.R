
#Load the required packages to run the app
rm(list = ls())
library(tidyverse)
library(shiny)
library(dplyr)
library(readr)
library(ggplot2)
library(tidyverse)
library(htmltools)
library(rgdal)
library(leaflet)
library(plotrix)
library(treemapify)
library(plotly)

#Load the csv files
all_forests <- read_csv("Data/CSVs/all_forests.csv")
dat_molten<- read_csv("Data/CSVs/dat_molten.csv")
dat_sum<- read_csv("Data/CSVs/dat_sum.csv")
dataFinal<- read_csv("Data/CSVs/dataFinal.csv")
mammals<- read_csv("Data/CSVs/mammals.csv")
newData<- read_csv("Data/CSVs/newData.csv")
divFinal<- read_csv("Data/CSVs/divFinal.csv")
Activity <- read_csv("Data/CSVs/Activity.csv")


#Load the shapefiles to prep the map
Forests<-readOGR("Data/Shapefiles/Study Forest Locations.shp", layer = "Study Forest Locations")

#Project shape file
Forests_proj<-spTransform(Forests, CRS("+proj=longlat +datum=WGS84"))

#Add column with unabbreviated forest name
Forests_proj@data$Forest <- with(Forests@data, ifelse(
  ForestCode == "SH", 'South Hammond', ifelse(
    ForestCode == "BC", 'Beaver Creek', ifelse(
      ForestCode == "DON", 'Donnerville', ifelse(
        ForestCode == "WHIP", 'Whippoorwill Corners', ifelse(
          ForestCode == "WF", 'Whiskey Flats', ifelse(
            ForestCode == "DEG", 'Degrasse', 'whoops')))))))

#add columns to all_forests and data_molten for labeling treemap
dat_molten$label<-paste(dat_molten$variable, round(dat_molten$value, 1), sep = "\n")
all_forests$label<-paste(all_forests$variable, round(all_forests$value, 1), sep = "\n")



ui <- fluidPage(
  
  # App title ----
  titlePanel("North Country Wild Project"),
  
  tabsetPanel(
    #Intro tab
    tabPanel("Introduction", fluid = TRUE,
             sidebarLayout(
               sidebarPanel(img(src = "NatureUpNorth.png", height = 100, width = 240),
                            h6("Created by Kate Andy, 2020")),
               
             mainPanel(h4("Welcome to the North Country Wild Project! Our team set up camera traps
             in 6 state forests in St. Lawrence County and collected data for over a year.
             Our goal was to determine the mammal diversity in St. Lawrence County and compare diversity between deciduous and replanted 
             pine forest types. As our climate changes, habitat that was once suitable for some mammals may no longer
             be habitable. St. Lawrence County is situated within a proposed wildlife corridor
             between the Adirondack State Park in New York State, United States and Algonquin Provincial
             Park in Ontario, Canada (shown in the map below). Wildlife corridors are connected landscapes that allow species 
             to move to larger reserves. If we can understand how mammals are 
             using the forests in St. Lawrence County, we can select priority habitat to conserve in 
             the proposed wildlife corridor. Thanks to volunteers who helped classify our photo data,
             we have created interactive maps and graphs of our results. We hope this app will
             show you what mammals are out there in St. Lawrence County and how these mammals are
             using the available space. At the top of this page are tabs you 
               can click on that will bring up a different interactive graphic. Let's get exploring!"),
                       uiOutput("map")
             )    
             )
    ),
  

    #Map of Study Sites
    tabPanel("Map of Study Sites", fluid = TRUE,
             sidebarLayout(
               sidebarPanel(selectInput("species", h3("Choose your Species"),
                                        choices = c("All Mammals",
                                                    "Black Bear",
                                                    "Bobcat",
                                                    "Chipmunk",
                                                    "Cottontail Rabbit",
                                                    "Coyote",
                                                    "Fisher",
                                                    "Flying Squirrel",
                                                    "Gray Squirrel",
                                                    "Mink",
                                                    "Opossum",
                                                    "Other Small Mammal",
                                                    "Porcupine",
                                                    "Raccoon",
                                                    "Red Fox",
                                                    "Red Squirrel",
                                                    "River Otter",
                                                    "Snowshoe Hare",
                                                    "Striped Skunk",
                                                    "Weasel",
                                                    "White-tailed Deer"),
                                        selected = "All Mammals"),
                            
                            img(src = "NatureUpNorth.png", height = 100, width = 240)
               ),
               
                         
                         mainPanel(h5("Use the map below to explore the state forests we sampled for this study in 2019. 
                            Select the mammal you want to learn about on the left and pan over the forests
                            (outlined in red) to see how many detections we were able to get."),
                         leafletOutput("speciesmap"))   
             )
    ),
    #Number of Detections per Species per Forest 
    tabPanel("Species Detections", fluid = TRUE,
             sidebarLayout(
               sidebarPanel(selectInput("forest", h3("Choose your Forest"),
                                        choices = c("All Forests", 
                                                    "South Hammond", 
                                                    "Donnerville", 
                                                    "Beaver Creek", 
                                                    "Whippoorwill Corners", 
                                                    "Whiskey Flats", 
                                                    "Degrasse"), selected = "South Hammond"),
                            checkboxGroupInput("animals",
                                               h3("Choose your Species"),
                                               choices = list("All Mammals",
                                                              "Black Bear",
                                                              "Bobcat",
                                                              "Chipmunk",
                                                              "Cottontail Rabbit",
                                                              "Coyote",
                                                              "Fisher",
                                                              "Flying Squirrel",
                                                              "Gray Squirrel",
                                                              "Mink",
                                                              "Opossum",
                                                              "Other Small Mammal",
                                                              "Porcupine",
                                                              "Raccoon",
                                                              "Red Fox",
                                                              "Red Squirrel",
                                                              "River Otter",
                                                              "Snowshoe Hare",
                                                              "Striped Skunk",
                                                              "Weasel",
                                                              "White-tailed Deer"),
                                               selected = "Black Bear"),
                            
                            img(src = "NatureUpNorth.png", height = 100, width = 240)
               ),
               mainPanel(h5("This graph displays the number of detections per species per study site.
               Select the forest and which mammals you want to learn about on the left and pan over 
                            the bars on the graph to see the number of detections."),
                         plotlyOutput(outputId = "foresthist"))   
             )
    ),
    #Number of Detections per Species per Forest Tab #2
    tabPanel("Species Detections per Forest", fluid = TRUE,
             sidebarLayout(
               sidebarPanel(selectInput("spec", h3("Choose your Species"),
                                        choices = c("All Mammals",
                                                    "Black Bear",
                                                    "Bobcat",
                                                    "Chipmunk",
                                                    "Cottontail Rabbit",
                                                    "Coyote",
                                                    "Fisher",
                                                    "Flying Squirrel",
                                                    "Gray Squirrel",
                                                    "Mink",
                                                    "Opossum",
                                                    "Other Small Mammal",
                                                    "Porcupine",
                                                    "Raccoon",
                                                    "Red Fox",
                                                    "Red Squirrel",
                                                    "River Otter",
                                                    "Snowshoe Hare",
                                                    "Striped Skunk",
                                                    "Weasel",
                                                    "White-tailed Deer"),
                                        selected = "Black Bear"),
                            checkboxGroupInput("forests",
                                               h3("Choose your Forest"),
                                               choices = list("All Forests",
                                                              "South Hammond", 
                                                              "Donnerville", 
                                                              "Beaver Creek", 
                                                              "Whippoorwill Corners", 
                                                              "Whiskey Flats", 
                                                              "Degrasse"), selected = "South Hammond"),
                            
                            
                            img(src = "NatureUpNorth.png", height = 100, width = 240)
               ),
               mainPanel(h5("This graph is another way to represent the number of species
               detections per study site. On the panel to the left, first select the species you want to learn about using the 
               menu and then select your forest(s) by clicking the checkboxes. Pan over 
                            the bars on the graph to see the number of detections per forest."),
                         plotlyOutput(outputId = "speciesdetect"))   
             )
    ),
    #Mammal Activity Patterns 
    tabPanel("Mammal Activity Patterns", fluid = TRUE,
             sidebarLayout(
               sidebarPanel(selectInput("mammals", h3("Choose your Species"),
                                        choices = c("Black Bear",
                                                    "Bobcat",
                                                    "Chipmunk",
                                                    "Cottontail Rabbit",
                                                    "Coyote",
                                                    "Fisher",
                                                    "Flying Squirrel",
                                                    "Gray Squirrel",
                                                    "Opossum",
                                                    "Other Small Mammal",
                                                    "Porcupine",
                                                    "Raccoon",
                                                    "Red Fox",
                                                    "Red Squirrel",
                                                    "Weasel",
                                                    "White-tailed Deer"),
                                        selected = "Black Bear"),
                            
                            checkboxInput("rare", "Species Detected Infrequently", value = FALSE),
                            
                            
                            img(src = "NatureUpNorth.png", height = 100, width = 240)
               ),
               mainPanel(h5("Mammals are not all active at the same time! Select a mammal using the tab on the
               left and learn when mammals were detected by our cameras. The graph is shaped like a clock with 
               0-23 symbolizing the hour we detected the mammal.The x-axis lets you know how many detections we
                            had during that hour. There were four mammals that were rarely detected by our cameras.
                            Click on the checkbox to find out who they are!"),
                         plotOutput(outputId = "activity"), uiOutput(outputId = "image"))   
             )
    ),
    
    #Species Trophic Level
    tabPanel("Species Trophic Level", fluid = TRUE,
             sidebarLayout(
               sidebarPanel(selectInput("sites", h3("Choose your Forest"),
                                        choices = c("All Forests", 
                                                    "South Hammond", 
                                                    "Donnerville", 
                                                    "Beaver Creek", 
                                                    "Whippoorwill Corners", 
                                                    "Whiskey Flats", 
                                                    "Degrasse"), selected = "South Hammond"),
                            
                            
                            img(src = "NatureUpNorth.png", height = 100, width = 240)
               ),
               mainPanel(h5("For this graph, we grouped species according to their dietary preference. 
               Herbivores eat primarily plants, mesocarivores are medium-sized mammals with meat-eating diets,
               omnivores eat a variety of foods, and carnivores eat primarily meat. 
                            Select the forest using the tab in the panel to the left and compare graphs."),
                         plotlyOutput(outputId = "trophic"))   
             )
    ),
    #Forest Composition
    tabPanel("Forest Composition", fluid = TRUE,
             sidebarLayout(
               sidebarPanel(selectInput("locations", h3("Choose your Forest"),
                                        choices = c("All Forests", 
                                                    "South Hammond", 
                                                    "Donnerville", 
                                                    "Beaver Creek", 
                                                    "Whippoorwill Corners", 
                                                    "Whiskey Flats", 
                                                    "Degrasse"), selected = "South Hammond"),
                            
                            
                            img(src = "NatureUpNorth.png", height = 100, width = 240)
               ),
               mainPanel(h5("The graphic below shows the forest composition of each study site. Forests are not
               homogeneous. Even a forest composed of mostly deciduous trees has other subhabitats present. Choose 
                            a forest using the tab in the panel to the left to see how forests vary. If you 
                            select 'All Forests' the graphic will show an average across all forests." ),
                         plotOutput(outputId = "covariates"))   
             )
    ),
    #Forest Diversity
    tabPanel("Forest Diversity", fluid = TRUE,
             sidebarLayout(
               sidebarPanel(selectInput("study", h3("Choose your Forest"),
                                        choices = c("All Forests", 
                                                    "South Hammond", 
                                                    "Donnerville", 
                                                    "Beaver Creek", 
                                                    "Whippoorwill Corners", 
                                                    "Whiskey Flats", 
                                                    "Degrasse"), selected = "South Hammond"),
                            
                            
                            img(src = "NatureUpNorth.png", height = 100, width = 240)
               ),
               mainPanel(h5("This graph displays three different diversity indices calculated per study site.
              Shannon and Simpson Indices measure diversity by accounting for species abundance and evenness.
              Species Richness is the total number of species detected.
                            Select the forest using the tab in the panel to the left and learn how species
                            diversity vary per forest."),
                         plotlyOutput(outputId = "diversity"))   
             )
    )
    
  )
)



server <- function(input, output){
  
  #Introduction
  output$map <- renderUI({
  img(src = "StudySites.png", height = 400, width = 500)
  })


  #Map of Study Sites
  output$speciesmap<-renderLeaflet({
    
    if(input$species == "All Mammals"){
      data<-dat_sum %>% group_by(Forest) %>% summarise(
        number_det = sum(number_det)
      )
      Forests_proj@data <- left_join(Forests_proj@data, data, by = c("Forest"= "Forest"))
      pal <- colorNumeric("Blues", domain= Forests_proj$number_det) 
      labels<-sprintf( "%s, %s Detections", 
                       Forests_proj$Forest, Forests_proj$number_det) %>% lapply(htmltools::HTML)
      leaflet() %>% addTiles() %>% 
        setView(lng = -75.169395, lat = 44.595466, zoom = 8.5) %>% 
        addPolygons(
          data = Forests_proj, 
          fillColor = ~pal(Forests_proj$number_det),
          fillOpacity = 1,
          weight = 1, 
          col = 'red',
          highlight = highlightOptions(#highlight lets you mouse over a site and have it change color
            weight = 5,
            color = "orange", 
            bringToFront = T),
          label = labels,
          labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "15px",
            direction = "auto")) 
      
    } else{
      data<-switch(input$species, 
                   "White-tailed Deer" = dat_sum %>% filter(choice == "DEERWHITETAILED"),
                   "Chipmunk" = dat_sum %>% filter(choice == "CHIPMUNK"),
                   "Cottontail Rabbit" = dat_sum %>% filter(choice == "COTTONTAILRABBIT"),
                   "Coyote" = dat_sum %>% filter(choice == "COYOTE"),
                   "Fisher" = dat_sum %>% filter(choice == "FISHER"),
                   "Raccoon" = dat_sum %>% filter(choice == "RACCOON"),
                   "Red Squirrel" = dat_sum %>% filter(choice == "SQUIRRELRED"),
                   "Gray Squirrel" = dat_sum %>% filter(choice == "SQUIRRELGRAY"),
                   "Black Bear" = dat_sum %>% filter(choice == "BLACKBEAR"),
                   "Red Fox" = dat_sum %>% filter(choice == "FOXRED"),
                   "Porcupine" = dat_sum %>% filter(choice == "PORCUPINE"),
                   "Bobcat" = dat_sum %>% filter(choice == "BOBCAT"),
                   "Weasel" = dat_sum %>% filter(choice == "WEASEL"),
                   "Striped Skunk" = dat_sum %>% filter(choice == "SKUNKSTRIPED"),
                   "Flying Squirrel" = dat_sum %>% filter(choice == "SQUIRRELFLYING"),
                   "Snowshoe Hare" = dat_sum %>% filter(choice == "SNOWSHOEHARE"),
                   "River Otter" = dat_sum %>% filter(choice == "RIVEROTTER"),
                   "Mink" = dat_sum %>% filter(choice == "MINK"),
                   "Other Small Mammal" = dat_sum %>% filter(choice == "OTHERSMALLMAMMAL"),
                   "Opossum" = dat_sum %>% filter(choice == "OPOSSUM"),)
      #Join data to shape file
      Forests_proj@data <- left_join(Forests_proj@data, data, by = c("Forest"= "Forest"))
      pal <- colorNumeric("Blues", domain= Forests_proj$number_det) 
      labels<-sprintf( "%s, %s Detections", 
                       Forests_proj$Forest, Forests_proj$number_det) %>% lapply(htmltools::HTML)
      leaflet() %>% addTiles() %>% 
        setView(lng = -75.169395, lat = 44.595466, zoom = 8.5) %>% 
        addPolygons(
          data = Forests_proj, 
          fillColor = ~pal(Forests_proj$number_det),
          fillOpacity = 1,
          weight = 1, 
          col = 'red',
          highlight = highlightOptions(#highlight lets you mouse over a site and have it change color
            weight = 5,
            color = "orange", 
            bringToFront = T),
          label = labels,
          labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "15px",
            direction = "auto")) 
    }
    
  })
  
  
  
  #Number of Detections per Species per Forest
  output$foresthist <- renderPlotly({
    
    if("All Mammals" %in% input$animals){
      choices<-c(unique(newData$Species))
      data<-newData %>% filter(Species %in% choices)
      study<-reactive(switch(input$forest,
                             "All Forests" = data,
                             "South Hammond" = data %>% filter(Forest=="South Hammond"),
                             "Beaver Creek" = data %>% filter(Forest=="Beaver Creek"),
                             "Donnerville" = data %>% filter(Forest == "Donnerville"),
                             "Whippoorwill Corners" = data %>% filter(Forest == "Whippoorwill Corners"),
                             "Whiskey Flats" = data %>% filter(Forest == "Whiskey Flats"),
                             "Degrasse" = data %>% filter(Forest == "Degrasse"))
      )
      p<- ggplot(study(), aes(Species)) +
        geom_histogram(stat = "count", position = "dodge", fill = '#165970', colour = '#543b1f') +
        theme_bw() +
        xlab("Species") +
        ylab("Number of Detections") +
        theme(axis.text.x = element_text(angle = 90, size = 10, vjust = 0.5)) + 
        labs(title = "Number of Detections per Species") +
        theme(plot.title = element_text(hjust=0.5))
      ggplotly(p) %>% config(displayModeBar = F)
      
    }
    else{
      choices<-c(input$animals)
      data<-newData %>% filter(Species %in% choices)
      study<-reactive(switch(input$forest,
                             "All Forests" = data,
                             "South Hammond" = data %>% filter(Forest=="South Hammond"),
                             "Beaver Creek" = data %>% filter(Forest=="Beaver Creek"),
                             "Donnerville" = data %>% filter(Forest == "Donnerville"),
                             "Whippoorwill Corners" = data %>% filter(Forest == "Whippoorwill Corners"),
                             "Whiskey Flats" = data %>% filter(Forest == "Whiskey Flats"),
                             "Degrasse" = data %>% filter(Forest == "Degrasse"))
      )
      p<-ggplot(study(), aes(Species)) +
        geom_histogram(stat = "count", position = "dodge", fill = '#165970', colour = '#543b1f') +
        theme_bw() +
        xlab("Species") +
        ylab("Number of Detections") +
        theme(axis.text.x = element_text(angle = 90, size = 10, vjust = 0.5)) +
        labs(title = "Number of Detections per Species") +
        theme(plot.title = element_text(hjust=0.5))
      ggplotly(p) %>% config(displayModeBar = F)
      
    }
  })
  #Number of Detections per Species per Forest part #2
  output$speciesdetect <- renderPlotly({
    
    if("All Forests" %in% input$forests){
      data<-newData
      study<-reactive(switch(input$spec,
                             "All Mammals" = data,
                             "White-tailed Deer" = data %>% filter(Species == "White-tailed Deer"),
                             "Chipmunk" = data %>% filter(Species == "Chipmunk"),
                             "Cottontail Rabbit" = data %>% filter(Species == "Cottontail Rabbit"),
                             "Coyote" = data %>% filter(Species == "Coyote"),
                             "Fisher" = data %>% filter(Species == "Fisher"),
                             "Raccoon" = data %>% filter(Species == "Raccoon"),
                             "Red Squirrel" = data %>% filter(Species == "Red Squirrel"),
                             "Gray Squirrel" = data %>% filter(Species == "Gray Squirrel"),
                             "Black Bear" = data %>% filter(Species == "Black Bear"),
                             "Red Fox" = data %>% filter(Species == "Red Fox"),
                             "Porcupine" = data %>% filter(Species == "Porcupine"),
                             "Bobcat" = data %>% filter(Species == "Bobcat"),
                             "Weasel" = data %>% filter(Species == "Weasel"),
                             "Striped Skunk" = data %>% filter(Species == "Striped Skunk"),
                             "Flying Squirrel" = data %>% filter(Species == "Flying Squirrel"),
                             "Snowshoe Hare" = data %>% filter(Species == "Snowshoe Hare"),
                             "River Otter" = data %>% filter(Species == "River Otter"),
                             "Mink" = data %>% filter(Species == "Mink"),
                             "Other Small Mammal" = data %>% filter(Species == "Other Small Mammal"),
                             "Opossum" = data %>% filter(Species == "Opossum"))
      )
      
      validate(
        need(nrow(study()) > 0, "Oh no! Looks like we did not detect that mammal here.")
      )
      
      p<-ggplot(study(), aes(Forest)) +
        geom_histogram(stat = "count", position = "dodge", fill = '#165970', colour = '#543b1f') +
        theme_bw() +
        xlab("Forest") +
        ylab("Number of Detections") +
        theme(axis.text.x = element_text(angle = 90, size = 10, vjust = 0.5)) +
        labs(title = "Number of Species Detections per Forest") +
        theme(plot.title = element_text(hjust=0.5))
      ggplotly(p) %>% config(displayModeBar = F)
    }
    
    else{
      choices<-c(input$forests)
      data<-newData %>% filter(Forest %in% choices)
      study<-reactive(switch(input$spec,
                             "All Mammals" = data,
                             "White-tailed Deer" = data %>% filter(Species == "White-tailed Deer"),
                             "Chipmunk" = data %>% filter(Species == "Chipmunk"),
                             "Cottontail Rabbit" = data %>% filter(Species == "Cottontail Rabbit"),
                             "Coyote" = data %>% filter(Species == "Coyote"),
                             "Fisher" = data %>% filter(Species == "Fisher"),
                             "Raccoon" = data %>% filter(Species == "Raccoon"),
                             "Red Squirrel" = data %>% filter(Species == "Red Squirrel"),
                             "Gray Squirrel" = data %>% filter(Species == "Gray Squirrel"),
                             "Black Bear" = data %>% filter(Species == "Black Bear"),
                             "Red Fox" = data %>% filter(Species == "Red Fox"),
                             "Porcupine" = data %>% filter(Species == "Porcupine"),
                             "Bobcat" = data %>% filter(Species == "Bobcat"),
                             "Weasel" = data %>% filter(Species == "Weasel"),
                             "Striped Skunk" = data %>% filter(Species == "Striped Skunk"),
                             "Flying Squirrel" = data %>% filter(Species == "Flying Squirrel"),
                             "Snowshoe Hare" = data %>% filter(Species == "Snowshoe Hare"),
                             "River Otter" = data %>% filter(Species == "River Otter"),
                             "Mink" = data %>% filter(Species == "Mink"),
                             "Other Small Mammal" = data %>% filter(Species == "Other Small Mammal"),
                             "Opossum" = data %>% filter(Species == "Opossum"))
      )
      
      validate(
        need(nrow(study()) > 0, "Oh no! Looks like we did not detect that mammal here.")
      )
      
      p<-ggplot(study(), aes(Forest)) +
        geom_histogram(stat = "count", position = "dodge", fill = '#165970', colour = '#543b1f') +
        theme_bw() +
        xlab("Forest") +
        ylab("Number of Detections") +
        theme(axis.text.x = element_text(angle = 90, size = 10, vjust = 0.5)) +
        labs(title = "Number of Species Detections per Forest") +
        theme(plot.title = element_text(hjust=0.5))
      ggplotly(p) %>% config(displayModeBar = F)
    }
    
    
  })
  #Species Activity Patterns
  output$activity<-renderPlot({
    
    if(input$rare){
      Rare<-Activity %>% filter(bin == "MINK" | bin == "RIVEROTTER" |bin == "SNOWSHOEHARE" |bin == "SKUNKSTRIPED")
      
      ggplot(Rare, aes(x= truncHour, y = NumObs, fill = Species)) + 
        geom_bar(stat = "identity",position= position_dodge(), width = 0.7) +
        labs(title = "Number of Detections per Hour", x= "Time of Detection (hour)", y= "Number of Detections") +
        theme (plot.title =element_text(hjust = 0.5),
               axis.text.x = element_text(angle = 90, size = 10, vjust = 0.5)) + 
        scale_fill_manual(values = c("Mink" = "#ABC4E0",
                                     "River Otter" = "#d4b18a",
                                     "Striped Skunk" = "#C6ABE1",
                                     "Snowshoe Hare" = "#39541e")) 
      
      
      
    }
    
    else{
      data<-switch(input$mammals, 
                   "White-tailed Deer" = Activity %>% filter(bin == "DEERWHITETAILED"),
                   "Chipmunk" = Activity %>% filter(bin == "CHIPMUNK"),
                   "Cottontail Rabbit" = Activity %>% filter(bin == "COTTONTAILRABBIT"),
                   "Coyote" = Activity %>% filter(bin == "COYOTE"),
                   "Fisher" = Activity %>% filter(bin=="FISHER"),
                   "Raccoon" = Activity %>% filter(bin =="RACCOON"),
                   "Red Squirrel" = Activity %>% filter(bin == "SQUIRRELRED"),
                   "Gray Squirrel" = Activity %>% filter(bin =="SQUIRRELGRAY"),
                   "Black Bear" = Activity %>% filter(bin == "BLACKBEAR"),
                   "Red Fox" = Activity %>% filter(bin == "FOXRED"),
                   "Porcupine" = Activity %>% filter(bin == "PORCUPINE"),
                   "Bobcat" = Activity %>% filter( bin == "BOBCAT"),
                   "Weasel" = Activity %>% filter(bin == "WEASEL"),
                   "Flying Squirrel" = Activity %>% filter(bin== "SQUIRRELFLYING"),
                   "Other Small Mammal" = Activity %>% filter(bin == "OTHERSMALLMAMMAL"),
                   "Opossum" = Activity %>% filter(bin == "OPOSSUM"))
      
      clock<-c(0:23)
      clock24.plot(data$NumObs, clock, show.grid = T, lwd = 2, line.col = "blue", cex.lab = 0.5)
      
    }
    
  })
  
  output$image <- renderUI({    
    
    if(input$rare){
      img(src = "Rare.png", height = 220, width = 600, align = "left")
    }
    
    else if(input$mammals == "White-tailed Deer"){
      img(src = "Deer.png", height = 240, width = 300, align = "left")
    }
    else if(input$mammals == "Chipmunk"){
      img(src = "Chipmunk.png", height = 240, width = 300, align = "left")
    }
    else if (input$mammals == "Cottontail Rabbit"){
      img(src = "Cottontail.png", height = 240, width = 300, align = "left")
    }
    else if(input$mammals == "Coyote"){
      img(src = "Coyote.png", height = 240, width = 300, align = "left")
    }
    else if(input$mammals == "Fisher"){
      img(src = "Fisher.png", height = 240, width = 300, align = "left")
    }
    else if(input$mammals == "Raccoon"){
      img(src = "Raccoon.png", height = 240, width = 300, align = "left")
    }
    else if(input$mammals == "Red Squirrel"){
      img(src = "RedSquirrel.png", height = 240, width = 300, align = "left")
    }
    else if(input$mammals == "Other Small Mammal"){
      img(src = "Mole.png", height = 240, width = 300, align = "left")
    }
    else if(input$mammals == "Gray Squirrel"){
      img(src = "GraySquirrel.png", height = 240, width = 300, align = "left")
    }
    else if(input$mammals == "Black Bear"){
      img(src = "blackbear.png", height = 240, width = 300, align = "left")
    }
    else if(input$mammals == "Red Fox"){
      img(src = "RedFox.png", height = 240, width = 300, align = "left")
    }
    else if(input$mammals == "Porcupine"){
      img(src = "Porcupine.png", height = 240, width = 300, align = "left")
    }
    else if(input$mammals == "Bobcat"){
      img(src = "Bobcat.png", height = 240, width = 300, align = "left")
    }
    else if(input$mammals == "Opossum"){
      img(src = "Opossum.png", height = 240, width = 300, align = "left")
    }
    else if(input$mammals == "Weasel"){
      img(src = "Weasel.png", height = 240, width = 300, align = "left")
    }
    else if(input$mammals == "Flying Squirrel"){
      img(src = "FlyingSquirrel.png", height = 240, width = 300, align = "left")
    }
    
    
  })
  
  
  
  #Species Trophic Levels
  output$trophic <- renderPlotly({
    
    data<-switch(input$sites, 
                 "All Forests" = mammals,
                 "Whiskey Flats" = mammals %>% filter(ForestName=="WF"),
                 "South Hammond" = mammals %>% filter(ForestName=="SH"),
                 "Donnerville" = mammals %>% filter(ForestName == "DON"),
                 "Beaver Creek" = mammals %>% filter(ForestName == "BC"),
                 "Degrasse" = mammals %>% filter(ForestName == "DEG"),
                 "Whippoorwill Corners" = mammals %>% filter(ForestName == "WHIP"),)
    
    p<-ggplot(data, aes(Diet)) + 
      geom_histogram(stat = "count", position = "dodge", fill = '#165970', colour = '#543b1f') + 
      labs(title = "Trophic Levels per Forest", x="Trophic Level", y="Number of Detections") +
      theme(axis.text.x = element_text(angle = 90, size = 10, vjust = 0.5))+ theme(plot.title = element_text(hjust=0.5))
    ggplotly(p) %>% config(displayModeBar = F)
    
  })
  #Forest Composition
  output$covariates <- renderPlot({
    
    data<-switch(input$locations, 
                 "All Forests" = all_forests,
                 "South Hammond" = dat_molten %>% filter(Forest=="SH"),
                 "Beaver Creek" = dat_molten %>% filter(Forest=="BC"),
                 "Donnerville" = dat_molten %>% filter(Forest=="DON"),
                 "Degrasse" = dat_molten %>% filter(Forest=="DEG"),
                 "Whippoorwill Corners" = dat_molten %>% filter(Forest=="WHIP"),
                 "Whiskey Flats" = dat_molten %>% filter(Forest=="WF"),)
    
    #treemap
    
    ggplot(data,(aes(area = value, label = label, fill = as.factor(variable))))+
      geom_treemap()+
      geom_treemap_text(color = "white", place = "topleft", grow = TRUE)+
      scale_fill_manual(values = c("Water" = "#165970",
                                   "Mixed" = "#543b1f",
                                   "Evergreen" = "#C6ABE1",
                                   "Deciduous" = "#39541e",
                                   "Development" = "#ABC4E0",
                                   "Agriculture" = "#E9A68F",
                                   "Barren" = "#b9da97",
                                   "Shrub" = "#d4b18a",
                                   "Wetland" = "#69c3e1",
                                   "Herbaceous" = "Gray"))+
      theme(
        legend.position = "bottom", 
        plot.title=element_text(size = 15),
        legend.text=element_text(size = 10),
        legend.title=element_text(size = 15)
      )+
      labs(title="Forest Composition",
           fill = "Land Use Category")+ theme(plot.title = element_text(hjust=0.5))
    
    
    
    
  })
  #Forest Diversity 
  output$diversity <- renderPlotly({
    
    data<-switch(input$study, 
                 "All Forests" = divFinal,
                 "South Hammond" = divFinal %>% filter(Forest_Code == "SH"),
                 "Beaver Creek" = divFinal %>% filter(Forest_Code == "BC"),
                 "Donnerville" = divFinal %>% filter(Forest_Code == "DON"),
                 "Degrasse" = divFinal %>% filter(Forest_Code == "DEG"),
                 "Whippoorwill Corners" = divFinal %>% filter(Forest_Code == "WHIP"),
                 "Whiskey Flats" = divFinal %>% filter(Forest_Code == "WF"),)
    
    p<- ggplot(data, aes(x= Forest, y = Index, fill = Diversity)) + 
      geom_bar(stat = "identity",position= position_dodge(), width = 0.7) +
      labs(title = "Diversity Indices per Forest", x= "Forest", y= "Diversity Index", fill = "Diversity Index") +
      theme (plot.title =element_text(hjust = 0.5),
             axis.text.x = element_text(angle = 90, size = 10, vjust = 0.5)) + 
      scale_fill_manual(values = c("Shannon Index" = "#165970",
                                   "Simpson Index" = "#543b1f",
                                   "Species Richness" = "#C6ABE1")) 
    ggplotly(p) %>% config(displayModeBar = F)
    
  })
  
}
shinyApp(ui = ui, server = server)