

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
library(rsconnect)

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
  titlePanel("North Country Wild Zooniverse Project"),
  tabsetPanel(
    #Map of Study Sites
    tabPanel("Map of Study Sites", fluid = TRUE,
             sidebarLayout(
               sidebarPanel(selectInput("species", h3("Choose your Species"),
                                        choices = c("All Mammals",
                                                    "Black Bear",
                                                    "Bobcat",
                                                    "Chipmunk",
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
               mainPanel(h5("Use the map below to explore the state forests we sampled for this study. 
                            Select the mammal you want to learn about and pan over the forests
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
               Select the forest and which mammals you want to learn about and see the number of detections."),
                         plotOutput(outputId = "foresthist"))   
             )
    ),
    #Mammal Activity Patterns 
    tabPanel("Mammal Activity Patterns", fluid = TRUE,
             sidebarLayout(
               sidebarPanel(selectInput("mammals", h3("Choose your Species"),
                                        choices = c("Black Bear",
                                                              "Bobcat",
                                                              "Chipmunk",
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
               mainPanel(h5("Mammals are not all active at the same time! Select a mammal using the tab in the 
               panel to the left and learn what times of day that mammal was detected by our cameras.
               The graph is shaped like a clock with 0-23 symbolizing the hour we detected the mammal.
               The x-axis lets you know how many detections we had during that hour."),
                 plotOutput(outputId = "activity"))   
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
               mainPanel(h5("This graph displays the dietary preferences of the mammals detected at each 
                            study site. Select the forest using the tab in the panel to the left and compare graphs."),
                         plotOutput(outputId = "trophic"))   
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
               mainPanel(h5("The graphic below shows the forest composition of each study site. Choose 
                            a forest using the tab in the panel to the left and compare results. If you 
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
               The three measures of diversity include: Shannon Index, Inverse Simpson Index, and Species Richness. 
                            Select the forest using the tab in the panel to the left and compare graphs."),
                         plotOutput(outputId = "diversity"))   
             )
    )
    
  )
)


server <- function(input, output){

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
  output$foresthist <- renderPlot({
    
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
                             ggplot(study(), aes(Species)) +
                               geom_histogram(stat = "count", position = "dodge", fill = '#165970', colour = '#543b1f') +
                               theme_bw() +
                               xlab("Species") +
                               ylab("Number of Detections") +
                               theme(axis.text.x = element_text(angle = 90, size = 10, vjust = 0.5)) + 
                               labs(title = "Number of Detections per Species") +
                             theme(plot.title = element_text(hjust=0.5))
                             
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
                             ggplot(study(), aes(Species)) +
                               geom_histogram(stat = "count", position = "dodge", fill = '#165970', colour = '#543b1f') +
                               theme_bw() +
                               xlab("Species") +
                               ylab("Number of Detections") +
                               theme(axis.text.x = element_text(angle = 90, size = 10, vjust = 0.5)) +
                               labs(title = "Number of Detections per Species") +
                             theme(plot.title = element_text(hjust=0.5))
                             
                           }
    })
  #Species Activity Patterns
  output$activity<-renderPlot({
    
    title<- sprintf( "%s Activity Patterns", input$mammals) %>% lapply(htmltools::HTML)
    
    
    data<-switch(input$mammals, 
                 "White-tailed Deer" = Activity %>% filter(bin == "DEERWHITETAILED"),
                 "Chipmunk" = Activity %>% filter(bin == "CHIPMUNK"),
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
                 "Striped Skunk" = Activity %>% filter(bin == "SKUNKSTRIPED"),
                 "Flying Squirrel" = Activity %>% filter(bin== "SQUIRRELFLYING"),
                 "Snowshoe Hare" = Activity %>% filter(bin == "SNOWSHOEHARE"),
                 "River Otter" = Activity %>% filter(bin == "RIVEROTTER"),
                 "Mink" = Activity %>% filter(bin == "MINK"),
                 "Other Small Mammal" = Activity %>% filter(bin == "OTHERSMALLMAMMAL"),
                 "Opossum" = Activity %>% filter(bin == "OPOSSUM"))
    
    clock<-c(0:23)
    clock24.plot(data$NumObs, clock, show.grid = T, lwd = 2, line.col = "#165970", cex.lab = 0.5, main = title)
    
  })
  
  
  
  #Species Trophic Levels
  output$trophic <- renderPlot({
    
    data<-switch(input$sites, 
                 "All Forests" = mammals,
                 "Whiskey Flats" = mammals %>% filter(ForestName=="WF"),
                 "South Hammond" = mammals %>% filter(ForestName=="SH"),
                 "Donnerville" = mammals %>% filter(ForestName == "DON"),
                 "Beaver Creek" = mammals %>% filter(ForestName == "BC"),
                 "Degrasse" = mammals %>% filter(ForestName == "DEG"),
                 "Whippoorwill Corners" = mammals %>% filter(ForestName == "WHIP"),)
    
    ggplot(data, aes(Trophic)) + 
      geom_histogram(stat = "count", position = "dodge", fill = '#165970', colour = '#543b1f') + 
      labs(title = "Trophic Levels per Forest", x="Trophic Level", y="Number of Detections") +
      theme(axis.text.x = element_text(angle = 90, size = 10, vjust = 0.5))+ theme(plot.title = element_text(hjust=0.5))
    
    
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
  output$diversity <- renderPlot({
    
    data<-switch(input$study, 
                 "All Forests" = divFinal,
                 "South Hammond" = divFinal %>% filter(Forest == "SH"),
                 "Beaver Creek" = divFinal %>% filter(Forest == "BC"),
                 "Donnerville" = divFinal %>% filter(Forest == "DON"),
                 "Degrasse" = divFinal %>% filter(Forest == "DEG"),
                 "Whippoorwill Corners" = divFinal %>% filter(Forest == "WHIP"),
                 "Whiskey Flats" = divFinal %>% filter(Forest == "WF"),)
    
    ggplot(data, aes(x= Forest_Name, y = Index, fill = Diversity_Index)) + 
      geom_bar(stat = "identity",position= position_dodge(), width = 0.7) +
      labs(title = "Diversity Indices per Forest", x= "Forest", y= "Diversity Index") +
     theme (plot.title =element_text(hjust = 0.5),
              axis.text.x = element_text(angle = 90, size = 10, vjust = 0.5)) + 
      scale_fill_manual(values = c("Shannon Index" = "#165970",
                                   "Inverse Simpson Index" = "#543b1f",
                                   "Species Richness" = "#C6ABE1")) 
    
  })
  
}
shinyApp(ui = ui, server = server)