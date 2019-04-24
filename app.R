

library(tidyverse)
library(shiny)
library(ggmap)
library(leaflet)

register_google(key = "your API key")
write = TRUE

datatree <- read.csv("trees.csv", stringsAsFactors = FALSE)
r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()
v <- data.frame()

ui <- fluidPage(
  leafletOutput("mymap", height = 500),
  p(),
  textInput(inputId = "address", label = "Address", placeholder = "e.g 85 Prospect Ave, Hackensack, NJ 07601"),
  actionButton("search", "Search"),
  actionButton("displaytrees", "Display Trees")
)

server <- function(input, output, session) {

  addresspoints <- eventReactive(input$search, {
    p <- c(input$address)
    q <- geocode(p)
    points1 <- data.frame (q)
    return(points1)
  }, ignoreNULL = FALSE)


  treepoints <- eventReactive(input$displaytrees,{
   v <- rbind(datatree,addresspoints())
   points2 <- data.frame(v)
   return(points2)
  })
  
  
  output$mymap <- renderLeaflet({
    
    observeEvent(input$search, {
      v<- addresspoints()
      assign("v",addresspoints(), envir=.GlobalEnv)
    })
    
    leaflet() %>%
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE)
      ) %>%
      #addMarkers(data=v, label = input$address) 
      addMarkers(data=addresspoints(), label = input$address) %>%
      addMarkers(data=treepoints(), label = input$addres)
  })

}

shinyApp(ui, server)
