# NYC Trees Visualization
# This is a Shiny web application. 
# Masters Project by Elona Zharri

library(shiny)
library(shinydashboard)
library(leaflet)
library(ggmap)
library(plyr)
library(rsconnect)

register_google(key = "AIzaSyCFANenvxHGw3HQphmyEXgaNBAfoqZe1wQ")
write = TRUE
trees <- read.csv("originaltrees.csv", stringsAsFactors = FALSE)

# Define dashboard

ui <- dashboardPage(
  skin = "green",
  dashboardHeader(title = "NYC Trees"),
  dashboardSidebar(
    width = 250,
    textInput(inputId = "address", label = "Enter your address:", placeholder = "e.g 66 Mary Street, Lodi, NJ, 07644"),
    actionButton(inputId = "search", label = "Search"),
    actionButton(inputId = "displaytrees", label = "Display Trees"),
    actionButton(inputId = "treesinfo", label = "Trees Info"),
    actionButton(inputId = "airquality", label = "Air Quality"),
    actionButton(inputId = "energy", label = "Energy")
  ),
  
  dashboardBody(
    
    fluidRow(
      column(width = 12,
      title = "My Map",
      box(width = NULL, solidHeader = TRUE,
          leafletOutput("mymap", height = 400)),
      box(title = "Trees Info", width = NULL,
          dataTableOutput("treesinfo"))
      )),
    
    fluidRow(
      column(width = 12,
             box(title = "Airquality",width = NULL,
                 plotOutput("airquality")),
             box(title="Energy",width = NULL,
                 uiOutput("energy"))  
      )
    )
  )
)

# Define server logic required to draw the map

server <- function(input, output, session) {
  
# Static aspect goes with leaflet()
output$mymap <- renderLeaflet({
  leaflet( ) %>%
    addTiles() %>%
    setView(-73.935242, 40.730610, 10)
})

# Use an observer to display the address recieved from the user
observeEvent(input$search,{
  
  icons <- awesomeIcons(
    icon = 'ios-close',
    iconColor = 'red',
    library = 'ion'
  )
  
  p <- c(input$address)
  q <- geocode(p)
  points1 <- data.frame (q)
  leafletProxy("mymap") %>%
    addAwesomeMarkers( data=points1, 
                popup = input$address,
                icon = icons)
}, ignoreNULL = FALSE)

# Use a separate observer to display the trees after receiving the user address
observeEvent(input$displaytrees,{
  p <- c(input$address)
  q <- geocode(p)
  points1 <- data.frame (q)
  addresstrees <- cbind(trees,points1)
  #names(addresstrees)<-c("latitude","longitude","lat","lon")
  #v <- rbind(trees,points1)
  #points2 <- data.frame(v)
  x <- addresstrees$latitude - addresstrees$lat
  x <- x*x
  y <- addresstrees$longitude - addresstrees$lon
  y <- y*y
  z <- x+y
  distance <- sqrt(z)
  distance
  finaltress <- cbind(addresstrees,distance)
  write.csv(finaltress, "FinalTrees.csv")
  names(finaltress)
  
  display <-subset(finaltress, distance < 0.001, select = c(latitude,longitude, health, status, spc_common, adressess))
  
  write.csv(display, "present5.csv")
  displayonly <- read.csv("present5.csv", stringsAsFactors = FALSE)
  data(displayonly)


   leafletProxy("mymap")%>%
      addMarkers(data=display)
})

# Display Trees Information
observeEvent(input$treesinfo,{
  info <- read.csv("present5.csv", stringsAsFactors = FALSE)
  output$treesinfo <- renderDataTable(info)
  })


# Display airquality
observeEvent(input$airquality,{
  info <- read.csv("present5.csv", stringsAsFactors = FALSE)
  output$airquality <-renderPlot({ 

    ggplot(info) + 
    aes(x= spc_common, fill = health ) + geom_bar()
  })
  
})

# Display how much energy is saved
observeEvent(input$energy,{
  
  output$energy <-renderText({ 
  df <- read.csv("FinalTrees.csv", stringsAsFactors = FALSE)
  dis <-subset(df, distance < 0.001 & tree_dbh >= 5, select = c( health, status, spc_common))
  if ( dis<0.001) {
  
  paste("We have filtered all your trees located around your address based on the tree distance and tree diameter to estimate the amount of the energy these trees will save.
        It resulted that the closed trees number, having a diameter bigger than 5 is: ",length(dis), ". The importance of these trees is being properly placed around your home 
        which can reduce your air conditioning needs by 30% and save 20% to 50% in heating costs, according to the USDA Forest Service. 
        The U.S. Department of Energy says three properly placed trees could save you $100 to $250 a year.", sep="\n")
  }else{
    
    paste("We have found ",length(dis), "trees properly placed around your home which can reduce your air conditioning needs
          by 15% in heating costs and could save you $10 monthly. Trees near your air conditioner create shades which keeps 
          your AC unit cool, helping it to run more efficiently.", sep= "\n")
  }
  })
  
})

}
# Run the application 
shinyApp(ui = ui, server = server)

