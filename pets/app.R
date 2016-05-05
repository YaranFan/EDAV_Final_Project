library(rsconnect)
library(shiny)
library(leaflet)

ui <- fluidPage(
  leafletOutput("mymap", width = 1280, height = 800)
)

server <- function(input, output, session) {
  ## Process file.
  complaints <- read.csv("complaints_details.csv", stringsAsFactors = F)
  names(complaints) <- c("details", "date", "street", "city", "zip", "lat", "lon", "animal1","animal2")
  
  ## Unique animals.
  complaints[complaints$animal1 == "snakes",]$animal1 <- "snake"
  complaints[complaints$animal1 %in% c("birds","bird","crows","crow"),]$animal1 <- "bird"
  complaints[complaints$animal1 %in% c("chickens","chicken","rooster","roosters"),]$animal1 <- "chicken"
  complaints[complaints$animal1 == "pigs",]$animal1 <- "pig"
  complaints[complaints$animal1 == "",]$animal1 <- "pet"
  
  ## Icon list.
  animalIcons <- iconList(
    bird = makeIcon("icons/crow.png", 18, 18),
    chicken = makeIcon("icons/chicken.png", 18, 18),
    pig = makeIcon("icons/pig.png", 25, 25),
    racoon = makeIcon("icons/racoon.png", 25, 25),
    snake = makeIcon("icons/snake.png", 18, 18),
    pet = makeIcon("icons/pet.png", 18, 18)
  )
  
  output$mymap <- renderLeaflet({
    leaflet(complaints) %>%
      addProviderTiles("Esri.WorldStreetMap",
                       options = providerTileOptions(noWrap = TRUE)
                       ) %>%
      addMarkers(popup = ~details, icon = ~animalIcons[animal1])
  })
}

shinyApp(ui, server)