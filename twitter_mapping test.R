library(shiny)
library(leaflet)
library(twitteR)

shinyApp(
  ui = fluidPage(
    fluidRow(
      column(4, textInput("keyword", label = "search:", value = "")),
      column(4, textInput("lat", label = "lat:", value = 8.59)),
      column(4, textInput("long", label = "long:", value = -71.14)),
      column(8, leafletOutput("myMap")),
      column(12, tableOutput('table'))
    )
  ),
  server = function(input, output) {
    
    # OAuth authentication
    consumer_key <- readLines("tokens.txt")[1]
    consumer_secret <- readLines("tokens.txt")[2]
    access_token <- readLines("tokens.txt")[3]
    access_secret <- readLines("tokens.txt")[4]
    options(httr_oauth_cache = TRUE) # enable using a local file to cache OAuth access credentials between R sessions
    setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)
    
        
    setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)
    
    # Issue search query to Twitter
    dataInput <- reactive({  
      tweets <- twListToDF(searchTwitter(input$keyword, n = 100, lang="es",since='2016-01-01',
                                         geocode = paste0(input$lat, ",", input$long, ",10km"))) 
      tweets$created <- as.character(tweets$created)
      tweets <- tweets[!is.na(tweets[, "longitude"]), ]
    })
    
    # Create a reactive leaflet map
    mapTweets <- reactive({
      map = leaflet() %>% addTiles() %>%
        addMarkers(dataInput()$longitude, dataInput()$latitude, popup = dataInput()$screenName) %>%
        setView(input$long, input$lat, zoom = 11)
    })
    output$myMap = renderLeaflet(mapTweets())
    
    # Create a reactive table 
    output$table <- renderTable(
      dataInput()[, c("text", "screenName", "longitude", "latitude", "created")]
    )
  }
)