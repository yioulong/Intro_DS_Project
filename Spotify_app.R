library(shiny)
library(shinythemes)
library(plotrix)
library(Rspotify)

options(httr_oauth_cache=T)
keys <- spotifyOAuth("spotify plot","a46cde2d9ace45caad073785d9eea36c","b65d781fc6034b0688ed26b557ad851e")

user <- getUser("Ukshin",token=keys)


Artist_info = function(x){
    
   search_artist = searchArtist(x ,token=keys)
    
    Artist = getArtist(search_artist$id[1], token=keys)
    
    Artist
    
}


#ninho<-searchArtist("ninho",token=keys)
#ninho$id[1]
#Xpath ://*[@id="main"]/div/div[3]/div[4]/div[1]/div/div[2]/section[1]/div[1]/div[3]
#https://open.spotify.com/artist/6Te49r3A6f5BiIgBRxH7FH
#avec web scraping afficher l'image de ninho

ui <- fluidPage(
    theme = shinytheme("darkly"),
    titlePanel("Music Match"),
    
    sidebarLayout(
        sidebarPanel(
            textInput("artistA", "I am looking for the most ", value = "", placeholder = "Artist Name"),
            actionButton("cast", "\n Compute simulation", icon = icon("calculator")),
        ),
        
        mainPanel(
            tableOutput("artist_info"),
            #plotOutput("barChart", height = 500)
        )
    )
)

server <- function(input, output) {
    #output$songResult <- renderText({matchSong()})
    
    cast <- eventReactive(input$cast, {#when i clic smth happen on input$cast see UI
        
        Artist_info(input$artistA)
    })
    
     output$artist_info <- renderTable({
         
         cast()
     }
     ) #INCOMPLETE
}
shinyApp(ui, server)

