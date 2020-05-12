library(shiny)
library(shinythemes)
library(plotrix)
library(charlie86/spotifyr)

# Spotify Authentication
id <- "5f5f8357fca8446cb7ce3aac2d603161"
secret <- "741217b2f1e747028d8ce6e0b98913d3"
Sys.setenv(SPOTIFY_CLIENT_ID = id)
Sys.setenv(SPOTIFY_CLIENT_SECRET = secret)
access_token <- get_spotify_access_token()
# Someone help cos I don't use Spotify ^

# This function is to convert our input to Artist Spotify ID so we can access its Spotify data
convert_to_ArtistID <- function(inputArtist) {
  artistName = inputArtist
  HeaderValue = paste0('Bearer ', mytoken)1
  
  URI = paste0('https://api.spotify.com/v1/search?query=', artistName,'&offset=0&limit=20&type=artist')
  response2 = GET(url = URI, add_headers(Authorization = HeaderValue))
  Artist = content(response2)
  Artist$id
}
# I don't have spotify so can't even test if the result is going to be an Artist Object
# Someone please help check
# If this function doesn't work, we have to build a function to get ArtistID from the input
# Basically a basic search function

matchSong <- function() {
 newA = convert_to_Artist(input$artistA)
 newB = convert_to_Artist(input$artistB)
 tenAsongs = get_artist_top_tracks(newA, market = "US",
                                   authorization = get_spotify_access_token(), #access token needed, again
                                   include_meta_info = FALSE)
 # ^ this returns an array of 10 songs, according to Spotify
 # v obtain attributes from each song
 for (song in tenAsongs) {
   get_track_audio_features(song$id, authorization = get_spotify_access_token())
   # ^ this returns one data frame for each song, accroding to Spotify
 }
 # ^ now we have 10 data frames 
 # need to have the mean() of the same attributes of 10 data frames
 # something to do with "plyr", FILL UP LATER
 # in the end the Result should be one data frame merged from the 10 data frame
 # IMPORTANT & IMCOMPLETE
 
 # now use the one data frame to find a perfect match in newB's songs, "Plyr"again probably 
}



ui <- fluidPage(
  theme = shinytheme("darkly"),
  titlePanel("Music Match"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("artistA", "I am looking for the most ", value = "", placeholder = "Artist Name"),
      textInput("artistB", " song in ", value = "", placeholder = "Artist Name"),
    ),
    
    mainPanel(
      textOutput("songResult"),
      plotOutput("barChart", height = 500)
    )
  )
)

server <- function(input, output) {
  output$songResult <- renderText({matchSong()})
  output$barChart <- renderPlot() #INCOMPLETE
}
shinyApp(ui, server)

