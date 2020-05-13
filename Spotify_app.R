library(shiny)
library(shinythemes)
library(plotrix)
library(Rspotify)
library(httr)
library(plyr)
library(dplyr)
library(jsonlite)

options(httr_oauth_cache=T)
keys <- spotifyOAuth("spotify plot","a46cde2d9ace45caad073785d9eea36c","b65d781fc6034b0688ed26b557ad851e")

user <- getUser("Ukshin",token=keys)


Preview_url = function(y){
    search_artist = searchArtist(y ,token=keys)
    req<-httr::content(httr::GET(
        paste0("https://api.spotify.com/v1/artists/",search_artist$id[1],
               "/top-tracks?country=","US"),
        httr::config(token = keys)))
    
    if(is.null(req[[1]][[1]]$preview_url) != TRUE && is.null(req[[1]][[2]]$preview_url) != TRUE && is.null(req[[1]][[3]]$preview_url) != TRUE && is.null(req[[1]][[4]]$preview_url) != TRUE && is.null(req[[1]][[5]]$preview_url) != TRUE && is.null(req[[1]][[6]]$preview_url) != TRUE && is.null(req[[1]][[7]]$preview_url) != TRUE && is.null(req[[1]][[8]]$preview_url) != TRUE && is.null(req[[1]][[9]]$preview_url) != TRUE && is.null(req[[1]][[10]]$preview_url) != TRUE){ 
        return(
            dplyr::bind_rows(
                lapply(req$tracks, function(x)
                    data.frame(id = x$id, name = x$name, preview_url = x$preview_url,
                               stringsAsFactors = F))))
    }else{
        test = matrix(0,1,3)
        test[1,1]="null"
        return(test)
    }
}

Compute_norm = function(x0, y0,x1,y1){

    sqrt((x1-x0)^2 + (y1-y0)^2)
}
Artist_info = function(x){

    search_artist = searchArtist(x ,token=keys)

    Artist = getArtist(search_artist$id[1], token=keys)

    out <- list(Name = Artist$name,Popularity = Artist$popularity,Followers = Artist$followers,Genres= Artist$genres)
    out

}
image_link = function(x){
    search_artist = searchArtist(x ,token=keys)
    req <- httr::GET(paste0("https://api.spotify.com/v1/artists/",search_artist$id[1]),httr::config(token = keys))
    json1<-httr::content(req)
    image = json1$images
    return (image[[1]]$url)
}

related_artist = function(x){

    search_artist = searchArtist(x ,token=keys)

    req <- httr::GET(paste0("https://api.spotify.com/v1/artists/",search_artist$id[1],"/related-artists"),httr::config(token = keys))
    json1<-httr::content(req)
    M <- lapply(json1$artists, "[",                      # commence par [  et fini par ce qui ya dans c(, , , , )
                c("name", "id", "popularity", "type" ))
    N <- lapply(json1$artists, "[[", "followers")
    N <- lapply(N, "[", "total")

    relatedArtists = plyr::ldply(M, data.frame)
    relatedArtists$followers = plyr::ldply(N, data.frame)$total
    out <- list(name = relatedArtists$name[], popularity = relatedArtists$popularity[])
    out
}

plot_relation =function(x){

    Artist_relation = related_artist(x)
    search_artist = searchArtist(x,token=keys)

    plot(NA, xlim = c(-100,100), ylim = c(-100,100), main =paste0(search_artist$artist[1]," Relation"), xlab = "", ylab = "",xaxt='n',yaxt='n')


    N = 20
    Mat = matrix(0, N, 2)
    for(i in 1:N){

        angle = runif(1,0,2*pi)
        step = i*5

        
        Mat[i, ] = Mat[i, ] + step*c(cos(angle),sin(angle))

        lines(c(0, Mat[i,1]), c(0, Mat[i,2]), col = 1)



    }
    for(i in 1:N){
        draw.circle(Mat[i,1],Mat[i,2],(Artist_relation$popularity[i])/10,nv = 1000,border="black",lty=1,lwd=1, col=rgb(30,215,96,maxColorValue = 255))

        text(Mat[i,1],Mat[i,2],labels = Artist_relation$name[i], col = 1, cex = 0.5)
    }

    draw.circle(0,0,(search_artist$popularity[1])/10,nv = 1000,border="black",lty=1,lwd=1, col=rgb(30,215,96,maxColorValue = 255))

    text(0,0,labels = search_artist$artist[1], col = 1, cex = 0.5) 
}

plot_artist = function(x){
    search_artist = searchArtist(x,token=keys)

    top_track = getTopTracks(search_artist$id[1],"US", token=keys)
    N = nrow(top_track)
    danc_ener = matrix(0, N, 4)
    for(i in 1:N){

        feature_track = getFeatures(top_track$artist_id[i], token=keys)
        danc_ener[i,1] = feature_track$id
        danc_ener[i,2] = top_track$name[i]
        danc_ener[i,3] = feature_track$danceability
        danc_ener[i,4] = feature_track$energy
    }

    plot(NA, xlim = c(0,1), ylim = c(0,1), main =paste0("The danceability & energy of ", search_artist$artist[1], "'s top 10 tracks"), xlab = "Danceability", ylab = "Energy")

    for(i in 1:N){
        points(danc_ener[i,3], danc_ener[i,4], pch = 19, col = i)
    }
    legend(0,1 , legend=c(danc_ener[1:N ,2]),
           col=c(1:N), pch = 19, cex=0.8,
           title="Track", text.font=4, bg='lightblue')
}
ui <- fluidPage(
    theme = shinytheme("darkly"),
    titlePanel("Artist Relation & Music Analysis"),

    sidebarLayout(
        sidebarPanel(
            textInput("artistA", "Tell me more about ", value = "", placeholder = "My Favourite Artist"),
            actionButton("cast", "\n Search", icon = icon("search")),
            selectInput("Artist_select", "Select Artist", choices = NULL),
            actionButton("second_cast", "\n Explore", icon = icon("calculator")),
        ),

        mainPanel(

            tabsetPanel(
                
                tabPanel("App Explication",textOutput("para1"), textOutput("break1"),
                         textOutput("para2"), textOutput("break2"),
                         textOutput("para3_1"), textOutput("para3_2"), textOutput("para3_3"), textOutput("para3_4"), textOutput("break3"),
                         textOutput("para4"), textOutput("break4"),
                         textOutput("para5_1"), textOutput("para5_2"), textOutput("para5_3"), textOutput("break5"),
                         textOutput("para6")),
                tabPanel("Artist Information",tableOutput("artist_info"), uiOutput("image1")),
                tabPanel("Related Artists Plot" ,plotOutput("Relation_plot")),
                tabPanel("Track Analysis",
                         plotOutput("plot_track",click = "plot_click"),
                         uiOutput("prewiew"),tableOutput("test"))
            )
        )
    ))

server <- function(input, output, session) {


    observeEvent(input$cast, {
        search_artist = searchArtist(input$artistA,token=keys)
        updateSelectInput(session, "Artist_select", choices = search_artist$artist)
    })

    #Give Artist basic Information
    cast <- eventReactive(input$second_cast, {#when i clic smth happen on input$cast see UI

        Artist_info(input$Artist_select)

    })

    output$artist_info <- renderTable({

        cast()
    }
    )
    #Give Image of the Artist
    cast1 <- eventReactive(input$second_cast, {

        Ima = image_link(input$Artist_select)
        tags$img(src=Ima, height = "200px")
    })


    output$image1 = renderUI({
        cast1()
    })
    #Top track preview#################################################################################
    cast4 <- eventReactive(input$plot_click, {

        Pre = Preview_url(input$Artist_select)
        if(Pre[1,1] != "null"){ 
            search_artist = searchArtist(input$Artist_select,token=keys)
            
            top_track = getTopTracks(search_artist$id[1],"US", token=keys)
            N = nrow(top_track)
            danc_ener_pos = matrix(0, N, 4)
            for(i in 1:N){
                
                feature_track = getFeatures(top_track$artist_id[i], token=keys)
                danc_ener_pos[i,1] = feature_track$id
                danc_ener_pos[i,2] = top_track$name[i]
                danc_ener_pos[i,3] = feature_track$danceability
                danc_ener_pos[i,4] = feature_track$energy
            }
            selec = 0
            for(i in 1:N){
                if(Compute_norm(input$plot_click$x, input$plot_click$y,as.numeric(danc_ener_pos[i,3]), as.numeric(danc_ener_pos[i,4])) < 0.05){
                    selec = i
                }
            }
            
            tags$audio(src = Pre$preview_url[selec], type = "audio/mp3", 
                       autoplay = FALSE, controls = TRUE) 
            
        } 

    })


    output$prewiew = renderUI({
        cast4()
    })
    ########################################################################################################

    #Give a plot with 20 most realeted artist
    cast2 <- eventReactive(input$second_cast, {

        plot_relation(input$Artist_select)
    })

    output$Relation_plot = renderPlot({

        cast2()

    }, height = 740, width = 720)

    #Give a plot of dancebility and engery of the 10 top's track of the artist
    cast3 <- eventReactive(input$second_cast, {

        plot_artist(input$Artist_select)
        Pre = Preview_url(input$Artist_select)
        if(Pre[1,1] == "null"){ 
            
        legend(0,0.5 , legend = "Preview URL for this artist are not avalaible!\n You can try for example: Dua lipa or Red Hot Chili Peppers",
               col= 2, cex=0.8,
               title="!Warning!\b", text.font=4, bg='lightblue',text.col = "red")}
    })

    output$plot_track = renderPlot({

        cast3()

    }, height = 740, width = 720)


    output$para1 <- renderText({paste0("Welcome to the one and only music app built by Ukshin Ukshini, Halim Jashari, Corrina Pascale and Yiou Long.")})
    output$break1 <- renderText({paste0("-----♬")})
    
    output$para2 <- renderText({paste0("We suggest you to use Google Chrome for optimal experience.")}) 
    output$break2 <- renderText({paste0("-----♬")})
    
    output$para3_1 <- renderText({paste0("Wanna know more about your favourite artist?")}) 
    output$para3_2 <- renderText({paste0("Type in his/her name and you can find out his/her basic information, relations with other artists and music analysis of his/her top 10 tracks!")})
    output$para3_3 <- renderText({paste0("Our system currently only supports European alphabets.")})
    output$para3_4 <- renderText({paste0("Non-European characters such as Chinese, Korean, Japanese and Thai may cause system shutdown and will be replaced by □ on the plot.")})
    output$break3 <- renderText({paste0("-----♬")})
    
    
    output$para4 <- renderText({paste0("For the Related Artists Plot, the distance between your Artist and the bubble of other artists represents the relatability, and the size of the bubble represents the artist's popularity.")})
    output$break4 <- renderText({paste0("-----♬")})
    
    output$para5_1 <- renderText({paste0("For the Track Analysis, we have his/her 10 tracks plotted on a diagram based on the danceability and energy level of each track.")})
    output$para5_2 <- renderText({paste0("Click on the dot to play it for 30s preview. ")})                                   
    output$para5_3 <- renderText({paste0("Unfortunately Spotify doesn't have url for every songs of every artists, so sometimes you might not be able to play the song, and an warning message will show up on your screen.")})    
    output$break5 <- renderText({paste0("-----♬")})
    
    output$para6 <- renderText({paste0("We hope you enjoy our app : )")})

}
shinyApp(ui, server)

