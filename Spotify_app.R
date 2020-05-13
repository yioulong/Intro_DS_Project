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
    return(
        dplyr::bind_rows(
            lapply(req$tracks, function(x)
                data.frame(id = x$id, name = x$name, preview_url = x$preview_url,
                           stringsAsFactors = F))))
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

    text(0,0,labels = search_artist$artist[1], col = 1, cex = 0.5) # peut avoir un probleme si le nom Artist_relation$name pas correct
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

    plot(NA, xlim = c(0,1), ylim = c(0,1), main =paste0(search_artist$artist[1]," Top track's danceability & energy"), xlab = "Danceability", ylab = "energy")

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
            textInput("artistA", "I am looking for the most ", value = "", placeholder = "Artist Name"),
            actionButton("cast", "\n Search", icon = icon("search")),
            selectInput("Artist_select", "Select Artist", choices = NULL),
            actionButton("second_cast", "\n Compute simulation", icon = icon("calculator")),
        ),

        mainPanel(

            tabsetPanel(
                
                tabPanel("Explication",textOutput("App_info")),
                tabPanel("Description",tableOutput("artist_info"), uiOutput("image1")),
                tabPanel("Related Artists Plot" ,plotOutput("Relation_plot")),
                tabPanel("Danceability & Energy of Top's 10 Track",
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
            if(Compute_norm(input$plot_click$x, input$plot_click$y,as.numeric(danc_ener_pos[i,3]), as.numeric(danc_ener_pos[i,4])) < 0.01){
                selec = i
            }
        }

        tags$audio(src = Pre$preview_url[selec], type = "audio/mp3",
                   autoplay = FALSE, controls = TRUE)


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
    })

    output$plot_track = renderPlot({

        cast3()

    }, height = 740, width = 720)


    output$App_info <- renderText({
        paste0("blalalalal")
        #Related Artists Plot --> if the circle is big so the artist has a high popularity
                       #--> if the closer the circle is to the center the more it is linked to the artist

        #Danceability & Energy of Top's 10 Track
                      # -->  x = danc... y= energy
                        #points reprensent the top 10 track of the artist
        #             if you click on one of point you get the song preview
                        #(warning : for certain  artist spotify does not provide the preview Url so it doesn't work)


    })

}
shinyApp(ui, server)

