library(Rspotify)

keys <- spotifyOAuth("spotify plot","a46cde2d9ace45caad073785d9eea36c","b65d781fc6034b0688ed26b557ad851e")

user <- getUser("Ukshin",token=keys)
user$display_name # user name
user$id

user$followers
#dashbord creer app 
#client id et secret id
#dans edit mettre le lien de tiago --> https://github.com/tiagomendesdantas/Rspotify

