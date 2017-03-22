library(devtools)
install_github("pablobarbera/instaR/instaR")
library(instaR)

app_id<-"30db8d77782646cebb09ff8ab35e7fde"
app_secret<-"e49fd92848f5421fa3cbd4677c144e01"
token<-instaOAuth(app_id,app_secret)

#searching public media for a specific hashtag
MachuPicchu<-searchInstagram("MachuPicchu",token,lat=13.1633,lng=72.5456,n=10,folder="Granada")
instag<-getUserMedia("_musicalnote",token,n=100,folder = "instagram")
me<-getUser("mmusicalnote",token)
