library(Rfacebook) # load library for mining FB
load("fb_oauth")

me<-getUsers("1882711358640845",token,private_info = TRUE)
friends<-getFriends(token,simplify = TRUE)
friend1<-getLikes("10208744691245840",n=100,token) # not aviable 
friend2<-getLikes("987052667996445",n=100,token)
friend3<-getLikes("773640712719649",n=100,token)
friend4<-getLikes("745947288828056",n=100,token)
friend5<-getLikes("562214867255590",n=100,token)
friend6<-getLikes("1697735810486543",n=100,token) # not aviable


