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

# add column with names to the likes
friend1$user<-friends$name[1] # not aviable 
friend2$user<-friends$name[2]
friend3$user<-friends$name[3]
friend4$user<-friends$name[4]
friend5$user<-friends$name[5]
friend6$user<-friends$name[6] # not aviable 

# combine friends likes using rbind
friendlikedata<-rbind(friend2,friend3,friend4,friend5)

# select just the username and pages they like
forRecc<-friendlikedata[,c("user","id")]
write.csv(forRecc, "/home/cris/mrcrstnherediagmez@gmail.com/Countwords/forRecc.csv",row.names = FALSE,col.names = NA)

# install arules
install.packages("arules")
library(arules)

# read the data
data<-read.transactions("/home/cris/mrcrstnherediagmez@gmail.com/Countwords/forRecc.csv",rm.duplicates = FALSE,format ="single",sep = ",",cols = c(1,2))
# inspect data (users and pages it likes)
inspect(data)

