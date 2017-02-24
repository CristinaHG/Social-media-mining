#facebook mining

install.packages('Rfacebook')
library(Rfacebook)

#generating short lasting token
source('/home/cris/mrcrstnherediagmez@gmail.com/Countwords/autenthicate.R')

frieds<-getFriends(token,simplify = TRUE)
friends_data<-getUsers(frieds$id,token,private_info = TRUE)
table(friends_data$gender)

#language
table(substr(friends_data$locale,1,2))

#country
table(substr(friends_data$locale,4,5))

#relationShip status
table(friends_data$relationship_status)

#getting likes
likes<-getLikes(user = "me",token = token)

#friends<-getFQL("SELECT uid2 FROM friend WHERE uid1=me()",token = token)# deprecated
friends<-callAPI("https://graph.facebook.com/me?fields=friends",token)

network<-getNetwork(token,format = "adj.matrix")
require(igraph)
social_graph<-graph.adjacency(network)
layout<-layout.drl(social_graph,options = list(simmer.attraction=0))

plot(social_graph,vertex.size=15,vertex.color="green",vertex.label.cex=0.8,edge.arrow.size=0,edge.curved=TRUE,
     layout=layout.fruchterman.reingold)
    dev.copy(png,filename="/home/cris/mrcrstnherediagmez@gmail.com/Countwords/friendsGraph.png",width=600,height=600)
    dev.off()

# measure the net degree
degree(social_graph,v=V(social_graph),mode = c("all","out","in","total"),loops = TRUE,normalized = FALSE)
degree.distribution(social_graph,cumulative = FALSE)
    
#measure betweenness
betweenness(social_graph,v=V(social_graph),directed = TRUE,weights=NULL,nobigint=TRUE,normalized = FALSE)
    
# measure closeness
closeness(social_graph,vids = V(social_graph),mode=c("out","in","all","total"),weights = NULL,normalized = FALSE)

# clusters in network
is.connected(social_graph,mode=c("weak","strong"))
clusters(social_graph,mode=c("weak","strong"))

# communities
network_Community<-walktrap.community(social_graph)
modularity(network_Community)
plot(network_Community,social_graph,vertex.size=15,vertex.label.cex=0.7,edge.arrow.size=0,edge.curved=TRUE,layout=layout.fruchterman.reingold)
dev.copy(png,filename="/home/cris/mrcrstnherediagmez@gmail.com/Countwords/friendsGraphCommunities.png",width=600,height=600)
dev.off()


# Facebook pages
page<-getPage("TED",token,n=500)
# lets get the detail about the post which had the maximum number of likes
page[which.max(page$likes_count),]
# get the one posted after dicember 2016
pageRecent<-page[which(page$created_time> "2017-01-01"),]
top<-pageRecent[order(- pageRecent$likes_count),]
head(top,n=10)
