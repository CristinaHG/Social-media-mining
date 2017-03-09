#facebook mining

install.packages('Rfacebook')
library(Rfacebook)

#generating short lasting token
source('/home/cris/mrcrstnherediagmez@gmail.com/Countwords/autenthicateFB.R')

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


# Influencers based on a single post:
# get the ID of most recent post
post_id<-head(page$id,n=1)
# get 1000 comments and likes from that post
post<- getPost(post_id,token,n=1000,likes=TRUE,comments = TRUE)
# display two first comments
head(post$comments,n=2)
# copy all comments for that post
samppost<-post$comments

library(sqldf)
# copy commments of that post
comments<-post$comments
# check who is the user with maximun likes with a query
influentialusers<-sqldf("select from_name, sum(likes_count) as totlikes from comments group by from_name")
head(influentialusers)
influentialusers$totlikes<-as.numeric(influentialusers$totlikes)
# sort uers based on the number of likes they have
top<-influentialusers[order(- influentialusers$totlikes), ]
head(top)


# Finding influencers in FB based on multiple post

# get the top 100 post from the page
post_id<-head(page$id,n=100)
head(post_id)
# Convert to matrix to make it easier to access the comments based on their position
post_id<-as.matrix(post_id)
# initialize data frame as null
allcomments<-""

# collect comments from all the 100 post
for(i in 1:nrow(post_id)){
  # get upto 1000 comments for each post
  post<-getPost(post_id[i,],token,n=1000,likes=TRUE,comments = TRUE)
  comments<-post$comments
  # append the comments to a single df
  allcomments<-rbind(allcomments,comments)
}

#consolidating like for each user
influentialusers<-sqldf("select from_name,sum(likes_count) as totlikes from allcomments group by from_name")
influentialusers$totlikes<-as.numeric(influentialusers$totlikes)
top<-influentialusers[order(-influentialusers$totlikes),]
head(top)

# elbaulP<-getPage("elbauldelprogramador",token,n=500)

# Measuring CTR performance for a page
format.facebook.date<-function(datestring){
  date<-as.POSIXct(datestring,format="%Y-%m-%dT%H:%M: %S+0000",tz="GMT")
}

# aggregate data on a monthly basis
aggregate.metric<-function(metric){
  m<-aggregate(page[[paste0(metric, "_count")]],
               list(month=page$month),
               mean)
  m$month<-as.Date(paste0(m$month,"-15"))
  m$metric<-metric
  return(m)
}

# extract post from page
page<-getPage("elbauldelprogramador",token,n=500)
page$datetime<-format.facebook.date(page$created_time)
page$month<-format(page$datetime,"%Y-%m")

# aggreate number of likes, comments and shares on monthly basis
df.list<-lapply(c("likes","comments","shares"),aggregate.metric)
df<-do.call(rbind,df.list)

library(ggplot2)
library(scales)
ggplot(df,aes(x = month, y = x,group = metric)) +
  geom_line(aes(color = metric)) +scale_x_date(date_breaks = "1 months",labels=date_format("%Y-%m"))+
theme_bw()+
  ylab("mean per post")+
  xlab("months/year")+
  ggtitle("Facebook page performance for The programmer chest")
#ggsave()




