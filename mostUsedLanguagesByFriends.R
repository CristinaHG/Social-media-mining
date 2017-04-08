require(devtools)
install_github("cscheid/rgithub")
library(github)

# get my followers
myFollowers<-get.my.followers(ctx)
# get number of mmy followers
numFollowing<-length(myFollowers$content)

#creating a dataset with my followers
 dataset<-unlist(myFollowers$content[[1]])
 
 for(i in 2:length(myFollowers$content)){
   dataset<-rbind(dataset,unlist(myFollowers$content[[i]]))
 }
 
# myFollowers$content<-unname(myFollowers$content)
# newDataSet<-sapply(1:length(myFollowers$content), function(i){
#   (myFollowers$content[[i]])
# })
 
 dataset<-unname(dataset)
 dataset<-as.data.frame(dataset)
 colnames(dataset)<-c(names(myFollowers$content[[1]]))
 write.csv(dataset,file="CrisFollowers.csv")
 
 # read the username column
 myFriends<-read.csv("CrisFollowers.csv")
 unname<-as.character(myFriends$login)     
   
 # extract data from friend's public repositories   
 library(jsonlite)
 library(stringr)
 compdata<- ""
 
 for(i in 1:nrow(myFriends)){
   data2<-fromJSON(paste0("https://api.github.com/users/",str_trim(unname[i],side = "both"),"/repos?client_id&client_secret"))
   if(length(data2)!=0){
    data2<-data2[,-4]
    compdata<-rbind(compdata,data2)
   }
   print(i)
 }
 
 write.csv(compdata,"ActiveUsers.csv")
 
 activeusers<-read.csv("ActiveUsers.csv")
 
 # make date format supported by R
 date.format<-function(datestring){
   date<-as.POSIXct(datestring,format="%Y-%m-%dT%H:%M:%SZ",tz="GMT")
 }
 
 #update dates with new format
 activeusers$created_at<-date.format(activeusers$created_at)
 activeusers$updated_at<-date.format(activeusers$updated_at)
 activeusers$pushed_at<-date.format(activeusers$pushed_at)
 
 
 