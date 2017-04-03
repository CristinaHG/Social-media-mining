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
 