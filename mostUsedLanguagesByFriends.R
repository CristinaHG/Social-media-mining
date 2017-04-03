require(devtools)
install_github("cscheid/rgithub")
library(github)

# get my followers
myFollowers<-get.my.followers(ctx)
# get number of mmy followers
numFollowing<-length(myFollowers$content)

#creating a dataset with my followers
 dataset<-myFollowers$content[[1]]
 
 for(i in 2:length(myFollowers$content)){
   dataset<-rbind(dataset,myFollowers$content[[i]])
 }
 dataset<-unname(dataset)
 dataset<-as.data.frame(dataset)
 colnames(dataset)<-c(names(myFollowers$content[[1]]))
 