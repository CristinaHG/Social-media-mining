require(devtools)
install.packages('jsonlite')
install_github("cscheid/rgithub")

# load libraries
library(github)
library(jsonlite)
library(stringr)
library(ggplot2)
library(reshape2)
library(sqldf)
library(data.table)
library(zoo)
library(github)

# get your followers
myFollowers<-get.my.followers(context)

# get number of my followers
numFollowing<-length(myFollowers$content)

#create a dataset with your followers
dataset<-unlist(myFollowers$content[[1]])

for(i in 2:length(myFollowers$content)){
  dataset<-rbind(dataset,unlist(myFollowers$content[[i]]))
}

# create a data frame and save it for later use
dataset<-unname(dataset)
dataset<-as.data.frame(dataset)
colnames(dataset)<-c(names(myFollowers$content[[1]]))
write.csv(dataset,file="CrisFollowers.csv")

