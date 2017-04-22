require(devtools)
#install.packages('jsonlite')
#install_github("cscheid/rgithub")

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

# create a dataset with your followers
dataset<-unlist(myFollowers$content[[1]])

for(i in 2:length(myFollowers$content)){
  dataset<-rbind(dataset,unlist(myFollowers$content[[i]]))
}

# create a data frame and save it for later use
dataset<-unname(dataset)
dataset<-as.data.frame(dataset)
colnames(dataset)<-c(names(myFollowers$content[[1]]))
write.csv(dataset,file="CrisFollowers.csv")

# read latest created csv 
myFriends<-read.csv("CrisFollowers.csv")

# extract the names
unname<-as.character(myFriends$login)     

# extract data from friend's public repositories   
compdata<- NULL

for(i in 1:nrow(myFriends)){
  data<-fromJSON(paste0("https://api.github.com/users/",str_trim(unname[i],side = "both"),"/repos?clientID&clientSecret"))
  if(length(data)!=0){
    data<-data[,-4]
    compdata<-rbind(compdata,data)
  }
  print(i)
}

# write data for reuse
write.csv(compdata,"UsersWithRepoInfo.csv")

# load data
activeFriends<-read.csv("UsersWithRepoInfo.csv")

# make date format supported by R
date.format<-function(datestring){
  date<-as.POSIXct(datestring,format="%Y-%m-%dT%H:%M:%SZ",tz="GMT")
}

# update dates with new format
activeFriends$created_at<-date.format(activeFriends$created_at)
activeFriends$updated_at<-date.format(activeFriends$updated_at)
activeFriends$pushed_at<-date.format(activeFriends$pushed_at)

# selecting just the interesting cols
activeFriends<-activeFriends[,c("id","name","full_name","private","description","fork","created_at","updated_at","pushed_at","homepage","size","stargazers_count","watchers_count","language",
                             "has_issues","has_downloads","forks_count","open_issues_count","forks","open_issues","watchers")]

# replace T/F values by 0/1
activeFriends$private<-as.integer(activeFriends$private)
activeFriends$has_issues<-as.integer(activeFriends$has_issues)
activeFriends$fork<-as.integer(activeFriends$fork)
activeFriends$has_downloads<-as.integer(activeFriends$has_downloads)

# Getting the username
activeFriends$full_name<-unlist(lapply(strsplit(as.character(activeFriends$full_name),split = '/',fixed = TRUE),function(x) (x[1])))
