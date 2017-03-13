setwd('/home/cris/mrcrstnherediagmez@gmail.com/Countwords/') # set working directory
library(Rfacebook) # load library for mining FB
# generate short lasting token and authenticate
page<-getPage("beach4all",token,n=500) # download latest 500 FB post
post_id<-head(page$id,n=100) # get the 100 top post in page
head(post_id,n=10)

post_id<-as.matrix(post_id) # create matrix 
allcomments<- ""
for(i in 1:nrow(post_id)){
  post<-getPost(post_id[i,],token,n=1000,likes = TRUE,comments = TRUE)
  comments<-post$comments
  allcomments<-rbind(allcomments,comments)
}
# look for number of chars and urls in comments
allcomments<-as.data.frame(allcomments)
allcomments$chars<-""
allcomments$chars<-nchar(allcomments$message)
allcomments$url<-""
allcomments$url<-grepl(".com",allcomments$message)
allcomments$spam<-""

# create datasets
train<-allcomments[1:100,]
test<-allcomments[101:nrow(allcomments),]
write.csv(train,"comment-train.csv")
write.csv(test,"comment-test.csv")

train<-read.csv("comment-train.csv")
test<-read.csv("comment-test.csv")

spamUrls<- (which(train$url))
emptyMessages<- which(train$message=="")

for(i in spamUrls){
  train$spam[i]<-1
}

for(j in emptyMessages){
  train$spam[j]<-1
}


# train$spam<-lapply(seq_along(1:nrow(train)),function(x){
#  train[x,]
#  if(x %in% spamUrls){
#    train[x,]$spam<-1
#  }
# })



