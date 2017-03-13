setwd('/home/cris/mrcrstnherediagmez@gmail.com/Countwords/') # set working directory
library(Rfacebook) # load library for mining FB
# generate short lasting token and authenticate
page<-getPage("beach4all",token,n=500) # download latest 500 FB post
post_id<-head(page$id,n=100) # get the 100 top post in page
head(post_id,n=10)