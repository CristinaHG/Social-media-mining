#load the necessary packages
source('/home/cris/mrcrstnherediagmez@gmail.com/Countwords/authenticate.R')

Meru_tweets=searchTwitter("MeruCabs",n=2000,lan="en")
Ola_tweets=searchTwitter("OlaCabs",n=2000,lan="en")
TaxiForSure_tweets=searchTwitter("TaxiForSure",n=2000,lan="en")
Uber_tweets=searchTwitter("Uber_Delhi",n=2000,lan="en")

#check length
length(Meru_tweets)
length(Ola_tweets)
length(TaxiForSure_tweets)
length(Uber_tweets)

#cleaning the corpus
Meru_tweets=sapply(Meru_tweets,function(x) x$getText())
Ola_tweets=sapply(Ola_tweets,function(x) x$getText())
TaxiForSure_tweets=sapply(TaxiForSure_tweets, function(x) x$getText())
Uber_tweets=sapply(Uber_tweets, function(x) x$getText())

catch.error=function(x){
  #missing value for test purpose
  y=NA
  #try to catch that error we just created
  catch_error=tryCatch(tolower(x),error=function(e) e)
  #if not an error
  if(!inherits(catch_error,"error"))
    y=tolower(x)
  #check result if error exist, otherwise the function works fine
  return(y)
}

cleanTweets=function(tweet){
  #clean the tweet for sentiment analysis
  #remove html links, which are not required for sentiment analysis
  tweet=gsub("(f|ht) (tp) (s?) (://) (.*) [.|/] (.*)"," ",tweet)
  
  #first we will remove RT entities from sotred tweets
  tweet=gsub("(RT|via) ((?:\\b\\W*@\\w+)+)"," ",tweet)
  
  #then remove all hashtag
  tweet=gsub("#\\w+"," ",tweet)
  
  #them remove @people
  tweet=gsub("@\\w+"," ",tweet)
  
  #then remove punctuation
  tweet=gsub("[[:punct:]]"," ",tweet)
  
  #then remove numbers, we only need text for analysis
  tweet=gsub("[[:digit:]]"," ",tweet)
  
  #finally remove unnecesary spaces,tabs etc
  tweet=gsub("[ \t]{2,}"," ",tweet)
  tweet=gsub("^\\s+|\\s+$"," ",tweet)
  
  
  #convert all the words to lower case(uniform pattern )
  tweet=catch.error(tweet)
  tweet
}


cleanTweetsAndRemoveNAs=function(Tweets){
 TweetsCleaned=sapply(Tweets,cleanTweets)
 
 #remove the Na tweeets from this tweet list
 TweetsCleaned=TweetsCleaned[!is.na(TweetsCleaned)]
 names(TweetsCleaned)=NULL
 
 #remove the repetitive tweets from this tweet list
 TweetsCleaned=unique(TweetsCleaned)
 TweetsCleaned
}

Meru_tweetsCleaned=cleanTweetsAndRemoveNAs(Meru_tweets)
Ola_tweetsCleaned=cleanTweetsAndRemoveNAs(Ola_tweets)
TaxiForSure_tweetsCleaned=cleanTweetsAndRemoveNAs(TaxiForSure_tweets)
Uber_tweetsCleaned=cleanTweetsAndRemoveNAs(Uber_tweets)

#size for cleaned lists
length(Meru_tweetsCleaned)
length(Ola_tweetsCleaned)
length(TaxiForSure_tweetsCleaned)
length(Uber_tweetsCleaned)

#Estimating sentiment A
opinion.lexicon.pos=scan('/home/cris/mrcrstnherediagmez@gmail.com/Countwords/data/opinion-lexicon-English/positive-words.txt',what = 'character',comment.char = ';')




