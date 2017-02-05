#load the necessary packages
source('/home/cris/mrcrstnherediagmez@gmail.com/Countwords/authenticate.R')

Meru_tweets=searchTwitter("MeruCabs",n=2000,lan="en")
Ola_tweets=searchTwitter("OlaCabs",n=2000,lan="en")
TaxiForSure_tweets=searchTwitter("TaxiForSure",n=2000,lan="en")
Uber_tweets=searchTwitter("Uber_Delhi",n=2000,lan="en")

# check length
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

# installing packages from source
install.packages("/home/cris/Descargas/Rstem_0.4-1.tar.gz",repos=NULL,type="source")
install.packages("/home/cris/Descargas/sentiment_0.2.tar.gz",repos=NULL,type="source")

# lets build bayes classifier 
library(sentiment)
# classify emotion function returns an object of class data frame with seven columns (anger,disgust,fear,joy,sadness,
# surprise,best_fit) and one row for each document
MeruTweetsClassEmo=classify_emotion(Meru_tweetsCleaned,algorithm = "bayes",prior = 1)
OlaTweetsClassEmo=classify_emotion(Ola_tweetsCleaned,algorithm = "bayes",prior = 1)
TaxiForSureTweetsClassEmo=classify_emotion(TaxiForSure_tweetsCleaned,algorithm = "bayes",prior=1)
Uber_tweetsClassEmo=classify_emotion(Uber_tweetsCleaned,algorithm = "bayes",prior=1)

#we fetch emotion category best_fit for our analysis purposes
MeruEmotion=MeruTweetsClassEmo[,7]
OlaEmotion=OlaTweetsClassEmo[,7]
TaxiForSureEmotion=TaxiForSureTweetsClassEmo[,7]
UberEmotion=Uber_tweetsClassEmo[,7]

#substitute NAs by "unknown"
MeruEmotion[is.na(MeruEmotion)]="unknown"
OlaEmotion[is.na(OlaEmotion)]="unknown"
TaxiForSureEmotion[is.na(TaxiForSureEmotion)]="unknown"
UberEmotion[is.na(UberEmotion)]="unknown"

