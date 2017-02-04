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

#Estimating sentiment A
opinion.lexicon.pos=scan('/home/cris/mrcrstnherediagmez@gmail.com/Countwords/data/opinion-lexicon-English/positive-words.txt',what = 'character',comment.char = ';')
opinion.lexicon.neg=scan('/home/cris/mrcrstnherediagmez@gmail.com/Countwords/data/opinion-lexicon-English/negative-words.txt',what = 'character',comment.char = ';')

#we can add other terms (based on our reqeriments)
pos.words=c(opinion.lexicon.pos,'upgrade')
neg.words=c(opinion.lexicon.neg,'wait','waiting','wtf','cancellation')


#now we create a function to score a sentiment (computes raw sentiment based on simple matching algorithm)
getSentimentScore=function(sentences,words.positive,words.negative){
  require(plyr)
  require(stringr)
  
  scores=sapply(sentences,function(sentence,words.positive,words.negative){
    #remove digits,punc and control chars
    sentence=gsub('[[:cntrl:]]','',gsub('[[:punct:]]','',gsub('\\d+','',sentence)))
    #convert all to lower case
    sentence=tolower(sentence)
    #split each sentence by space delimiter
    words=unlist(strsplit(sentence,'\\s+'))
    #get the boolean match of each words with the positive and negative opinion lexicon
    pos.matches=!is.na(match(words,words.positive))
    neg.matches=!is.na(match(words,words.negative))
    #get the score as total positive sentiment minus the total negatives
    score=sum(pos.matches)-sum(neg.matches)
    
    return(score)
  },words.positive,words.negative)
  
  #return data frame with respective sentence and score
  return(data.frame(text=sentences,score=scores))
}

#apply preceding to each corpus
MeruRes=getSentimentScore(Meru_tweetsCleaned,pos.words,neg.words)
OlaRes=getSentimentScore(Ola_tweetsCleaned,pos.words,neg.words)
TaxiForSureRes=getSentimentScore(TaxiForSure_tweetsCleaned,pos.words,neg.words)
UberRes=getSentimentScore(Uber_tweetsCleaned,pos.words,neg.words)

#since not all samples are the same size, lets compute mean and sd
MeruMean=mean(MeruRes$score)
OlaMean=mean(OlaRes$score)
TaxiForSureMean=mean(TaxiForSureRes$score)
UberMean=mean(UberRes$score)

MeruSd=sd(MeruRes$score)
OlaSd=sd(OlaRes$score)
TaxiForSureSd=sd(TaxiForSureRes$score)
UberSd=sd(UberRes$score)