#Finding trending topics

install.packages(c("devtools","rjson","bit64","httr"))

#load libraries
library(devtools)
library(twitteR)
library(tm)
library(wordcloud)

# get autorization
api_key="YBsbh0WkchdhsKONtGYvUhRi1"
api_secret="pBlNqKOi1VzFOc9nRgZpzwO4lPWWnzHbgXHng5M9DpEnqFwztq"
acces_token="1004279869-qdzCaot4PUuBcFxW3ejjeGqKL2luuWBF1gZJAmd"
acces_token_secret="2fyesoH83Gw4tXwSCvQSgI2B74eFdHtfxO3U2y5y187lM"
setup_twitter_oauth(api_key,api_secret,acces_token,acces_token_secret )
#tweets=userTimeline("_musicalnote",2000)

Locs=availableTrendLocations()
locsEngland=subset(Locs,country="United Kingdom")
id=subset(locsEngland,name=="Liverpool")
trends=getTrends(woeid = id)
trends=Corpus(VectorSource(trends$name))
trends=TermDocumentMatrix(trends)
ap.tdm=as.matrix(trends)
ap.v=sort(rowSums(ap.tdm),decreasing = T)
ap.d=data.frame(word=names(ap.v),freq=ap.v)
table(ap.d$freq)
#tdm=as.matrix(trends)
wordcloud(ap.d$word,ap.d$freq, scale=c(3,0.5), max.words=50, random.order=FALSE, rot.per=0.15, use.r.layout=FALSE, colors=brewer.pal(,"Dark2"))
dev.off()
