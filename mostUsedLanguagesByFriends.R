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
 
 # read the username column
 myFriends<-read.csv("CrisFollowers.csv")
 unname<-as.character(myFriends$login)     
   
 # extract data from friend's public repositories   
 library(jsonlite)
 library(stringr)
 compdata<- ""
 
 for(i in 1:nrow(myFriends)){
   data2<-fromJSON(paste0("https://api.github.com/users/",str_trim(unname[i],side = "both"),"/repos?client_id&client_secret"))
   if(length(data2)!=0){
    data2<-data2[,-4]
    compdata<-rbind(compdata,data2)
   }
   print(i)
 }
 
 write.csv(compdata,"ActiveUsers.csv")
 
 activeusers<-read.csv("ActiveUsers.csv")
 
 # make date format supported by R
 date.format<-function(datestring){
   date<-as.POSIXct(datestring,format="%Y-%m-%dT%H:%M:%SZ",tz="GMT")
 }
 
 # update dates with new format
 activeusers$created_at<-date.format(activeusers$created_at)
 activeusers$updated_at<-date.format(activeusers$updated_at)
 activeusers$pushed_at<-date.format(activeusers$pushed_at)
 
 # selecting just the interesting cols
 activesubset<-activeusers[,c("id","name","full_name","private","description","fork","created_at","updated_at","pushed_at","homepage","size","stargazers_count","watchers_count","language",
                              "has_issues","has_downloads","has_wiki","has_pages","forks_count","open_issues_count","forks","open_issues","watchers")]
 
 # replace T/F values by 0/1
 activesubset$private<-as.integer(activesubset$private)
 activesubset$fork<-as.integer(activesubset$fork)
 activesubset$has_downloads<-as.integer(activesubset$has_downloads)
 activesubset$has_wiki<-as.integer(activesubset$has_wiki)
 activesubset$has_pages<-as.integer(activesubset$has_pages)
 
 # Getting the username
 activesubset$full_name<-sapply(strsplit(as.character(activesubset$full_name),split = '/',fixed = TRUE),function(x) (x[1]))
 
 # flag for website owning
 activesubset$hasweb<-as.numeric(grepl(".",activesubset$homepage))
 
 # lenght for the description
 activesubset$desclen<-nchar(as.character(activesubset$description))
 
 # Day difference from current date from created,updated and pushed
 activesubset$daycreated<-as.integer(difftime(Sys.Date(),activesubset$created_at,units = c("days")))
 activesubset$dayupdated<-as.integer(difftime(Sys.Date(),activesubset$updated_at,units = c("days")))
 activesubset$daypushed<-as.integer(difftime(Sys.Date(),activesubset$pushed_at,units = c("days")))

  head(activesubset)
 
 newWithoutEmpty<-activesubset$language[-1]
 newWithoutEmpty<-na.omit(newWithoutEmpty)
 table(newWithoutEmpty)
 
 # plot graphycally
 library(ggplot2)
 q<-qplot(newWithoutEmpty,geom = "bar",xlab = "language", ylab="usage",fill=I("springgreen4"))
 q+theme(axis.text.x = element_text(angle = 90,hjust = 1)) +ggtitle("Programming languages used by my friends")+theme(plot.title = element_text(hjust = 0.5))
 
 
 # looking at watchers,forks,issues (univariate analysis)
 activesubset<-activesubset[-1,]
 boxplot(activesubset$watchers_count,outline = TRUE)
 boxplot(activesubset$forks_count,outline = TRUE)
 boxplot(activesubset$open_issues,outline = TRUE)
 boxplot(activesubset$stargazers_count,outline = TRUE)
 
 forbplot<-activesubset[c("watchers_count","forks_count","open_issues","stargazers_count")]
 boxplot(forbplot,outline = FALSE)
 
 #see how watchers trends across open_issues
 library(reshape2)
 colnames(activesubset)
 dat.m<-melt(activesubset,id.vars = 'open_issues_count',measure.vars = c('watchers_count'))
 library(ggplot2)
 p<-ggplot(dat.m)+
   geom_boxplot(aes(x=open_issues_count,y=value,color=variable),outlier.shape = NA,notch = TRUE,notchwidth = 0.5)+
   scale_y_continuous(limits = quantile(activesubset$stargazers_count,c(0,0.6)))
 
 # how many repos had issues
 pie<-ggplot(activesubset,aes(x=factor(1),fill=factor(activesubset$has_issues)))+geom_bar(width = 1)
 pie+coord_polar(theta = "y")
 ggsave(file="/home/cris/mrcrstnherediagmez@gmail.com/Countwords/issues_chart.png") 
 
 # looking for the trend on updating repos
 trendingdata<-activesubset[c("updated_at","id")]
 trendingdata$updated_at<-as.POSIXct(strptime(trendingdata$updated_at,"%Y-%m-%d")) 
 tabledata<-table(trendingdata$updated_at) 
 tabledata<-as.data.frame(tabledata)
 colnames(tabledata)<-c("Date","Repositories")
 tabledata$Date<-as.Date(tabledata$Date)
 tabledata<-tail(tabledata,100) 
 
 # plot
 q<-ggplot(data=tabledata,aes(x=Date,y=Repositories,group=1))+geom_line()+geom_point()
 q+theme(axis.text.x = element_text(angle = 90,hjust = 1))
 ggsave(file="/home/cris/mrcrstnherediagmez@gmail.com/Countwords/pdating_trending.png") 
 
 
 # comparing users with a heat map
 colnames(activesubset)
 library(sqldf)
 newdata<-activesubset[c("id","full_name","size","watchers_count","forks_count","open_issues_count","desclen","daycreated","dayupdated","daypushed")]
 sd<-sqldf("select full_name, count(id), avg(size), sum(watchers_count), count(forks_count), count(open_issues_count), avg(desclen), avg(daycreated), avg(dayupdated),avg(daypushed) from newdata group by full_name")
 
 # give descriptive colnames
 colnames(sd)<-c("Name","Repositories","AvgSize","Watchers","Forks","Issues","Avg_descriptionLenght","Avg_daysSinceCreated","Avg_daysSinceUpdated","Avg_daysSincePushed")
 row.names(sd)<-sd$Name
 # just take the numeric
 sd<-sd[,2:10]
 sd<-as.matrix(sd)
 heat.map<-heatmap(sd,Rowv = NA,Colv = NA,col=cm.colors(256),scale = "column",margins=c(11,8))
 dev.copy(png,filename="/home/cris/mrcrstnherediagmez@gmail.com/Countwords/friendsHeatmap.png",width=600,height=875)

 
 # correlation analysis
 library(ggplot2)
 cor(sd$Forks,sd$Watchers)
 ggplot(sd,aes(x=Forks,y=Watchers))+
   geom_point(shape=1)+
   geom_smooth(method = lm)
 
 cor(sd$Avg_daysSinceCreated,sd$Avg_daysSincePushed)
 ggplot(sd,aes(x=Avg_daysSinceCreated,y=Avg_daysSincePushed))+
   geom_point(shape=1)+
   geom_smooth(method = lm)
 
 # using loess method(by default)
 ggplot(sd,aes(x=Avg_daysSinceCreated,y=Avg_daysSincePushed))+
   geom_point(shape=1)+
   geom_smooth()
 
 
 # correlatin on segmented data
 newdataS<-activesubset[c("id","full_name","size","watchers_count","forks_count","open_issues_count","desclen","daycreated","dayupdated","daypushed","has_issues")]
 sdS<-sqldf("select full_name, count(id), avg(size), sum(watchers_count), count(forks_count), count(open_issues_count), avg(desclen), avg(daycreated), avg(dayupdated),avg(daypushed),sum(has_issues) from newdataS group by full_name")
 
 # give descriptive colnames
 colnames(sdS)<-c("Name","Repositories","AvgSize","Watchers","Forks","Issues","Avg_descriptionLenght","Avg_daysSinceCreated","Avg_daysSinceUpdated","Avg_daysSincePushed","IssuesF")
 #sdS$IssuesF<-as.factor(sdS$IssuesF)
 sdS$IssuesF[sdS$Issues<10]=0
 sdS$IssuesF[sdS$Issues>=10]=1
 sdS$IssuesF<-as.factor(sdS$IssuesF)
 
 ggplot(sdS,aes(x=Avg_daysSinceCreated,y=Avg_daysSincePushed,color=IssuesF))+
   geom_point(shape=1)+
   geom_smooth(method=lm)
 
 # correlation between programming langugages