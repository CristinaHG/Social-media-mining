require(devtools)
install_github("cscheid/rgithub")
library(github)

me<-get.myself(ctx)
get.my.repositories(ctx=get.github.context())
followers<-get.my.following(ctx)
publicEvents<-get.public.events(ctx=get.github.context())
someRepos<-get.all.repositories(ctx=get.github.context())
get.user("approdriguez",ctx)

aisharRepos<-get.user.repositories("approdriguez",ctx=get.github.context()) #get aishar repos
# How much repos Aishar has?
length(aisharRepos$content) #15
usedLanguageInRepo<-get.repository.languages("approdriguez","Expert-System",ctx = get.github.context())

