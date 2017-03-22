require(devtools)
install_github("cscheid/rgithub")
library(github)

me<-get.myself(ctx)
get.my.repositories(ctx=get.github.context())
get.my.following(ctx)
get.public.events(ctx=get.github.context())
get.all.repositories(ctx=get.github.context())
get.user("approdriguez",ctx)

