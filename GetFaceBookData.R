url <- paste0('https://graph.facebook.com/', '15',
              '/posts?fields=from,message,created_time,type,link,comments.summary(true)',
              ',likes.summary(true),shares')
url
??callAPI
tail(frameGroup)
mat <- getNetwork(token, format = "adj.matrix")
dim(mat)

page <- getPage("298933490453962", token, n = 100)
page[which.max(page$likes_count), ]

format.facebook.date <- function(datestring) {
  date <- as.POSIXct(datestring, format = "%Y-%m-%dT%H:%M:%S+0000", tz = "GMT")
}
## aggregate metric counts over month
aggregate.metric <- function(metric) {
  m <- aggregate(page[[paste0(metric, "_count")]], list(month = page$month), 
                 mean)
  m$month <- as.Date(paste0(m$month, "-15"))
  m$metric <- metric
  return(m)
}
# create data frame with average metric counts per month
page$datetime <- format.facebook.date(page$created_time)
page$month <- format(page$datetime, "%Y-%m")
df.list <- lapply(c("likes", "comments", "shares"), aggregate.metric)
df <- do.call(rbind, df.list)
# visualize evolution in metric
library(ggplot2)
library(scales)
ggplot(df, aes(x = month, y = x, group = metric)) + geom_line(aes(color = metric)) + 
  scale_x_date(date_breaks = "years", labels = date_format("%Y")) + scale_y_log10("Average count per post"
          , breaks = c(10, 100, 1000, 10000, 50000)) + theme_bw() + theme(axis.title.x = element_blank())
library("Rfacebook", lib.loc="~/R/win-library/3.3")
token<-'EAACEdEose0cBAFO9HabC5cxDAgVepybZBYFy7YP2jrfXOyxAG4erZCz9fhdFtC2e2CUwQLKlF2hBA8sYIi7R7ZBgCPP8ZAZBcQlYadryXZCSUZAfV8GOjVRLsUMoZBMt1ASotwDzLsmhi8vuBuEtHQhyxKif33AlwUfFRsFse6qAKZAZAsGUB6U5trtYWgZBQrI7iMZD'

me <- getUsers("me", token=token)
my_friends <- getFriends(token=token, simplify=TRUE)
my_friends_info <- getUsers(my_friends$id, token=token, private_info=TRUE)
my_network <- getNetwork(token, format="adj.matrix")
singletons <- rowSums(my_network)==0 # friends who are friends with me alone

require(igraph)
my_graph <- graph.adjacency(my_network[!singletons,!singletons])
layout <- layout.drl(my_graph,options=list(simmer.attraction=0))
plot(my_graph, vertex.size=3, 
     #vertex.label=NA, 
     vertex.label.cex=0.5,
     edge.arrow.size=0, edge.curved=TRUE,layout=layout)






require(Rfacebook)
fb_oauth<-'EAACEdEose0cBAFO9HabC5cxDAgVepybZBYFy7YP2jrfXOyxAG4erZCz9fhdFtC2e2CUwQLKlF2hBA8sYIi7R7ZBgCPP8ZAZBcQlYadryXZCSUZAfV8GOjVRLsUMoZBMt1ASotwDzLsmhi8vuBuEtHQhyxKif33AlwUfFRsFse6qAKZAZAsGUB6U5trtYWgZBQrI7iMZD'
me <- getUsers("me", token=fb_oauth)
my_friends <- getFriends(token=fb_oauth, simplify=TRUE)
my_friends_info <- getUsers(my_friends$id, token=fb_oauth, private_info=TRUE)
my_network <- getNetwork(fb_oauth, format="adj.matrix")
singletons <- rowSums(my_network)==0 # friends who are friends with me alone

require(igraph)
my_graph <- graph.adjacency(my_network[!singletons,!singletons])
layout <- layout.drl(my_graph,options=list(simmer.attraction=0))
plot(my_graph, vertex.size=4, 
     #vertex.label=NA, 
     vertex.label.cex=1.0,
     edge.arrow.size=1, edge.curved=TRUE)
