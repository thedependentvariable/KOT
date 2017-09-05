library(rtweet)

harshtags <- c("#sad","#happy","#sarcasm")

twitter_token=create_token(app = "Adie", consumer_key="7ktjx3mVs0H63sHVZZw", 
                           consumer_secret="VT9UhDwijQh7jiszvavYZ0j0V4yE1pb8WJaNObOu7BM", 
                           cache = TRUE)

unique_users <- tweets_all$user_id %>% as.numeric() %>% unique() %>% na.omit()
followed_parent <- NULL
for (i in 66:length(unique_users)){
followed=get_friends(unique_users[i],page="-1",parse=TRUE, as_double = FALSE,
                        token = twitter_token)
followed$parent= unique_users[i]
followed_parent=rbind(followed_parent,followed)
}

#introducing a dummy variable to be used to populate the sparse matrix
follow_train <- followed_parent %>%
  mutate(true_false=1) %>%
  spread(user_id, true_false)

edges <- tibble::tibble(
  from = followed_parent$parent,
  to = followed_parent$user_id
)
graph <- graph_from_data_frame(edges, directed = TRUE)
V(graph)$tooltip <- V(graph)$name

set.seed(525)
p <- ggraph(graph, layout = "nicely") + 
  geom_edge_link() + 
  geom_point_interactive(aes(x, y, color = "red", alpha = 0.05, tooltip = tooltip)) +
  theme_void() + 
  theme(legend.position = "none")
ggiraph(code = print(p),
        width_svg = 10,
        zoom_max = 4)


#########################################################
##### Read twitter follower information #################
#########################################################

library(httr)
library(rvest)
twitter_login <- "https://twitter.com/login"

pars <- list(
  "session[username_or_email]" = "@AdiePrestone",
  "session[password]" = "me+God=100%",
  "authenticity_token"= "aac8e8e51a941bb58f8ba9cd5374e13889f1dd5c"
)
POST(twitter_login, body = pars)

tweets_followers <- read_html("https://twitter.com/QueenGathoni/following")
