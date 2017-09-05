library("dplyr")
library("ggraph")
library("igraph")
library("ggiraph")

tweets <- rtweet::search_tweets(q = "@hspter OR to:hspter OR hspter",
                                sinceId = 887022381809639424,
                                n = 2000, 
                                include_rts = FALSE)

tweets <- tweets %>%
  distinct()

id <- "887493806735597568"
diff <- 1
while (diff != 0) {
  id_next <- tweets %>%
    filter(in_reply_to_status_status_id %in% id) %>%
    select(status_id) %>%
    pull()
  id_new <- unique(c(id, id_next))
  diff <- length(id_new) - length(id)
  id <- id_new
}

all_replies <- tweets %>% 
  filter(in_reply_to_status_status_id %in% id)

from_text <- all_replies %>%
  select(in_reply_to_status_status_id) %>%
  left_join(all_replies, c("in_reply_to_status_status_id" = "status_id")) %>%
  select(screen_name, text)

tweet_0 <- "@hspter: Do you like getting google calendar invites from your friends for lunches / coffees / etc.?"

to_text <- paste0(all_replies$screen_name, ": ", all_replies$text)
to_text <- gsub("'", "`", to_text)
from_text <- paste0(from_text$screen_name, ": ", from_text$text)
from_text <- gsub("'", "`", from_text)

edges <- tibble::tibble(
  from = from_text,
  to = to_text
) %>%
  mutate(from = ifelse(
    from == "NA: NA",
    tweet_0,
    from)
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
