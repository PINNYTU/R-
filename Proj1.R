# Rfacebook package
install.packages("Rfacebook", dependencies = TRUE)
library(Rfacebook)

# get your token from https://developers.facebook.com/tools/explorer
# replace "AAAABBBBCCCC" with your access token
token <- "EAAGTF53tVYABAIIFYYxeURFhnbBwtZAoSfc9p9W9R3QYb78N957xe5OIZCUrGlJCeKEbwZAzm1KtEHER0p0PPrLw4xfGRRP8UrV7HivZCwUByDz10INErS5dtoCa6OppXCKenPu6lPZAywZCkNZB4DZBqCOhaJEjlYi3z1xrktmd6WQ4PEknATGRHYQO6ZAimQmIZD"

# fetch data from fb page
page <- getPage("Rairakorganic", token, n=1000)

#total number of post 
nrow(page)
# average like, share, post
mean(page$likes_count)
mean(page$comments_count)
mean(page$shares_count)

# total like, share, post
sum(page$likes_count)
sum(page$comments_count)
sum(page$shares_count)

# proportion of content 
table(page$type)
------------------------------------------------------
  
# dplyr package
install.packages("dplyr")
library(dplyr)

# review page structure
glimpse(page)

# select these four columns for our analysis
small_page <- page %>% select(type, like=likes_count, comment=comments_count, share=shares_count)
small_page$type <- as.factor(small_page$type)

# do some deeper analysis
# 1. compute the ratio of share per like
small_page %>%
  mutate(like = like + 1, share = share + 1) %>%
  mutate(ratio_share_like = share / like) %>%
  summarise(avg_ratio = mean(ratio_share_like),
            median_ratio = median(ratio_share_like),
            max_ratio = max(ratio_share_like),
            min_ratio = min(ratio_share_like))

# 2. what kind of content get most likes
small_page %>%
  group_by(type) %>%
  summarise(avg_like = mean(like)) %>%
  arrange(desc(avg_like))

-------------------------------------------------------
# package ggplot2
install.packages("ggplot2")
library(ggplot2)

# create bar plot
small_page %>%
  group_by(type) %>%
  summarise(avg_like = mean(like)) %>%
  arrange(desc(avg_like)) %>%
  ggplot(aes(x = reorder(type, avg_like), y = avg_like, colour=type,fill=type)) +
  geom_bar(stat="identity") +
  theme_minimal() +
  labs(title = "Most popular content on Rairakorganic",
       x = "content type",
       y = "average like per post",
       subtitle = "Photo posts received most averrage likes, followed by video and link respectively",
       caption = "source:Rairakorganic FB Page"
       )

write.csv(page, file = "page_data.csv")
