rstats <- import(here("data", "rstats_tweets.rds")) %>% 
  clean_names() %>% 
  as_tibble()
view(rstats)

library(ggplot2)
library(forcats)

top25<-rstats %>%
  count(screen_name,sort=TRUE)

top25$screen_name <- factor(top25$screen_name) 
fct_reorder(top25$screen_name,top25$n, .desc=TRUE)

sample<-top25%>%
  slice_head(n=25)

ggplot(sample,aes(fct_reorder(screen_name,n),n)) +
  geom_col() +
  coord_flip()


