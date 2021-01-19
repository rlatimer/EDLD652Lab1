library(tidyverse)
library(here)
library(rio)
library(knitr)
library(janitor)
library(psych)
library(sjPlot)
library(sjmisc)

  
#install.packages("patchwork")
#install.packages("lubridate")
library(tidyverse)
library(here)
library(rio)
library(knitr)
library(janitor)
library(patchwork)
library(ggplot2)
library(forcats)
library(lubridate)

rstats <- import(here("data", "rstats_tweets.rds")) %>% 
  clean_names() %>% 
  as_tibble()


top25<-rstats %>%
  count(screen_name,sort=TRUE)

top25$screen_name <- factor(top25$screen_name) 
fct_reorder(top25$screen_name,top25$n, .desc=TRUE)

data_plot1<-top25%>%
  slice_head(n=25)

ggplot(data_plot1,aes(fct_reorder(screen_name,n),n)) +
  geom_col() +
  coord_flip()


plot2  <- 
  rstats %>%  
  count(month = round_date(created_at,"month"))

#plot draft of plot 2
ggplot(plot2, aes(month, n)) +
  geom_line() +
  geom_smooth() +
  xlab("Year (data summarized by month") +
  ylab("Number of #rstats tweets") +
  labs(title = "Growth of the #rstats hashtag on twitter over time") +
  coord_cartesian(xlim = lubridate::as_datetime(c("2008-01-01","2018-12-01")), 
                  ylim = c(0,12000),
                  expand = F)


plot1_refined <- sample %>% 
  ggplot(aes(x = fct_reorder(screen_name,n), y= n)) + 
  geom_col(fill = "steelblue1") + 
  coord_flip() +
  labs(title = "Most prolific #rstats tweeters", 
       subtitle = "Top 25 screen names displayed",
       x = "Twitter Screen Name",
       y = "Count",
       caption = "Data from Mike Kearny, distributed via #tidytuesday") + 
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank())
plot1_refined

data_plot2 <- plot2


top25 %>% 
  fct_reorder(screen_name, n, .desc = TRUE)


plot2_refined <- data_plot2 %>% 
  ggplot(aes(x = month, y = n)) +
  geom_line(color = "gray28", size = 1.5) +
  geom_smooth(se = FALSE, color = "orchid1", size = 1.5) +
  geom_area(fill = "slategray1", alpha = 0.7) +
  coord_cartesian(
    xlim = lubridate::as_datetime(c("2008-01-01","2018-12-01")), 
    ylim = c(0,12000),
    expand = F) + 
  labs(title = "Growth of the #rstats hashtag on twitter over time",
       x = "Year (data summarized by month)",
       y = "Number of #rstats tweets",
       caption = "Data from Mike Kearny, distributed via #tidytuesday") + 
  theme_minimal() + 
  theme(panel.grid.major = element_line(size = (1)),
        panel.grid.minor = element_line(size = (0.5)))
plot2_refined
