# libraries:
library(gganimate)
library(dplyr)
library(readr)
library(ggplot2)
library(magrittr)
library(tidyverse)
library(readr)
library(car)
library(gifski)


nba <-read.csv("basket.csv") %>% 
  filter(League=="NBA") %>% 
  select(Season, Player, Stage, GP, MIN, FGM, BLK, FGA, X3PM, X3PA, FTM, FTA) %>% 
  filter(Stage !="Playoffs") %>% 
  mutate(h_rate= FGM/FGA)  %>%
  mutate(poeng_pr_k = FGM/GP) %>%
  mutate(poeng_pr_min = FGM/MIN) %>%
  mutate(block_pr_k = BLK/GP) %>%
  mutate(block_pr_min = BLK/MIN) %>%
  mutate(Season = recode(
    Season,
    "'2009 - 2010' = '2009';
    '2010 - 2011' = '2010';
    '2011 - 2012' = '2011';
    '2012 - 2013' = '2012';
    '2013 - 2014' = '2013';
    '2014 - 2015' = '2014';
    '2015 - 2016' = '2015';
    '2016 - 2017' = '2016';
    '2017 - 2018' = '2017';
    '2018 - 2019' = '2018'"
  )) %>%
  mutate(Season = as.numeric(Season))

best<- function(data, players) {
  h.rate<- data %>% 
    filter(Player%in%players) %>% 
    ggplot(aes(x=Season, y=h_rate, group=Player, colour=Player))+
    geom_line() +
    transition_reveal(Season)
  
  return(h.rate)
}

g <- best(data=nba, players=c("LeBron James", "Kevin Durant"))

#Animate g
animate(g, duration = 5, fps = 20, width = 650, height = 450, renderer = gifski_renderer())

# Save at gif:
anim_save("Basketball_timeilne.gif")