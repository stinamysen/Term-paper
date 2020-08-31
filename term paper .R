library(dplyr)
library(readr)
library(ggplot2)
library(magrittr)
library(tidyverse)
library(readr)

nba <-read.csv("basket.csv") %>% 
  filter(League=="NBA") %>% 
  select(Season, Player, Stage, MIN, FGM, FGA, X3PM, X3PA, FTM, FTA) %>% 
  filter(Stage !="Playoffs") %>% 
  mutate(h_rate= FGM/FGA)



best<- function(data, players) {
  h.rate<- data %>% 
    filter(Player%in%players) %>% 
    ggplot(aes(x=Season, y=h_rate, group=Player, colour=Player))+
    geom_line()
  
  return(h.rate)
}

best(data, players)
#test
