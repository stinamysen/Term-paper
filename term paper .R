# libraries:
library(gganimate)
library(dplyr)
library(readr)
library(ggplot2)
library(magrittr)
library(tidyverse)
library(readr)
library(gganimate)
library(car)
library(gifski)

library(usmap)



#making a data frame of the csv, and adding multiple columnes that says something about how good a player is:
nba <-read.csv("basket.csv") %>% 
  filter(League=="NBA") %>%
  select(Season, Player, Stage, Team, GP, MIN, FGM, BLK, FGA, X3PM, X3PA, FTM, FTA, height_cm) %>% 
  filter(Stage !="Playoffs") %>% 
  mutate(h_rate= FGM/FGA)%>%
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
  mutate(Season = as.numeric(Season)) #change the seasons to one year and to numeric.
                                      #This makes it possible to use this column in the best funtion

best <- function(data, players, column) {
  column <- enquo(column) #first we quote the column (by using enquo()) function
  plot.nba <- data %>%
    filter(Player%in%players) %>%
    ggplot(aes(x=Season, y=!!column, group=Player, colour=Player))+ #then we unquote the column (by "!!")  so we can read the value of the column
    geom_line() +
    transition_reveal(Season) #to make the plot gradually appear
  
  return(plot.nba)
}
#making a variable to put in the animate function
g <- best(data = nba, players = c("LeBron James", "Kevin Durant"), GP)

#Animate g
animate(g, duration = 5, fps = 20, width = 650, height = 450, renderer = gifski_renderer())

# Save as a gif:
anim_save("Basketball_timeilne.gif")


# Regression on how the height affects the hitting rate
summary(lm(h_rate~height_cm, data=nba))

#Creating empty columns
nba$p_value<-NA
nba$beta_hight<-NA

#This for-loop is omitting one player at time and then add the p- value and Beta to the new the columns
for(i in 1:nrow(nba)){
  nba$p_value[i] <- 
    lm(h_rate~ height_cm, data=nba, subset=-i) %>%#removing (by subset=-i) one of the player
    summary %>%  
    coef %>% 
    .[2, 4]    #Selecting the p-value from the regression output
  
  nba$beta_hight[i] <- 
    lm(h_rate~ height_cm, data=nba, subset=-i) %>% 
    .$coefficients %>%    #obtain a vector of coeffesient
    .[2]                  #selecting the beta value from the vector 
  
}
#Creating a character string to put in the geom_text
remove<- "removed" 

#Creating a function for the regression model

reg<- function(dataf, players, season) {
  p_plot<-
  dataf %>% 
  filter(Season==season) %>% 
  filter(Player%in%players) %>% 
  ggplot(aes(x=beta_hight, y=p_value))+ 
  geom_line()+
  geom_point(size=0.5)+
  geom_text(aes(label=paste(Player,"\n", remove)),hjust=0.45, vjust=1, size=2.5, colour="blue", fontface='bold')+
    xlab("Beta")+
    ylab("P value")+
    theme_classic(base_size = 8)
  
  return(p_plot)
}
#test:
reg(dataf=nba, players = c("LeBron James", "Kevin Durant", "Kobe Bryant", "Chris Bosh"), season=2010)


#south<-c("OKC","MIA","DAL","LAL", "PHX", "MEM", "CHA", "ATL", "HOU", "NOP","ORL", "SAS")
#midwest<-c("CLE","CHI", "IND","MIN","MIL", "DET")
#northeast<-c("NYK", "NJN", "PHI", "BOS", "BRK")
#west<-c("GSW", "UTA", "SAC", "LAC", "POR","WAS", "DEN")

#Creating variables and vectors which says which teams that each state consist of
OK<-"OKC"
FL<-c("MIA","ORL")
TX<-c("DAL","HOU","SAS")
AZ<-c("PHX")
TN<-c("MEM")
NC<-c("CHA")
GA<-c("ATL")
LA<-c("NOP")
OH<-c("CLE")
IL<-c("CHI")
IN<-c("IND")
MN<-"MIN"
WI<-"MIL"
MI<-"DET"
NY<-c("NYK","BRK")
NJ<-"NJN"
PA<-"PHI"
MA<-"BOS"
CA<-c("GSW","LAC","SAC", "LAL")
OR<-"POR"
UT<-"UTA"
CO<-"DEN"
WA<-"WAS"

 
#Making a new data frame which includes a new column "State"
heat <- nba %>%
  filter(Team!="TOR")%>%
  mutate(State = case_when(Team %in% OK ~ "OK",
                           Team %in% FL ~ "FL",
                           Team %in% AZ ~ "AZ",
                           Team %in% TN ~ "TN",
                           Team %in% NC ~ "NC",
                           Team %in% GA ~ "GA",
                           Team %in% LA ~ "LA",
                           Team %in% OH ~ "OH",
                           Team %in% IL ~ "IL",
                           Team %in% IN ~ "IN",
                           Team %in% MN ~ "MN",
                           Team %in% MI ~ "MI",
                           Team %in% NY ~ "NY",
                           Team %in% NJ ~ "NJ",
                           Team %in% PA ~ "PA",
                           Team %in% MA ~ "MA",
                           Team %in% CA ~ "CA",
                           Team %in% OR ~ "OR",
                           Team %in% UT ~ "UT",
                           Team %in% CO ~ "CO",
                           Team %in% WA ~ "WA",
                           Team %in% TX ~ "TX",
                           Team%in% WI ~ "WI"
                                      )) %>% 
  filter(Season==2009) %>% 
  select(State,h_rate) 

colnames(heat)
#changing the colname of the statecodes from the statepop dataframe to "State"
colnames(statepop)[colnames(statepop)=="abbr"] <- "State"

#merging the two data frames
merge<-left_join(heat,statepop) 
  
  

#plotting a heatmap
heatmap<-
  plot_usmap(data=merge, values="m_h_rate", color="red") + 
  scale_fill_continuous(high="red", low="white")+
  theme(legend.position = "right") + labs(fill="Hit Rate") +
  ggtitle("2009 Hit Rate Average")
  








