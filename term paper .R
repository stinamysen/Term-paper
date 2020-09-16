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
library(readxl)

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

#GIF 
#Making function for ggplot of the wanted ---
best <- function(data, players, target) {
  target <- enquo(target) #first we quote the target (by using enquo()) function
  plot.nba <- data %>%
    filter(Player%in%players) %>% #Filtering so that we only get the wanted players
    ggplot(aes(x=Season, y=!!target, group=Player, colour=Player))+ #then we unquote the target (by "!!")  so we can read the value of the column
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


#REGRESSION 
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


#HEATMAP
#Reading data from excel file and merging the data frame nba and state
State <- read_xls("Statkoder.xls")
merge_stat <- left_join(nba,State)

heatmap <- function (data,season,target){
  #Making target a quote
  target <- enquo(target)
  #Making a new data frame which shows the states and the mean of target
  heat <- data %>%
    filter(Team!="TOR")%>%
    filter(Season==season) %>% 
    select(State,!!target) %>% 
    group_by(State) %>% 
    summarize(mean_target=mean(!!target))
  
  colnames(statepop)[colnames(statepop)=="abbr"] <- "State"
  
  #merging the two data frames
  merge<-left_join(heat,statepop)

    #Plotting into heatmap 
  heatmap_plot<-
    plot_usmap(data=merge, values="mean_target", color="red") + 
    scale_fill_continuous(high="red", low="white")+
    theme(legend.position = "right") + labs(fill=target) +
    ggtitle(paste0("Average of ", target, " in ", season, " USA")) #Får ikke opp target, men rar strek
  
  return(heatmap_plot)
}
#Calling function
heatmap(merge_stat,2009,block_pr_k)

