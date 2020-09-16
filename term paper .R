# libraries:
library(dplyr)
library(ggplot2)
library(magrittr)
library(tidyverse)
library(gganimate)
library(gifski)
library(usmap)
library(readxl)

#Making a data frame of the csv and filtering so that only the players in NBA is a part of the frame.
#Also filtering so that we only have the players that are not at the stage "playoffs". 
#Selecting the columns we want to use further in our program. 
#Making columns (mutate) that tells us how good a player is.
nba <-read.csv("basket.csv") %>% 
  filter(League=="NBA") %>%
  select(Season, Player, Stage, Team, GP, MIN, FGM, BLK, FGA, X3PM, X3PA, FTM, FTA, height_cm) %>% 
  filter(Stage !="Playoffs") %>% 
  mutate(h_rate= FGM/FGA)%>%
  mutate(poeng_pr_k = FGM/GP) %>%
  mutate(poeng_pr_min = FGM/MIN) %>%
  mutate(block_pr_k = BLK/GP) %>%
  mutate(block_pr_min = BLK/MIN) %>%
  mutate(Season = recode(          #Recoding the season, so it will be easier to use.
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
  mutate(Season = as.numeric(Season)) #change the seasons to numeric.
                                      #This makes it possible to use this column in the "best" function




#GIF 
#Making a function so that we can look at spesicif players improvement throughout the time period.
#The parameter data will be the nba data frame we made above. 
#Players can be anyone of the players in the dataset. 
#Target refers to what qualification (kan vi bruke et bedre ord her?) we want to look at.

best <- function(data, players, target) {
  target <- enquo(target) #first we quote the target (by using enquo() function). 
                          #
  plot.nba <- data %>%
    filter(Player%in%players) %>% #Filtering so that we only look at the wanted players
    ggplot(aes(x=Season, y=!!target, group=Player, colour=Player))+ #Then we unquote the target (by "!!") 
                                                                    #We do this so that er can read the value of the column.
    geom_line() +                                                   
    transition_reveal(Season) #to make the data gradually appear
  
  return(plot.nba)
}
#making a variable to put in the animate function
g <- best(data = nba, players = c("LeBron James", "Kevin Durant"), GP)

#Animate g
animate(g, duration = 5, width = 650, height = 450, renderer = gifski_renderer())
#Duration: length of the animation in seconds
#Renderer: renders the generated frames and makes it into an animation (gifski_renderer is the default).

# Save as a gif:
anim_save("Basketball_timeilne.gif")





#REGRESSION 
# Regression on how the height affects the hitting rate
summary(lm(h_rate~height_cm, data=nba))

#Creating empty columns
nba$p_value<-NA
nba$beta_hight<-NA

#This for-loop is omitting one player at time and then add the p- value and Beta to the new columns
for(i in 1:nrow(nba)){
  nba$p_value[i] <- 
    lm(h_rate~ height_cm, data=nba, subset=-i) %>% #removing (by subset=-i) one of the players
    summary %>%  
    coef %>%   #Hva gjør denne?
    .[2, 4]    #Selecting the p-value from the regression output - hvorfor 2 og 4?
  
  nba$beta_hight[i] <- 
    lm(h_rate~ height_cm, data=nba, subset=-i) %>% 
    .$coefficients %>%    #obtain a vector of coefficient
    .[2]                  #selecting the beta value from the vector 
  
}
#Creating a character string to put in the geom_text
remove<- "removed" 

#Creating a function for the regression model
reg<- function(dataf, players, season) {
  p_plot<-
  dataf %>% 
  filter(Season==season) %>%       #Filtering so that we only look at the chosen season (parameter)
  filter(Player%in%players) %>%    #Filtering so that we only use the chosen players
  ggplot(aes(x=beta_hight, y=p_value))+ 
  geom_line()+
  geom_point(size=0.5)+
  geom_text(aes(label=paste(Player,"\n", remove)),hjust=0.45, vjust=1, size=2.5, colour="blue", fontface='bold')+
    xlab("Beta")+
    ylab("P value")+
    theme_classic(base_size = 8)
  
  return(p_plot)
}
#testcalling the function:
reg(dataf=nba, players = c("LeBron James", "Kevin Durant", "Kobe Bryant", "Chris Bosh"), season=2010)






#HEATMAP
#Reading data from excel file we made and saving in the variable "State"
State <- read_xls("Statkoder.xls")
#Merging the data frame "nba" and "state"
merge_stat <- left_join(nba,State)

#Making a function so that the user can chose the data to me used, what season they want to look at
#and the target they want to analyse. 
heatmap <- function (data,season,target){
  target <- enquo(target)           #Making target a quote (same as in the function "best")
  heat <- data %>%                  #Making a new data frame which shows the states and the mean of target
    filter(Team!="TOR")%>%          #Filtering out "TOR" because it is a Canadian team.
    filter(Season==season) %>%      #Filtering so that we only look at the chosen season
    select(State,!!target) %>%      #Selecting the column "State" and given target. 
    group_by(State) %>%             #Grouping by state so that we can find the mean of target in next line. 
    summarize(mean_target=mean(!!target))
  
  colnames(statepop)[colnames(statepop)=="abbr"] <- "State"     #Hvorfor gjør vi dette igjen?
  
 
  merge<-left_join(heat,statepop)   #merging the two data frames
  
  heatmap_plot<-                    #Plotting into heatmap 
    plot_usmap(data=merge, values="mean_target", color="red") + 
    scale_fill_continuous(high="red", low="white")+
    theme(legend.position = "right") + labs(fill=target) +
    ggtitle(paste0("Heat map over the states")) 
  
  return(heatmap_plot)
}

#Calling function
heatmap(merge_stat,2009,h_rate)

