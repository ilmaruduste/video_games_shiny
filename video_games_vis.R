#Import the necessary libraries
#skrt
library(ggplot2)
library(dplyr)
library(shiny)
library(rstudioapi)
library(pheatmap)

#Set Working Directory to the folder where this script is located
setwd(dirname(getActiveDocumentContext()$path))

#Load the DataFrame
video_games <- read.csv("Video_Games_Sales_as_at_22_Dec_2016.csv")

#Summarise the data to see visualisation opportunities
summary(video_games)


#video_games$Platform <- as.factor(video_games$Platform)
#Define mainstream platforms
#TODO: Maybe create a separate column for full names of consoles?
mainstream_platforms <- c("XB", "X360", "WiiU", "Wii",
                          "SNES", "PSP", "PS4", "PS3", "PS2", 
                          "PS", "PC", "NES", "N64", "GEN", 
                          "GC", "GBA", "DS", "3DS", "2600")

#TODO: Transform user_score to same scale that critic_score has
video_games$User_Score <- as.numeric(video_games$User_Score)

#Testing filtering...
#For some reason the filter doesn't work at all for platforms
playstation <- c("PS", "PS2", "PS3")

pc <- "PC"

#It isn't correct to omit NA, since a lot of the games made before the 90s
  #didn't actually get scored (e.g. Super Mario Bros.)
video_games <- video_games %>% 
  filter(Genre != "" && Genre != " " && Platform %in% mainstream_platforms) 

video_games_ps <- video_games %>% 
  filter(Genre != "" && Genre != " " && Platform %in% playstation)

video_games_pc <- video_games %>% 
  filter(Genre != "" && Genre != " " && Platform == pc)

unique(video_games$Platform)

#Look at how Critic Scores affect sales. 
#TODO: Maybe do the same for user scores?
#TODO: Draw a regression line somehow
video_games %>%
  ggplot(aes(x=Critic_Score, y=Global_Sales)) + geom_point(aes(color=Year_of_Release)) +
  scale_y_continuous(limits = c(0,40))+ 
  geom_smooth()


#Visualize user_score and critic_score dependancy
video_games %>% 
  filter(User_Score != 1) %>% 
  ggplot(aes(x=User_Score, y=Critic_Score)) + 
  geom_point(aes(color=Rating)) + 
  geom_smooth()

#NOTE: Critic Scores start from 1994

#Look at how video game ratings have changed throughout history
#TODO: Maybe add possibility to pick out certain platforms and differentiate between Critic and User Score?
video_games %>% 
  group_by(Year_of_Release, Platform) %>% 
  summarise(avg_critic_rating = mean(Critic_Score, na.rm=TRUE),
            avg_user_rating = mean(User_Score, na.rm=TRUE)) %>% 
  filter(!is.na(avg_critic_rating) | !is.na(avg_user_rating)) %>% 
  ggplot(aes(x=Year_of_Release, group=2)) + 
  geom_line(aes(y=avg_critic_rating, color=Platform)) +
  geom_line(aes(y=avg_user_rating, color=Platform)) +   #This doesn't work for some reason
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_x_discrete()


#Visualise highest regional/global sales by publisher
#TODO: add filter for seeing publishers with n to m sales
#TODO: Sort the values on the graph
#TODO: Fill the global sales bar with regional sales perhaps?
video_games %>% 
  group_by(Publisher) %>% 
  summarise(Global_Sales = sum(Global_Sales)) %>% 
  arrange(desc(Global_Sales)) %>% 
  top_n(15) %>% 
  ggplot(aes(x=Publisher, y=Global_Sales)) + 
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


#Visualise video game sales over time
#TODO: Add regional parameters?
#TODO: Make the whole thing more aesthetic
#TODO: Edit parameters or select only a range of data to make the app work for every observation
plot_sales = function(genre, platform){
video_games %>% 
  filter(Genre==genre,
         Platform==platform) %>% 
  group_by(Year_of_Release) %>% 
  summarise(Global_Sales = sum(Global_Sales)) %>% 
  filter(Year_of_Release!='N/A') %>% 
  ggplot(aes(x=Year_of_Release, y=Global_Sales, group=1)) +
  geom_line(color='purple') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
    ggtitle("Müügitulud")+
    xlab("Aasta")+
    ylab("Tulu")
}

plot_sales("Sports", "Wii")

#Heatmap between Platform and Genre, to see which genres sold best on certain platforms
#TODO: Implement changing the scale from absolute sales to proportion of total sales for platform
#TODO: Look at different color schemes
#TODO: Change legend title to "millions of units sold" or smth
sales_heatmap <- function(selected_platforms) {
  heatmap <- video_games %>% 
    group_by(Genre, Platform) %>% 
    filter(Genre != "" && Platform %in% selected_platforms) %>% #Filtering for mainstream consoles
    summarise(Global_Sales = sum(Global_Sales)) %>% 
    ggplot(aes(x=Platform, y=Genre, fill=Global_Sales)) +
    geom_tile(aes(fill=Global_Sales)) +
    geom_text(aes(label = round(Global_Sales, 1))) +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 0, hjust = 1)) +
    scale_fill_gradient(low="white", high = "#39ff14")
  
  return(heatmap)
}

sales_heatmap(c("PS2", "PS", "PS3", "PC", "XB", "X360", "Wii"))

#Heatmap between Platform and Genre, to see which genres sold best on certain platforms (RELATIVE VERSION)
#WIP: Implement changing the scale from absolute sales to proportion of total sales for platform
video_games %>% 
  group_by(Genre, Platform) %>% 
  filter(Platform %in% mainstream_platforms) %>% #Filtering for mainstream consoles
  summarise(Global_Sales = 
              sum(Global_Sales_Proportion)/as.numeric(as.character(
                aggregate(video_games$Global_Sales, #Can't get this to work
                          by = list(Category=video_games$Platform), FUN=sum)))) %>% 
  ggplot(aes(x=Genre, y=Platform, fill=Global_Sales)) +
  geom_tile() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

