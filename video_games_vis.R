#Import the necessary libraries
library(ggplot2)
library(dplyr)
library(shiny)
library(rstudioapi)
library(heatmap)

#Set Working Directory to the folder where this script is located
setwd(dirname(getActiveDocumentContext()$path))

#Load the DataFrame
video_games <- read.csv("Video_Games_Sales_as_at_22_Dec_2016.csv")

#Summarise the data to see visualisation opportunities
summary(video_games)

mainstream_platforms <- c("XB", "X360", "WiiU", "Wii",
                          "SNES", "PSP", "PS4", "PS3", "PS2", 
                          "PS", "PC", "NES", "N64", "GEN", 
                          "GC", "GBA", "DS", "3DS", "2600")

#Look at how Critic Scores affect sales. 
#TODO: Maybe do the same for user scores?
#TODO: Draw a regression line somehow
video_games %>%
  ggplot(aes(x=Critic_Score, y=Global_Sales)) + geom_point(aes(color=Genre)) +
  scale_y_continuous(limits = c(0,40))

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
#TODO: Add regional parameters
#TODO: Add possibility to choose platform
#TODO: Make the whole thing more aesthetic xd
#TODO: Remove NA and 2020 from the years, maybe 2017 as well
video_games %>% 
  group_by(Year_of_Release) %>% 
  summarise(Global_Sales = sum(Global_Sales)) %>% 
  ggplot(aes(x=Year_of_Release, y=Global_Sales, group=1)) +
  geom_line() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


#Heatmap between Platform and Genre, to see which genres sold best on certain platforms
#TODO: Implement changing the scale from absolute sales to proportion of total sales for platform
#TODO: Remove a lot of the non-mainstream consoles?
#TODO: Maybe flip the scales? See what works best
#TODO: Remove the "" Genre
video_games %>% 
  group_by(Genre, Platform) %>% 
  filter(Platform %in% mainstream_platforms) %>% #Filtering for mainstream consoles
  summarise(Global_Sales = sum(Global_Sales)) %>% 
  ggplot(aes(x=Genre, y=Platform, fill=Global_Sales)) +
  geom_tile() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

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

