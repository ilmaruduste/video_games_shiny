# video_games_shiny

## Introduction

<b><a href = 'https://andmeteadus2020.shinyapps.io/videomangud/'>LINK to dashboard.</a></b>

The following is documentation for a Video Game Sales statistics visualization project made in the Shiny package (R) by Ilmar Uduste and Kai Budrikas, Data Science enthusiasts and students of the University of Tartu.

The app's UI in Estonian, as the app was meant as a project in an Estonian course, but the code itself and the comments are written in English.

## Necessary software and modules
R (tested on version 3.6.3)
* ggplot2
* dplyr
* Shiny
* rstudioapi
* pheatmap
* shinythemes
* shinyWidgets

## Explanation of files

**Video_Games_Sales_as_at_22_Dec_2016.csv**	- Dataset used for visualizing video game sales. The dataset is from 2016 and available on [Kaggle](https://www.kaggle.com/rush4ratio/video-game-sales-with-ratings).

**app.R**	- The main R file that activates the Shiny application.

**video_games_vis.R**	- A throwaway file meant for testing out different approaches in visualizing video game sales data.
