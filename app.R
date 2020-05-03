library(ggplot2)
library(dplyr)
library(shiny)
library(rstudioapi)
library(pheatmap)

#Set Working Directory to the folder where this script is located
setwd(dirname(getActiveDocumentContext()$path))

#Load the DataFrame
video_games <- read.csv("Video_Games_Sales_as_at_22_Dec_2016.csv")

#Define mainstream platforms
mainstream_platforms <- c("XB", "X360", "WiiU", "Wii",
                          "SNES", "PSP", "PS4", "PS3", "PS2", 
                          "PS", "PC", "NES", "N64", "GEN", 
                          "GC", "GBA", "DS", "3DS", "2600")

video_games <- video_games %>% 
    filter(Platform %in% mainstream_platforms)

plot_sales = function(genre, platform){
    plot <- video_games %>% 
        filter(Genre==genre,
               Platform==platform) %>% 
        group_by(Year_of_Release) %>% 
        summarise(Global_Sales = sum(Global_Sales)) %>% 
        filter(Year_of_Release!='N/A') %>% 
        ggplot(aes(x=Year_of_Release, y=Global_Sales, group=1)) +
        geom_line(color='purple') +
        theme(axis.text.x = element_text(angle = 90, hjust = 1))+
        ggtitle("Müügitulud")+
        #TODO: What units do the müügitulud have?
        xlab("Aasta")+
        ylab("Tulu")
    
    return(plot)
}

# Define UI for application
ui <- fluidPage(

    titlePanel("Rakendus videomängude müükide visualiseerimiseks"),
    
    sidebarLayout(
        sidebarPanel(
            
            selectInput("genre",
                        strong("Vali žanr"),
                        choices = levels(video_games$Genre),
                        selected = "Sports"),
            
            
            selectInput("platform",
                        strong("Vali platvorm"),
                        choices = levels(video_games$Platform),
                        selected = "Wii")
        ),
        
        mainPanel(
            h1("Videomängude müügid žanri ja platvormi järgi"),
            p("Skratta deuu"),
            plotOutput("plot_sales")
            )
        )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    observe({
        
        # Sinu kood
        
        updateSelectizeInput(session, "genre",
                             label = "Vali žanr",
                             choices = levels(video_games$Genre),
                             selected = "Sports")
        
        updateSelectizeInput(session, "platform",
                             label = "Vali platvorm",
                             choices = levels(video_games$Platform),
                             selected = "Wii")
    })
    
    output$plot_sales <- renderPlot({
        plot_sales(input$genre, input$platvorm)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
