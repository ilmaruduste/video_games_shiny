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

#TODO: Mainstream platform filtering doesn't seem to work atm, could use a fix.
video_games <- video_games %>% 
    filter(Genre != "" && Genre != " " && Platform %in% mainstream_platforms) %>% 
    na.omit()

#Define linegraph plot for video game sales
plot_sales = function(video_games, genre, platform){
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
        #TODO: What units do the müügitulud have? I think these are in units of copies, not $
        xlab("Aasta")+
        ylab("Tulu")
    
    return(plot)
}

#Define heatmap plot for video game sales
plot_heatmap <- function(platforms) {
    heatmap <- video_games %>% 
        group_by(Genre, Platform) %>% 
        filter(Genre != "" && Platform %in% platforms) %>% #Filtering for mainstream consoles
        summarise(Global_Sales = sum(Global_Sales)) %>% 
        ggplot(aes(x=Platform, y=Genre, fill=Global_Sales)) +
        geom_tile(aes(fill=Global_Sales)) +
        geom_text(aes(label = round(Global_Sales, 1))) +
        theme_classic() +
        theme(axis.text.x = element_text(angle = 0, hjust = 1)) +
        scale_fill_gradient(low="white", high = "#39ff14")
    
    return(heatmap)
}

# Define UI for application
#TODO: make the application support multiple tabs.
#Good example here: https://shiny.rstudio.com/gallery/navbar-example.html
ui <- fluidPage(
    
    navbarPage("Navbar!",
        tabPanel("Linegraph",
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
            ),
        tabPanel("Heatmap",
             titlePanel("Rakendus videomängude müükide visualiseerimiseks"),
             
             sidebarLayout(
                 sidebarPanel(

                     checkboxGroupInput("selected_platforms",
                                 strong("Vali platvorm"),
                                 choices = levels(video_games$Platform),
                                 selected = "PC")
                 ),
                 
                 mainPanel(
                     h1("Videomängude müügid žanri ja platvormi järgi"),
                     p("Heatmap application here"),
                     plotOutput("plot_heatmap")
                 )
             )      
            ),
        tabPanel("Summary",
                 titlePanel("Rakendus videomängude müükide visualiseerimiseks"),
                 verbatimTextOutput("summary")
            )
        )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    observe({
        
        updateSelectizeInput(session, "genre",
                             label = "Vali žanr",
                             choices = levels(video_games$Genre),
                             selected = "Sports")
        
        updateSelectizeInput(session, "platform",
                             label = "Vali platvorm",
                             choices = levels(video_games$Platform),
                             selected = "Wii")
        
        updateCheckboxGroupInput(session, "selected_platforms",
                             label = "Vali platvormid visualiseerimiseks",
                             choices = levels(video_games$Platform),
                             selected = "PC")
    })
    
    output$plot_sales <- renderPlot({
        plot_sales(video_games, input$genre, input$platform)
    })
    
    output$summary <- renderPrint({
        summary(video_games)
    })
    
    output$plot_heatmap <- renderPlot({
        plot_heatmap(platforms = input$selected_platforms)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
