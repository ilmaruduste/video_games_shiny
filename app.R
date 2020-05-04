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

#Mainstream platform filtering
video_games <- video_games %>% 
  filter(Genre != "" & Genre != " " & Platform %in% mainstream_platforms) %>% 
  na.omit()

#Reset the levels
video_games$Platform = droplevels(video_games$Platform)

#Define linegraph plot for video game sales
plot_sales = function(video_games, genre, platform){
  video_games %>% 
    filter(Genre==genre,
           Platform==platform) %>% 
    group_by(Year_of_Release) %>% 
    summarise(Global_Sales = sum(Global_Sales)) %>% 
    filter(Year_of_Release!='N/A') %>% 
    ggplot(aes(x=Year_of_Release, y=Global_Sales, group=1)) +
    geom_line(color='purple') +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))+
    ggtitle("Müüdud mängude koguarv valitud aastate lõikes")+
    xlab("Aasta")+
    ylab("Koguarv")
}

#Define heatmap plot for video game sales
plot_heatmap <- function(platforms) {
  video_games %>% 
    group_by(Genre, Platform) %>% 
    filter(Genre != "" & Platform %in% platforms) %>% #Filtering for mainstream consoles
    summarise(Global_Sales = sum(Global_Sales)) %>% 
    ggplot(aes(x=Platform, y=Genre, fill=Global_Sales)) +
    geom_tile(aes(fill=Global_Sales)) +
    geom_text(aes(label = round(Global_Sales, 1))) +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 0, hjust = 1)) +
    scale_fill_gradient(low="white", high = "#39ff14")
}

# Define UI for application
#TODO: make the application support multiple tabs.
#Good example here: https://shiny.rstudio.com/gallery/navbar-example.html
ui <- fluidPage(
  
  navbarPage("Navbar!<3",
             tabPanel("Avaleht",
                      titlePanel(h1("Rakendus videomängude müükide visualiseerimiseks", style = "font-family: 'Comic Sans MS'")),
                        
                        mainPanel(
                          h2("Projekti kirjeldus", style = "font-family: 'Comic Sans MS'"),
                          p('Oleme noored andmeteadusehuvilised Tartu Ülikooli tudengid Ilmar Uduste ja Kai Budrikas ning otsustasime 
                          aine "Statistiline andmeteadus ja visualiseerimine" raames uurida erinevate videomängude müüke.
                          Andmestik, mis pärineb Kaggle-st, koosneb 16 tunnusest ja pea 16 000 erinevast mängust.'),
                          p('Selleks, et oma andmeid kenasti visualiseerida, koostasime 100 002 interaktiivset visualiseeringut, 
                            mida võite uurida järgmistelt lehekülgedelt.') #Xd palun töötle mind
                        )
                            
             ),
             
             tabPanel("Joondiagramm",
                      titlePanel(h1("Rakendus videomängude müükide visualiseerimiseks", style = "font-family: 'Comic Sans MS'")),
                      
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
                          h2("Videomängude müügid žanri ja platvormi järgi", style = "font-family: 'Comic Sans MS'"),
                          p("Look at this ...graph"),
                          plotOutput("plot_sales")
                        )
                      )      
             ),
             tabPanel("Soe kaart",
                      titlePanel(h1("Rakendus videomängude müükide visualiseerimiseks", style = "font-family: 'Comic Sans MS'")),
                      
                      sidebarLayout(
                        sidebarPanel(
                          
                          checkboxGroupInput("selected_platforms",
                                             strong("Vali platvorm"),
                                             choices = levels(video_games$Platform),
                                             selected = "PC")
                        ),
                        
                        mainPanel(
                          h2("Videomängude müügid žanri ja platvormi järgi", style = "font-family: 'Comic Sans MS'"),
                          p("Heatmap application here"),
                          plotOutput("plot_heatmap")
                        )
                      )      
             ),
             tabPanel("Andmestiku ülevaade",
                      titlePanel(h1("Rakendus videomängude müükide visualiseerimiseks", style = "font-family: 'Comic Sans MS'")),
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
