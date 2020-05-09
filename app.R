library(ggplot2)
library(dplyr)
library(shiny)
library(rstudioapi)
library(pheatmap)
library(shinythemes) #For stylization

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
  filter(Genre != "" & Genre != " " & Platform %in% mainstream_platforms) 

#TODO: Transform user_score to same scale that critic_score has
video_games$User_Score <- as.numeric(video_games$User_Score)

#Reset the levels
video_games$Platform = droplevels(video_games$Platform)

#platforms = c('Wii', 'PS2')
#Define linegraph plot for video game sales
plot_sales = function(video_games, genre, platforms){
  video_games %>% 
    filter(Genre==genre & Platform %in% platforms) %>% 
    group_by(Year_of_Release, Platform) %>% 
    summarise(Global_Sales = sum(Global_Sales)) %>% 
    filter(Year_of_Release!='N/A') %>% 
    ggplot(aes(x=Year_of_Release, y=Global_Sales, group=Platform, color=Platform)) +
    geom_line() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))+
    ggtitle("Müüdud mängude koguarv valitud aastate lõikes")+
    xlab("Aasta")+
    ylab("Müük")+
    guides(fill=guide_legend(title="Platvorm"))
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
    scale_fill_gradient(low="white", high = "#a31000")+
    xlab("Platvorm")+
    ylab("Žanr")+
    guides(fill=guide_legend(title="Üleilmsed müügid"))
}

# Define UI for application
# Good example of navbar usage here: https://shiny.rstudio.com/gallery/navbar-example.html
ui <- fluidPage(
  theme = shinytheme("united"), #http://rstudio.github.io/shinythemes/
  
  navbarPage("Navigatsiooniriba",
             tabPanel("Avaleht",
                      titlePanel(h1("Rakendus videomängude müükide visualiseerimiseks")),
                      
                      uiOutput("img"),
                      
                      mainPanel(
                        h2("Projekti kirjeldus"),
                        p('Oleme noored andmeteadusehuvilised Tartu Ülikooli tudengid Ilmar Uduste ja Kai Budrikas ning otsustasime 
                          aine "Statistiline andmeteadus ja visualiseerimine" raames uurida erinevate videomängude müüke.
                          Andmestik, mis pärineb Kaggle-st, koosneb 16 tunnusest ja pea 16 000 erinevast mängust.'),
                        p('Selleks, et oma andmeid kenasti visualiseerida, koostasime 100 002 interaktiivset visualiseeringut, 
                            mida võite uurida järgmistelt lehekülgedelt.') #Xd palun töötle mind
                      )
                      
             ),
             
             tabPanel("Andmestiku ülevaade",
                      titlePanel(h1("Rakendus videomängude müükide visualiseerimiseks")),
                      p(tags$a(
                        href="https://www.kaggle.com/rush4ratio/video-game-sales-with-ratings", "Videomängude andmestik"), 
                        "on võetud andmeteadust propageerivalt lehelt Kaggle, kus jagatakse andmestikke
                        ning erinevaid töövihikud, mis põhinevad jagatavatel andmestikel."),
                      p("Allpool on kuvatud andmestiku tunnused:"),
                      verbatimTextOutput("colnames"),
                      p("Esialgne andmestik koosneb 16 tunnusest ning 16 719 kirjest, mida omakorda filtreeritakse, et
                        visualiseerimiseks kasutatav andmestik vastaks teatud nõutele:"),
                      tags$ol(
                        tags$li("Filtreeritakse välja mänguplatvormid, mida ei peeta peavoolu omadeks. 
                                Kasutatavate mänguplatvormide hulk on järgmine: ",
                                verbatimTextOutput("platforms")),
                        tags$li("Tunnus ", tags$b("User_score"), " ei ole õiges andmetüübiga, seega see teisendatakse "
                                , tags$b("numeric"), " tüübiks."),
                        tags$li("Tunnus ", tags$b("Genre"), " omab tühjasid väärtuseid (kuid mitte NA!), seega filtreeritakse mängud,
                                millel on žanri väli tühi.")
                      ),
                      p("Pärast andmete korrastamist jääb andmestikku 15 684 kirjet. 
                        Allpool on näha näiteid/kokkuvõtteid mõningatest tunnustest:"),
                      verbatimTextOutput("summary")
             ),
          
             tabPanel("Joondiagramm",
                      
                      titlePanel(h1("Rakendus videomängude müükide visualiseerimiseks")),
                      
                      sidebarLayout(
                        sidebarPanel(
                          
                          selectInput("genre",
                                      strong("Vali žanr"),
                                      choices = levels(video_games$Genre),
                                      selected = "Sports"),
                          
                          
                          checkboxGroupInput("selected_platformss",
                                             strong("Vali platvormid"),
                                             choices = levels(video_games$Platform),
                                             selected = "PC")
                        ),
                        
                        mainPanel(
                          h2("Videomängude müügid aastate lõikes"),
                          p("Selleks, et paremini aru saada, kuidas on videomängude müük läbi aastate muutunud, 
                            koostasime joondiagrammi vastavalt valitud žanrile ja platvormidele."),
                          plotOutput("plot_sales"),
                          p("Platvormidega katsetades ilmneb, et tuntumad platvormid müüvad paremini kui teised lol no shizzle
                            Ilmar tule appi")
                        )
                      )      
             ),
             tabPanel("Soe kaart",
                      titlePanel(h1("Rakendus videomängude müükide visualiseerimiseks")),
                      
                      sidebarLayout(
                        sidebarPanel(
                          
                          checkboxGroupInput("selected_platforms",
                                             strong("Vali platvormid"),
                                             choices = levels(video_games$Platform),
                                             selected = "PC")
                        ),
                        
                        mainPanel(
                          h2("Videomängude müügid žanri ja platvormi järgi"),
                          p("Erinevate platvormide žanrite müükide uurimiseks lõime heatmapi,
                            kuhu saab valida endale meelepäraseid platvorme."),
                          plotOutput("plot_heatmap"),
                          p("Mdea no")
                        )
                      )      
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
    
    updateCheckboxGroupInput(session, "selected_platformss",
                             label = "Vali platvormid visualiseerimiseks",
                             choices = levels(video_games$Platform),
                             selected = "PC")
    
    updateCheckboxGroupInput(session, "selected_platforms",
                             label = "Vali platvormid visualiseerimiseks",
                             choices = levels(video_games$Platform),
                             selected = "PC")
  })
  output$img <- renderUI({
    tags$img(src = "https://cdn.pixabay.com/photo/2016/04/16/09/03/video-game-1332694_1280.png", height="100%", width="100%")
  })
  
  output$plot_sales <- renderPlot({
    plot_sales(video_games, input$genre, input$selected_platformss)
  })
  
  output$summary <- renderPrint({
    summary(video_games)
  })
  
  output$colnames <- renderPrint({
    colnames(video_games)
  })
  
    output$platforms <- renderPrint({
      mainstream_platforms <- c("XB", "X360", "WiiU", "Wii",
                                "SNES", "PSP", "PS4", "PS3", "PS2", 
                                "PS", "PC", "NES", "N64", "GEN", 
                                "GC", "GBA", "DS", "3DS", "2600")
      mainstream_platforms
  })
    
  output$plot_heatmap <- renderPlot({
    plot_heatmap(platforms = input$selected_platforms)
  })
  

}

# Run the application 
shinyApp(ui = ui, server = server)
