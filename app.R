library(ggplot2)
library(dplyr)
library(shiny)
library(rstudioapi)
library(pheatmap)
library(shinythemes) #For stylization
library(shinyWidgets) #For more elegant category selection

#Set Working Directory to the folder where this script is located
setwd(dirname(getActiveDocumentContext()$path))

#Load the DataFrame
video_games <- read.csv("Video_Games_Sales_as_at_22_Dec_2016.csv")

#Define mainstream platforms
mainstream_platforms <- c("XB", "X360", "WiiU", "Wii",
                          "SNES", "PSP", "PS4", "PS3", "PS2", 
                          "PS", "PC", "NES", "N64", "GEN", 
                          "GC", "GBA", "DS", "3DS", "2600")

# Defining dictionary for column name to variable name
video_games_options <- c(
  "Year of Release" = "Year_of_Release",
  "North American Sales" = "NA_Sales", 
  "European Sales" = "EU_Sales",
  "Japanese Sales" = "JP_Sales", 
  "Sales in other regions" = "Other_Sales", 
  "Global Sales" = "Global_Sales",
  "Critic Score" = "Critic_Score", 
  "User Score" = "User_Score",
  "Genre" = "Genre",
  "Rating" = "Rating",
  "Platform" = "Platform"
)

#Mainstream platform filtering
video_games <- video_games %>% 
  filter(Genre != "" & Genre != " " & Platform %in% mainstream_platforms) 

# Transform user_score to same scale that critic_score has
video_games$User_Score <- as.numeric(video_games$User_Score)

#Reset the levels
video_games$Platform = droplevels(video_games$Platform)

#####  Color Palette by Paletton.com
#####  Palette URL: http://paletton.com/#uid=70d0u0krBtfh7Btm6uVu1nxyMi-

#platforms = c('Wii', 'PS2')
#Define linegraph plot for video game sales
plot_sales = function(video_games, genres, platforms){
  video_games %>% 
    filter(Genre %in% genres & Platform %in% platforms) %>% 
    group_by(Year_of_Release, Platform) %>% 
    summarise(Global_Sales = sum(Global_Sales)) %>% 
    filter(Year_of_Release!='N/A') %>% 
    mutate(Platvorm = Platform) %>% 
    ggplot(aes(x=Year_of_Release, y=Global_Sales, group=Platform, color=Platvorm)) +
    geom_ribbon(aes(ymin=0, ymax= Global_Sales, fill=Platvorm), alpha=0.2) + #TODO: See if this can be improved. Right now the filling is quite ugly. Turn down opacity somehow?
    #geom_line() + #TODO: Play around with this and see what works better
    theme(axis.text.x = element_text(angle = 90, hjust = 1))+
    ggtitle("Müüdud mängude koguarv valitud aastate lõikes")+
    xlab("Aasta")+
    ylab("Müüdud mängude arv, 10^6")
}

#Print out Sales for each platform

text_sales = function(platforms) {
  
  text <- c()
  
  for (platform in platforms) {
    correct_platform <- video_games %>% 
      filter(Platform == platform)
    
    sales <- sum(correct_platform$Global_Sales)
    plt_sales = paste(toString(platform), " Games Global Sales:", sep=" ") #a little kilplaneism never hurt nobody
    plt_sales = paste(plt_sales, toString(sales), sep=" ")
    plt_sales = paste(plt_sales, "M", sep="")
    text <- append(text, plt_sales)
  }
  
  return(text)
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
    scale_fill_gradient(low="white", high = "#E99020")+
    xlab("Platvorm")+
    ylab("Žanr")+
    guides(fill=guide_legend(title="Üleilmsed müügid sõltuvalt
platvormist ja žanrist, 10^6"))
}

#Visualise highest regional/global sales by publisher
#TODO: Fill the global sales bar with regional sales perhaps?
plot_publishers = function(genres, platforms, n) {
  video_games %>% 
    filter(Genre %in% genres,
           Platform %in% platforms) %>%
    group_by(Publisher) %>% 
    summarise(Global_Sales = sum(Global_Sales)) %>% 
    arrange(desc(Global_Sales)) %>% 
    top_n(n, Global_Sales) %>% 
    ggplot(aes(x=reorder(Publisher, Global_Sales), y=Global_Sales)) + 
    geom_bar(stat="identity", fill = "#1C6595") +
    geom_text(aes(label=round(Global_Sales, 1)), hjust=1.2, color="white", size=3.5)+
    theme(axis.text.x = element_text(angle = 0, hjust = 1))+
    ggtitle("Müüdud mängud")+
    xlab("Jaotaja")+
    ylab("Müüdud mängude arv, 10^6")+
    coord_flip()
}

#Visualise highest regional/global sales by game
#TODO: Fill the global sales bar with regional sales perhaps?
plot_games = function(genres, platforms, n) {
  video_games %>% 
    filter(Genre %in% genres,
           Platform %in% platforms) %>%
    arrange(desc(Global_Sales)) %>% 
    top_n(n, Global_Sales) %>% 
    ggplot(aes(x=reorder(Name, Global_Sales), y=Global_Sales)) + 
    geom_bar(stat="identity", fill = "#16A25F") +
    geom_text(aes(label=round(Global_Sales, 1)), hjust=1.2, color="white", size=3.5)+
    theme(axis.text.x = element_text(angle = 0, hjust = 1))+
    ggtitle("Müüdud mängud")+
    xlab("Mäng")+
    ylab("Müüdud mängude arv, 10^6")+
    coord_flip()
}

plot_cor = function(column_x, column_y, column_color) {
  
  new_video_games <- video_games
  
  tilt <- 0
  
  if (column_x == "Year of Release") {
    tilt <- 45
  }
  
  if (column_color == "Year of Release") {
    new_video_games <- as.numeric(video_games$Year_of_Release)
  }
  
  video_games %>% 
    filter(User_Score != 1 & !is.na(User_Count)) %>% 
    ggplot(aes_string(x=column_x, y=column_y)) + 
    geom_point(aes_string(color=column_color)) + 
    geom_smooth() + 
    theme(axis.text.x = element_text(angle = tilt, hjust = 1)) +
    guides(fill=guide_legend(title=video_games_options[column_color])) +
    labs(x = names(video_games_options[which(video_games_options == column_x)]),
         y = names(video_games_options[which(video_games_options == column_y)]))
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
                          aine "Statistiline andmeteadus ja visualiseerimine" raames uurida erinevate videomängude müüke.'),
                        p(tags$a(
                          href="https://www.kaggle.com/rush4ratio/video-game-sales-with-ratings", "Videomängude andmestik"), 
                          "on võetud andmeteadust propageerivalt lehelt Kaggle, kus jagatakse andmestikke
                        ning neil põhinevaid töövihikuid. Andmestik on aastast 2016 ning koosneb 16 tunnusest ja pea 16 000 erinevast mängust."),
                        p('Selleks, et oma andmeid kenasti visualiseerida, koostasime mitu interaktiivset visualiseeringut, 
                            mida võite uurida järgmistelt lehekülgedelt.')
                      )
                      
             ),
             
             tabPanel("Andmestiku ülevaade",
                      titlePanel(h1("Rakendus videomängude müükide visualiseerimiseks")),
                      p("Allpool on meie andmestiku tunnused:"),
                      verbatimTextOutput("colnames"),
                      p("Andmestiku kohandamiseks filtreerisime vaatluseid, et
                        visualiseerimiseks kasutatav see vastaks teatud nõutele:"),
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
                      verbatimTextOutput("summary"),
                      p('Esmase analüüsi käigus võib märgata, et kõige enam leidub andmestikus 
                        kirjeid mängu ', tags$b('"Need for Speed: Most Wanted"'), ' kohta, kusjuures top3-e kuulub ka ',
                        tags$b('"Ratatouille"'), '. Kõige rohkem leidub andmetes ', tags$b("PS2"), ' mänge ning populaarseim žanr on ',
                        tags$b('Action.')),
                      p("Kõige rohkem on üht mängu üle terve maailma müüdud pea ", tags$b("83 miljonit ühikut"),
                        ", kusjuures Põhja-Ameerikas on see 41, Euroopas 29 ja ainuüksi Jaapanis 7 miljonit ühikut. 
                        Üleilmsete müükide alumine kvartiil on 0,06, mis tähendab seda, et 75% andmestikus olevatest mängudest igaüht 
                        on ostetud 60 000 kuni 82 530 000 tükki, ning seega on tegemist üpris menukate mängudega."),
                      p("Kriitikud on kõige paremateks mängudeks valinud ",
                        tags$b("„Grand Theft Auto IV“"), " ja ", tags$b("„Tony Hawk's Pro Skater 2“"), " skooriga ", tags$b("98/100"),
                      ' ja kõige kehvemaks on märgitud Deep Silveri mäng ', tags$b("„Ride to Hell“ (13/100)"), '. 
                        Kõige paremini on Metacriticu tellijad hinnanud üht mängu 97 palliga 100-st, kõige väiksem 
                        skoor on 1. Kõige rohkem on üht mängu Metacriticu tellijate poolt hinnatud ', tags$b("10 665 korda („The Witcher 3: Wild Hunt“).")),
                      p("Kõige rohkem leidub meie andmestikus ", tags$b("Ubisofti"), " mänge, temale järgnevad ",
                        tags$b("EA Canada"), " ja ", tags$b("EA Sports"), ". Enim on vaadeldavate mängude seas selliseid, 
                        mis on kõigile mõeldud (E-Everyone) ning sellele järgnevad teismelistele (T-Teens) ja täiskasvanute (M-Mature) 
                        mängud."),
                      p("Käesoleva programmi koostamisel avastasime, et kasutatav andmestik on mingil määral vigane. Siinkohal ei arvestata mängude digitaalse
                        müügiga ning loetakse vaid füüsilisi koopiaid, mis ei ole tänapäeval enam põhiviis mängude levitamiseks. Seega on TOP mängude hulgast puudu
                        maailma ühed suuremad mängud, nagu „Minecraft“, „Skyrim“ jms. Steami platvormi müüginumbreid ei võeta arvesse PC kategoorias.")
             ),
             
             tabPanel("Joondiagramm",
                      
                      titlePanel(h1("Rakendus videomängude müükide visualiseerimiseks")),
                      
                      sidebarLayout(
                        sidebarPanel(
                          
                          pickerInput(
                            inputId = "genre", label = "Vali žanr",
                            choices = levels(video_games$Genre),
                            options = list(`actions-box` = TRUE,
                                           `selected-text-format` = paste0("count = ", length(levels(video_games$Genre))),
                                           `count-selected-text` = "Kõik valikud",
                                           `select-all-text` = "Vali kõik",
                                           `deselect-all-text` = "Tühista valik"),
                            multiple = TRUE),
                          
                          
                          checkboxGroupInput("selected_platforms_line",
                                             strong("Vali platvormid"),
                                             choices = levels(video_games$Platform),
                                             selected = "PC")
                        ),
                        
                        mainPanel(
                          h2("Videomängude müügid aastate lõikes"),
                          p("Selleks, et paremini aru saada, kuidas on videomängude müük läbi aastate muutunud, 
                            koostasime joondiagrammi vastavalt valitud žanrile ja platvormidele."),
                          plotOutput("plot_sales"),
                          p("Siinkohal tasub kõige enam võrrelda platvorme, mis on kas samas generatsioonis (nt 7. generatsioon: Wii, Xbox 360 ja PS3), et näha, missugune konsool
                          võitis \"konsoolisõja\", 
                            või on sama firma poolt välja antud konsoolid, et näha generatsioonide vaheldumist."),
                          verbatimTextOutput("text_sales")
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
                          plotOutput("plot_heatmap")
                        )
                      )      
             ),
             tabPanel("Mängud ja jaotajad",
                      titlePanel(h1("Rakendus videomängude müükide visualiseerimiseks")),
                      
                      sidebarLayout(
                        sidebarPanel(
                          checkboxGroupInput("selected_platforms_games",
                                             strong("Vali platvormid"),
                                             choices = levels(video_games$Platform),
                                             selected = "PC"),
                          checkboxInput('all_platforms_games', 'Kõik valikud'),
                          pickerInput(
                            inputId = "genre_games", label = "Vali žanr",
                            choices = levels(video_games$Genre),
                            options = list(`actions-box` = TRUE,
                                           `selected-text-format` = paste0("count = ", length(levels(video_games$Genre))),
                                           `count-selected-text` = "Kõik valikud",
                                           `select-all-text` = "Vali kõik",
                                           `deselect-all-text` = "Tühista valik"),
                            multiple = TRUE),
                          #selectInput("genre_games",
                          #            strong("Vali žanr"),
                          #            choices = levels(video_games$Genre),
                          #            selected = "Sports"),
                          sliderInput("n_games", 
                                      strong("Mängude/jaotajate arv"),
                                      min = 5, max = 30,
                                      value = 10)
                        ),
                        
                        mainPanel(
                          h2("TOP mängud"),
                          plotOutput("games"),
                          h2("TOP jaotajad"),
                          plotOutput("publishers")
                        )
                      )
             ),
             
             tabPanel("Korellatsioonid",
                      titlePanel(h1("Rakendus videomängude müükide visualiseerimiseks")),
                      
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("selected_column_x",
                                      "Vali horisontaaltelg:",
                                      choices = c("Year of Release", "North American Sales", "European Sales",
                                                  "Japanese Sales", "Sales in other regions", "Global Sales",
                                                  "Critic Score", "User Score"),
                                      selected = "User Score"
                                      ),
                          selectInput("selected_column_y",
                                      "Vali vertikaaltelg:",
                                      choices = c("Year of Release", "North American Sales", "European Sales",
                                                  "Japanese Sales", "Sales in other regions", "Global Sales",
                                                  "Critic Score", "User Score"),
                                      selected = "Critic Score"
                                      ),
                          selectInput("selected_column_color",
                                      "Vali värv:",
                                      choices = c("Platform", "Year of Release", "Genre",
                                                  "North American Sales", "European Sales", "Rating",
                                                  "Japanese Sales", "Sales in other regions", "Global Sales",
                                                  "Critic Score", "User Score"),
                                      selected = "Rating"
                          )
                        ),
                        mainPanel(
                          h1("Liides visualiseerimaks seoseid erinevate tunnuste vahel"),
                          plotOutput("cor"),
                          p("Käesoleval lehel on võimalik visualiseerida seoseid andmestiku kahe tunnuse vahel ning valida värviks mõni kolmas tunnus.
                            Huvitav on vaadata selliseid seoseid nagu User Score vs Critic Score (mille graafik võib viidata asjaolule, et mängukriitikud
                            ja mängijaskond ei pruugi alati nõustuda) või Japanese Sales vs Global Sales (mille graafik viitab Jaapani nišiturule, st Jaapani
                            mängud ei pruugi globaalselt hästi müüa, kuigi seda teevad Euroopas ja USAs populaarsed mängud).")
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
    
    updateCheckboxGroupInput(session, "selected_platforms_line",
                             label = "Vali platvormid visualiseerimiseks",
                             choices = levels(video_games$Platform),
                             selected = "PC")
    
    updateCheckboxGroupInput(session, "selected_platforms",
                             label = "Vali platvormid visualiseerimiseks",
                             choices = levels(video_games$Platform),
                             selected = "PC")
    
    updateCheckboxGroupInput(session, "selected_platforms_games",
                             label = "Vali platvormid visualiseerimiseks",
                             choices = levels(video_games$Platform),
                             selected = if (input$all_platforms_games) levels(video_games$Platform) else "PC")
    
    updateSelectizeInput(session, "genre_games",
                         label = "Vali žanr",
                         choices = levels(video_games$Genre),
                         selected = "Sports")
    
    updateSelectizeInput(session, "selected_column_x",
                         label = "Vali horisontaaltelg:",
                         choices = c("North American Sales", "European Sales",
                                     "Japanese Sales", "Sales in other regions", "Global Sales",
                                     "Critic Score", "User Score"),
                         selected = "User Score")
    
    updateSelectizeInput(session, "selected_column_y",
                         label = "Vali vertikaaltelg:",
                         choices = c("North American Sales", "European Sales",
                                     "Japanese Sales", "Sales in other regions", "Global Sales",
                                     "Critic Score", "User Score"),
                         selected = "Critic Score")
    
    updateSelectizeInput(session, "selected_column_color",
                         label = "Vali värv:",
                         choices = c("Platform", "Year of Release", "Genre",
                                     "North American Sales", "European Sales", "Rating",
                                     "Japanese Sales", "Sales in other regions", "Global Sales",
                                     "Critic Score", "User Score"),
                         selected = "Rating")
    
  })
  output$img <- renderUI({
    tags$img(src = "https://cdn.pixabay.com/photo/2016/04/16/09/03/video-game-1332694_1280.png", height="100%", width="100%")
  })
  
  output$plot_sales <- renderPlot({
    plot_sales(video_games, input$genre, input$selected_platforms_line)
  })
  
  output$text_sales <- renderPrint({
    text_sales(input$selected_platforms_line)
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
  
  output$games <- renderPlot({
    plot_games(platforms = input$selected_platforms_games, genres = input$genre_games, n = input$n_games)
  })
  
  output$publishers <- renderPlot({
    plot_publishers(platforms = input$selected_platforms_games, genres = input$genre_games, n = input$n_games)
  })
  
  output$cor <- renderPlot({

    plot_cor(column_x = video_games_options[input$selected_column_x], 
             column_y = video_games_options[input$selected_column_y], 
             column_color = video_games_options[input$selected_column_color])
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

