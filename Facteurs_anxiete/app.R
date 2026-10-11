require(shiny)
require(readr)
require(tidyverse)
require(shinydashboard)
require(ggplot2)
require(plotly)
require(corrplot)
require(dplyr)
require(tidyr)
require(skimr)
require(DT)
require(GGally)
require(ggcorrplot)
require(car)
require(glmnet)
require(randomForest)
require(FactoMineR)
require(factoextra)
require(effectsize)
require(scales)
require(ggh4x)
# un graphique fixe possible a mettre en pdf pour expliquer le choix et en quoi c'est interessant

donnees <- read_csv(
  "data/enhanced_anxiety_dataset.csv",
  show_col_types = FALSE
)
donnees <- donnees |>
  mutate(
    across(
      where(is.character),
      as.factor
    )
  )
names(donnees) <- c(
  "Âge",
  "Sexe",
  "Profession",
  "Heures de sommeil",
  "Activité physique",
  "Caféine consommée",
  "Consommation d'alcool",
  "Tabagisme",
  "Antécédents familiaux d'anxiété",
  "Niveau de stress",
  "Fréquence cardiaque",
  "Fréquence respiratoire",
  "Niveau de transpiration",
  "Vertiges",
  "Médication",
  "Séances de thérapie",
  "Événement de vie récent",
  "Qualité de l'alimentation",
  "Niveau d'anxiété"
)

source("R/page_accueil.R")
source("R/page_donnees.R")
source("R/page_structure.R")
source("R/page_modele.R")

ui <- dashboardPage(
  
  # --------------------------------------------------
  # HEADER
  # --------------------------------------------------
  
  dashboardHeader(
    
    title = tags$img(
      src = "logo.png",
      height = "45px",
      style = "margin-top: 2px;"
    )
    ,
    
    titleWidth = 230,
    tags$li(
      class = "dropdown",
      
      tags$a(
        href = "#",
        class = "dropdown-toggle",
        `data-toggle` = "dropdown",
        
        icon("users"),
        
        span(
          class = "label label-primary",
          "3"
        )
      ),
      
      tags$ul(
        class = "dropdown-menu",
        
        tags$li(
          tags$h4("Créateurs")
        ),
        
        tags$li(
          tags$a(icon("user"),"Anton Tyurin")
        ),
        
        tags$li(
          tags$a(icon("user"),"Augustin Barnerias")
        ),
        
        tags$li(
          tags$a(icon("user"),"Ines Bouzida")
        )
      )
    )
  )
  ,
  
  
  # --------------------------------------------------
  # MENU GAUCHE
  # --------------------------------------------------
  
  dashboardSidebar(
    collapsed = F,
    sidebarMenu(id = "tabs",
      
      menuItem(
        "Accueil",
        tabName = "accueil",
        icon = icon("house")
      ),
      
      menuItem(
        "Données",
        tabName = "donnees",
        icon = icon("database")
      ),
      
      menuItem(
        "Structure",
        tabName = "structure",
        icon = icon("chart-column")
      ),
      
      menuItem(
        "Modélisation",
        tabName = "modele",
        icon = icon("calculator")
      )
      
    )
  ),
  
  
  # --------------------------------------------------
  # CONTENU
  # --------------------------------------------------
  
  dashboardBody(
    
    tags$head(
      tags$link(
        rel = "stylesheet",
        type = "text/css",
        href = "style.css"
      )
    ),
    
    tabItems(
      
      tabItem(
        tabName = "accueil",
        page_accueil_ui("accueil")
      ),
      
      tabItem(
        tabName = "donnees",
        page_donnees_ui("donnees")
      ),
      
      tabItem(
        tabName = "structure",
        page_structure_ui("structure")
      ),
      
      tabItem(
        tabName = "modele",
        page_modele_ui("modele")
      )
      
    )
  )
)


server <- function(input, output, session) {
  
  navigation_accueil <- page_accueil_server("accueil")
  
  observeEvent(navigation_accueil(), {
    
    destination <- navigation_accueil()$destination
    
    updateTabItems(
      session = session,
      inputId = "tabs",
      selected = destination
    )
    
  }, ignoreInit = TRUE)
  
  page_donnees_server(
    "donnees",
    donnees = donnees
  )
  
  page_structure_server(
    "structure",
    donnees = donnees
  )
  
  page_modele_server(
    "modele",
    donnees = donnees
  )
}

shinyApp(ui, server)