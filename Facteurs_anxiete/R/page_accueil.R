
page_accueil_ui <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    
    # ============================================================
    # CSS spécifique à la page d'accueil
    # ============================================================
    
    tags$head(
      tags$style(HTML("
        .accueil {
          max-width: 1150px;
          margin: 0 auto;
          padding: 10px 25px 40px 25px;
        }

        .accueil-titre {
          text-align: center;
          margin-top: 10px;
          margin-bottom: 8px;
          font-size: 34px;
          font-weight: 700;
          color: #2c3e50;
        }

        .accueil-sous-titre {
          text-align: center;
          font-size: 17px;
          color: #6c757d;
          margin-bottom: 35px;
        }

        .accueil-section {
          margin-top: 30px;
          margin-bottom: 25px;
        }

        .accueil-section h2 {
          color: #2c3e50;
          font-size: 24px;
          font-weight: 600;
          margin-bottom: 12px;
          padding-bottom: 7px;
          border-bottom: 2px solid #3c8dbc;
        }

        .accueil-section p {
          font-size: 15px;
          line-height: 1.65;
          color: #444444;
          text-align: justify;
        }

        .problematique {
          background-color: #f5f8fa;
          border-left: 5px solid #3c8dbc;
          padding: 18px 22px;
          margin: 20px 0;
          border-radius: 3px;
          font-size: 16px;
          line-height: 1.6;
        }

        .problematique strong {
          color: #2c3e50;
        }

        .profil-box {
          padding: 18px;
          margin-top: 10px;
          border-radius: 5px;
          background-color: #f8f8f8;
          border: 1px solid #e5e5e5;
          min-height: 120px;
        }

        .profil-box h4 {
          margin-top: 0;
          font-weight: 600;
          color: #2c3e50;
        }

        .profil-box p {
          text-align: left;
          margin-bottom: 0;
        }

        .plan-container {
          margin-top: 25px;
        }

        .plan-card {
          display: block;
          width: 100%;
          text-align: left;
          text-decoration: none !important;
          color: inherit !important;
          background-color: white;
          border: 1px solid #dddddd;
          border-radius: 6px;
          padding: 20px;
          margin-bottom: 15px;
          min-height: 135px;
          transition: all 0.2s ease;
          box-shadow: 0 1px 3px rgba(0,0,0,0.08);
          cursor: pointer;
        }

        .plan-card:hover,
        .plan-card:focus {
          border-color: #3c8dbc;
          box-shadow: 0 4px 12px rgba(0,0,0,0.12);
          transform: translateY(-2px);
          outline: none;
        }

        .plan-numero {
          display: inline-block;
          width: 32px;
          height: 32px;
          line-height: 32px;
          text-align: center;
          border-radius: 50%;
          background-color: #3c8dbc;
          color: white;
          font-weight: bold;
          margin-right: 10px;
          text-align: center;
        }

        .plan-titre {
          font-size: 18px;
          font-weight: 600;
          color: #2c3e50;
        }

        .plan-description {
          display: block;
          margin-top: 12px;
          margin-left: 45px;
          color: #666666;
          font-size: 14px;
          line-height: 1.5;
        }

        .avertissement {
          margin-top: 25px;
          padding: 15px 18px;
          background-color: #fafafa;
          border: 1px solid #dddddd;
          border-radius: 4px;
          font-size: 13px;
          color: #666666;
          line-height: 1.5;
        }
      "))
    ),
    
    # ============================================================
    # CONTENU
    # ============================================================
    
    div(
      class = "accueil",
      
      h1(
        class = "accueil-titre",
        "Anxiété, comportements et professions"
      ),
      
      div(
        class = "accueil-sous-titre",
        "Exploration, analyse et modélisation d'un jeu de données"
      ),
      
      # ----------------------------------------------------------
      # Présentation du projet
      # ----------------------------------------------------------
      
      div(
        class = "accueil-section",
        
        h2("Présentation du projet"),
        
        p(
          "L'objectif initial de ce projet était d'étudier les liens entre ",
          "différents facteurs comportementaux et le niveau d'anxiété des ",
          "personnes appartenant à différentes catégories professionnelles."
        ),
        
        p(
          "Nous avons choisi un jeu de données contenant des informations ",
          "individuelles telles que l'âge, le sommeil, l'activité physique, ",
          "la consommation de caféine et d'alcool, le tabagisme, le niveau ",
          "de stress ou encore la fréquence cardiaque. Ces variables permettent ",
          "d'aborder l'anxiété à travers des facteurs relativement simples à ",
          "interpréter, tout en prenant en compte la profession des individus."
        ),
        
        p(
          "Le jeu de données est présenté comme un échantillon représentatif ",
          "et les données sont anonymisées. Cependant, les distributions réelles ",
          "des variables et la méthode exacte utilisée pour constituer l'échantillon ",
          "ne sont pas connues. Cette limite est importante pour interpréter ",
          "les résultats obtenus."
        )
      ),
      
      # ----------------------------------------------------------
      # Découverte de la structure
      # ----------------------------------------------------------
      
      div(
        class = "accueil-section",
        
        h2("Une exploration qui fait évoluer la problématique"),
        
        p(
          "En explorant les données, nous avons constaté plusieurs particularités ",
          "qui nous ont amenés à nous interroger sur leur construction."
        ),
        
        p(
          "Pour plusieurs variables, les statistiques descriptives du niveau ",
          "d'anxiété semblaient faire apparaître une structure très régulière. ",
          "Les moyennes et les quartiles ne prenaient notamment que quelques ",
          "valeurs récurrentes, avec une séparation particulièrement visible ",
          "entre deux niveaux de profil."
        ),
        
        div(
          class = "problematique",
          
          tags$strong("Nouvelle problématique : "),
          
          "comment les données sont-elles structurées et quels éléments ",
          "permettent d'expliquer les profils observés ?"
        ),
        
        p(
          "Nous ne connaissons pas la méthode exacte utilisée pour générer ",
          "ou sélectionner les données. Notre objectif est donc d'explorer ",
          "leurs propriétés statistiques et de rechercher des indices permettant ",
          "de mieux comprendre leur structure, sans prétendre reconstituer ",
          "avec certitude leur méthode de génération."
        )
      ),
      
      # ----------------------------------------------------------
      # Deux profils
      # ----------------------------------------------------------
      
      div(
        class = "accueil-section",
        
        h2("Deux profils d'anxiété"),
        
        p(
          "L'exploration des données a rapidement fait apparaître une séparation ",
          "entre deux grands groupes de niveaux d'anxiété."
        ),
        
        fluidRow(
          
          column(
            width = 6,
            
            div(
              class = "profil-box",
              
              h4("Anxiété faible à modérée"),
              
              p(
                strong("Niveaux 1 à 7"),
                " : un premier groupe regroupant les niveaux d'anxiété ",
                "faibles à modérés."
              )
            )
          ),
          
          column(
            width = 6,
            
            div(
              class = "profil-box",
              
              h4("Anxiété élevée"),
              
              p(
                strong("Niveaux 8 à 10"),
                " : un second groupe correspondant aux individus ",
                "présentant les niveaux d'anxiété les plus élevés."
              )
            )
          )
        ),
        
        p(
          style = "margin-top: 20px;",
          
          "Nous avons donc étudié ces deux groupes afin de déterminer quelles ",
          "caractéristiques les différencient et dans quelle mesure les variables ",
          "comportementales, physiologiques et sociodémographiques sont associées ",
          "à cette séparation."
        )
      ),
      
      # ----------------------------------------------------------
      # Modélisation
      # ----------------------------------------------------------
      
      div(
        class = "accueil-section",
        
        h2("Revenir à la problématique initiale"),
        
        p(
          "Malgré cette nouvelle problématique, notre objectif initial reste ",
          "central : comprendre quels facteurs comportementaux peuvent être ",
          "associés au niveau d'anxiété."
        ),
        
        p(
          "Nous avons donc construit un modèle de régression à partir d'un ",
          "ensemble réduit de variables facilement compréhensibles. L'objectif ",
          "est de proposer un modèle interprétable permettant d'estimer un ",
          "niveau d'anxiété à partir de caractéristiques simples renseignées ",
          "par l'utilisateur."
        ),
        
        p(
          "L'application permet ainsi d'explorer les données sous deux angles ",
          "complémentaires : comprendre la structure du jeu de données et utiliser ",
          "les résultats obtenus pour produire une estimation à partir de ",
          "caractéristiques individuelles."
        )
      ),
      
      # ----------------------------------------------------------
      # PLAN DE L'APPLICATION
      # ----------------------------------------------------------
      
      div(
        class = "accueil-section plan-container",
        
        h2("Plan de l'application"),
        
        p(
          "Vous pouvez suivre les différentes étapes de notre démarche ",
          "dans l'ordre ou accéder directement à la partie qui vous intéresse."
        ),
        
        # Carte 1 : Données
        
        actionButton(
          inputId = ns("aller_donnees"),
          label = tagList(
            span(class = "plan-numero", "1"),
            span(class = "plan-titre", "Données"),
            span(
              class = "plan-description",
              "Présentation du jeu de données, de ses variables et de leurs ",
              "distributions. Cette partie permet d'explorer les observations ",
              "et leurs principales caractéristiques."
            )
          ),
          class = "plan-card"
        ),
        
        # Carte 2 : Structure
        
        actionButton(
          inputId = ns("aller_structure"),
          label = tagList(
            span(class = "plan-numero", "2"),
            span(class = "plan-titre", "Structure"),
            span(
              class = "plan-description",
              "Exploration statistique des données, recherche de relations entre ",
              "les variables et étude de la structure des profils d'anxiété."
            )
          ),
          class = "plan-card"
        ),
        
        # Carte 3 : Modélisation
        
        actionButton(
          inputId = ns("aller_modele"),
          label = tagList(
            span(class = "plan-numero", "3"),
            span(class = "plan-titre", "Modélisation"),
            span(
              class = "plan-description",
              "Présentation du modèle de régression et des variables utilisées. ",
              "Cette partie permet également d'obtenir ",
              "une estimation du niveau d'anxiété selon le modèle."
            )
          ),
          class = "plan-card"
        )
      ),
      
      # ----------------------------------------------------------
      # Avertissement
      # ----------------------------------------------------------
      
      div(
        class = "avertissement",
        
        tags$strong("Attention : "),
        
        "l'estimation fournie par le modèle est issue de ce jeu de données ",
        "et ne constitue pas un diagnostic médical. Elle doit être interprétée ",
        "comme un résultat statistique exploratoire."
      )
    )
  )
}


# ==============================================================
# SERVER
# ==============================================================
page_accueil_server <- function(id) {
  
  moduleServer(id, function(input, output, session) {
    
    navigation <- reactiveVal(NULL)
    
    observeEvent(input$aller_donnees, {
      navigation(list(
        destination = "donnees",
        evenement = input$aller_donnees
      ))
    }, ignoreInit = TRUE)
    
    observeEvent(input$aller_structure, {
      navigation(list(
        destination = "structure",
        evenement = input$aller_structure
      ))
    }, ignoreInit = TRUE)
    
    observeEvent(input$aller_modele, {
      navigation(list(
        destination = "modele",
        evenement = input$aller_modele
      ))
    }, ignoreInit = TRUE)
    
    navigation
    
  })
}