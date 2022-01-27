
install.packages("anyLib")
anyLib::anyLib(c("shiny", "shinydashboard", "shinyWidgets", "DT", "plotly", "ggplot2", "googleVis", "colourpicker"))

list.of.packages <- c("ggplot2", 
                      "DT", 
                      "GGally",
                      "psych",
                      "Hmisc",
                      "MASS",
                      "tabplot","plotly","shinythemes","shinydashboard","shiny")
# importer les scripts
source("fonctions.R")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# load all these
lapply(list.of.packages, require, character.only = TRUE)


library(shiny)
library(anyLib)
library(shinydashboard)
library(forcats)
library(ggplot2)
library(hrbrthemes)
library(ggridges)
library(dplyr)
library(tidyr)
library(viridis)
library(cartography)
library(xts)   
library(forcats)
library(geojson)
library(geojsonio)
library(lubridate)

ui <- dashboardPage(
  dashboardHeader(title = "Typologie des régions en fonction de l'évolution du covid-19 en France"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Lecture des données", tabName = "readData", icon = icon("readme")),
      menuItem("Inspection des données", tabName = "inspection", icon = icon("readme")),
      menuItem("Tableau de bord", tabName = "TB", icon = icon("poll")),
      menuItem("Tableau de bord #2", tabName = "TB2", icon = icon("poll")),
      menuItem("Tableau de bord #3", tabName = "TB3", icon = icon("poll")),
      menuItem("Tableau de bord #4", tabName = "TB4", icon = icon("poll")),
      menuItem("Tableau de bord Cartes", tabName = "TB5", icon = icon("poll"))
      
    )
  ),
  dashboardBody(
    tabItems(
      # Read data
      tabItem(tabName = "readData",
              h1("Typologie des régions en fonction de l'évolution du covid-19 en France"),
              h2("Lecture des données"),
              h5("Le fichier étudié (de type CSV) sur le CODIV19 en France concerne les arrivées (par région) aux urgences depuis le 24 février, il est dans le dossier joint. Vous pouvez également télécharger le fichier à jour de la base de données sur le COVID-19 choisi ", a("FranceRégion-covid19.csv", href = "https://static.data.gouv.fr/resources/donnees-des-urgences-hospitalieres-et-de-sos-medecins-relatives-a-lepidemie-de-covid-19/20200420-191827/sursaud-covid19-quotidien-2020-04-20-19h18-region.csv")),
              tags$hr(),
              fileInput("dataFile",label = NULL,
                        buttonLabel = "Télécharger le fichier",
                        placeholder = "Aucun fichier",
                        accept = c(
                          'text/csv',
                          'text/comma-separated-values',
                          'text/tab-separated-values',
                          'text/plain',
                          '.csv',
                          '.tsv'
                        )
                        ),
              
              h3("Sélectionner les paramètres du fichier CSV"),
              
              # Input: Checkbox if file has header
              radioButtons(inputId = "header", 
                           label = "En-tête",
                           choices = c("Les colonnes ont des en-têtes" = "Yes",
                                       "Les colonnes n'ont pas en-têtes" = "No"),
                           selected = "Yes", inline=T),
              
              # Input: Select separator ----
              radioButtons(inputId = "sep", 
                           label = "Separateur",
                           choices = c('Virgule' = ",",
                                       'Point-Virgule' = ";",
                                       'Tabulation' = "\t"),
                           selected = ",", inline=T),
              
              # Input: Select quotes ----
              radioButtons(inputId = "quote", 
                           label= "Guillemet",
                           choices = c("Rien" = '',
                                       'Double'='"',
                                       'Simple'="'"),
                           selected = '"', inline=T),
              h4("Après avoir importer la base de données, cliquez sur l'onglet -Inspection des données-")
              
      ), #fin tab readData
      
      
      tabItem(tabName = "inspection",
              h1("Inspection des données"),
              
              tags$hr(),

              h3("Voici les données brutes du fichier CSV"),
              DT::dataTableOutput('contents'),
              tags$hr(),
              h3("Voici un résumé des données"),
              h3("Il faudra quelques secondes pour faire apparaître le tableau, utile pour explorer les relations entre les variables, pour découvrir des modèles de données étranges et pour vérifier l'occurrence et la sélectivité des valeurs manquantes."),
              tableOutput('summary')
              
      ),        
     
      
      # Tableau de bord
      tabItem(tabName = "TB",
              h1("Typologie des Régions en fonction la population total des urgences de france"),
              h3("Il faudra quelques secondes pour faire apparaître les graphiques"),
              plotOutput("tableplot1"),
              plotOutput("tableplot2"),
              plotOutput("tableplot3"),
              plotOutput("tableplot4")
              
      ),
      
      # Tableau de bord #2
      tabItem(tabName = "TB2",
              h1("Typologie des Régions en fonction de l'évolution du covid-19 aux urgences de france"),
              plotOutput("tableplot5"),
              plotOutput("tableplot55"),
              plotOutput("tableplot6"),
              plotOutput("tableplot7"),
              plotOutput("tableplot8")
      ),
      
      
      # Tableau de bord #3
      tabItem(tabName = "TB3",
              h1("Typologie des Régions en fonction du sexe des patients aux urgences de france"),
              plotOutput("tableplot9"),
              plotOutput("tableplot10"),
              plotOutput("tableplot11"),
              plotOutput("tableplot12"),
              plotOutput("tableplot13"),
              plotOutput("tableplot14")
      ),
      
      
      # Tableau de bord #4
      tabItem(tabName = "TB4",
              h1("Typologie des Régions en fonction du sexe des patients aux urgences de france atteint du covid-19"),
              plotOutput("tableplot15"),
              plotOutput("tableplot17"),
              plotOutput("tableplot18"),
              plotOutput("tableplot19"),
              plotOutput("tableplot20")
      ),
      
      
      # Tableau de bord Cartes
      tabItem(tabName = "TB5",
              h1("Carte de la typologie des régions métropolitaines face aux covid-19 dans les urgences française"),
              plotOutput("tableplot22"),
              plotOutput("tableplot21")
      )
      
      
              
      )
      
    )
  )



######################################## SERVEUR ####################################




shinyApp(ui, server)