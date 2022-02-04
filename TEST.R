library(shiny)
library(DT)
library(shinyWidgets)
library(bslib)
library("RColorBrewer")
library(ggplot2)

ui <- fluidPage(
  theme=bs_theme(version = 4, bootswatch = "default"),
  navbarPage("Visualisation de Splines",
             theme=bs_theme(version = 4, bootswatch = "default"),
             tabPanel("Splines",
                      sidebarLayout(
                        sidebarPanel(
                          #Slider du nombre de noeuds - Select number of datapoints to be shown/generated
                          sliderInput("datasize", "Nombre de noeuds", min = 1, max = 50,value=1, step = 1),
                          #slider du degré de spline - Select the degree of the spline to render
                          sliderInput('degrees', 'Degrés', min = 1, max = 20, value = 1, step = 1),
                          #Degre de liberte
                          sliderInput('ddl', 'Degrés de liberté', min = 1, max = 20, value = 1, step = 1),
                          materialSwitch(inputId = "mode", label = icon("moon"),
                                         right=TRUE,status = "success")),
                        mainPanel(
                          tabsetPanel(
                            
                            tabPanel(
                              # --- NAME OF THE PANEL ---
                              "Définitions et caractéristiques",
                              
                              # --- HEADER ---
                              headerPanel('Tout savoir sur les Splines'),
                              tableOutput(""),
                              
                              # --- FIRST DIV ---
                              h3 ("Définition"),
                              p("Une spline sur un intervalle T = [a, b] est une fonction polynomiale par morceaux, avec conditions de continuité sur la fonction et ses dérivées aux jointures. Elle est caractérisée par :"),
                              h5("- des noeuds"),
                              p("τ0 = a ≤ τ1 ≤ . . . ≤ τL = b,",style = "font-family: 'times'; font-si16pt"),
                              p("non nécessairement répartis régulièrement, non nécessairement distincts (points de ruptures - “breakpoints” = valeurs distinctes des τl )"),
                              h5("- un ordre (m) / degrés" ),
                              h5("- des dérivées continues sur l'intervalle T"),
                              p("Il faut savoir que plus l’ordre est élevé, plus la spline est régulière, plus le nombre de noeuds est grand, plus on gagne en précision. 
                 Et enfin, toute combinaison linéaire de fonctions spline est encore une fonction spline."),
                              p("Pour r = 2, une spline d'ordre 2 est donc une fonction continue et linéaire par morceaux. Les splines les plus fréquemment utilisées sont les splines d'ordre 4 dites splines cubiques", style = "font-family: 'times'; font-si16pt"),
                              
                              hr(),
                              # --- SECOND DIV ---
                              h3 ("Base de spline (B-spline)"),
                              p("Une base de spline d’ordre m et de séquence de noeuds τ est une famille de fonctions tq :"),
                              p("(i) chaque fonction de base est une spline (toute combinaison linéaire de ces fonctions
                  est donc encore une fonction spline) ;"),
                              p("(ii) toute spline d’ordre m et de séquence de noeuds τ peut s’exprimer comme combinaison linéaire de ces fonctions de base ;"),
                              p("(iii) les fonctions de bases sont linéairement indépendantes (pas nécessairement orthonormées)."),
                              br(),
                              p("Une base de B-splines est entièrement caractérisée par :"),
                              p("• un ordre m ou, de manière équivalente, le degré maximal m − 1 des morceaux de polynômes."),
                              p("• une séquence de noeuds τ, qui entraine la connaissance des points de rupture (jointure) des sous-intervalles."),
                              
                              hr(),
                              
                              # --- THIRD DIV ---
                              h3("Quelques propriétés"),
                              p("Le nombre de fonctions de base est égal à : ordre + nombre de noeuds intérieurs ( τ0 et τL ne sont pas comptés)"),
                              p("La forme des B-splines est définie par les noeuds. Pour des noeuds équidistants par exemple, toutes les fonctions de base ont la même forme. "),
                              p("Il y a toujours une baisse de continuité aux extrémités de l’intervalle T. "),
                              p("La somme des valeurs des fonctions de bases B-spline en tout point t est égale à 1. "),
                              
                              hr(),
                              
                              # --- FOURTH DIV ---
                              h3("Exemple de bases B-splines noeuds équidistants tous distincts sur [0, 10]."),
                              imageOutput("plot1")
                            ),
                            
                            tabPanel(
                              # --- NAME OF THE PANEL --- 
                              "Gérérer une B-spline",
                              
                              # --- GRAPH ---
                              plotOutput("splinePlot"),
                              headerPanel(""),
                              tableOutput("")
                              
                            )
                          )
                        ) #fin main panel
                      )
             )
  )
)


server <- function(input, output,session) {
  
  
  observe(session$setCurrentTheme(
    if(isTRUE(input$mode)){
      bs_theme(bootswatch = "superhero")
    } else {
      bs_theme(bootswatch = "default")
    }
  ))
  
  output$splinePlot = renderPlot({
    
  })
  
  output$value <- renderPrint({ input$num })
}
shinyApp(ui, server)