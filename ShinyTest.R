library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(DT)
library(shinyWidgets)
library(bslib)
library("RColorBrewer")
require(splines)

ui = fluidPage(
  theme=bs_theme(version = 4, bootswatch = "default"),
  navbarPage(title = "Visualisation de B-Splines",
             theme=bs_theme(version = 4, bootswatch = "default"),
             tabPanel("Splines",
                      sidebarLayout(
                        sidebarPanel(
                          helpText("Vous pouvez selectionner différentes options :"),
                          br(),
                          #Slider du nombre de données
                          sliderInput("n", "Nombre de points", min = 0, max = 500,value=100, step = 20),
                               
                          #Slider du nombre de noeuds
                          sliderInput("m", "Nombre de noeuds", min = 0, max = 50,value=10, step = 5),
                               
                          #Slider du degré de chaque spline
                          sliderInput("d", "Degré de la spline", min = 0, max = 5,value=3, step = 1),
                               
                          materialSwitch(inputId = "mode", label = icon("moon"),
                                              right=TRUE,status = "success"),
                          img(src='https://s1.qwant.com/thumbr/0x0/8/a/1e8b1431f670652e76767e9fd9bde1a1ed1972e4123df5c24bac354afa6dd4/2019-10-logo-ISPED-univ-Bordeaux.jpg?u=http%3A%2F%2Fwww.fondation.univ-bordeaux.fr%2Fwp-content%2Fuploads%2F2016%2F02%2F2019-10-logo-ISPED-univ-Bordeaux.jpg&q=0&b=1&p=0&a=0', height="60", width="150")
                          
                        ),
                          
                        mainPanel(
                          tabsetPanel(
                            tabPanel("Définitions et caractéristiques",
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
                              "Gérérer une B-spline",
                              headerPanel("Graphique d'une B-Spline"),
                              plotOutput("SplinePlot"),
                              textOutput("value")
                            )
                          )
                        )
                      )
              ),
                  
              navbarMenu("More",
                         tabPanel("Table"),
                         tabPanel("About")
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
  
    plotBSpline <- reactive({ 
      x = sort(runif(input$n, 91.78525, 123)) #
      
      equidistant.knots = seq(min(x) + 0.761, max(x) - 0.761, length=input$m) #vecteur de noeuds 
      equidistant.ret = bs(x, knots = equidistant.knots, degree = input$d, intercept = TRUE, Boundary.knots = range(x)) #matrice de base spline : chaque colonne correspond Ã  une courbe spline

      beta <- runif(input$m+input$d+1,0,1) ## coefficient de chaque spline
      equidistant.ret.2 <- equidistant.ret%*%as.matrix(beta,input$m+input$d+1,1) # on mutiplie la base par les coefficients pour chaque spline
      
      # on plot tout ça
      plot(equidistant.ret[,1]~x, ylim=c(0,max(equidistant.ret)), type='l', lwd=2, col="blue", 
           xlab="Cubic B-spline basis", ylab="")
      for (j in 2:ncol(equidistant.ret)) lines(equidistant.ret[,j]*beta[j]~x, lwd=2, col="blue")
      lines(x,equidistant.ret.2, col = "red", lwd = 4)
      }) 
  
    output$SplinePlot = renderPlot({
      plotBSpline()
    })
  
output$value <- renderText({paste("Avec ces paramètres vous tracez", input$m+input$d+1, "BSplines.")})

  }

shinyApp(ui, server)

