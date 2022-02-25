library(shiny)
library(dplyr)
library(shinyWidgets)
library(bslib)
library("RColorBrewer")
require(splines)

ui = fluidPage(
  theme=bs_theme(version = 4, bootswatch = "default"),
  navbarPage(title = "Application pour comprendre comment fonctionnent les de B-Splines",
             theme=bs_theme(version = 4, bootswatch = "default"),
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
               tabPanel("Gérérer une B-spline",
                        sidebarLayout(
                          sidebarPanel(
                            helpText("Vous pouvez selectionner différentes options :"),
                            br(),
                            
                            #Slider du nombre de noeuds
                            sliderInput("m", "Nombre de noeuds", min = 1, max = 50,value=10),
                            
                            #Slider du degré de chaque spline
                            sliderInput("d", "Degré de la spline", min = 1, max = 3,value=3, step = 1),
                            
                            actionButton("ReloadBetas", "Générer les coefficients pour chaque Spline"),
                            br(),
                            
                            materialSwitch(inputId = "mode", label = icon("moon"),
                                           right=TRUE,status = "success"),
                            
                            img(src='https://s1.qwant.com/thumbr/0x0/8/a/1e8b1431f670652e76767e9fd9bde1a1ed1972e4123df5c24bac354afa6dd4/2019-10-logo-ISPED-univ-Bordeaux.jpg?u=http%3A%2F%2Fwww.fondation.univ-bordeaux.fr%2Fwp-content%2Fuploads%2F2016%2F02%2F2019-10-logo-ISPED-univ-Bordeaux.jpg&q=0&b=1&p=0&a=0', height="60", width="150")
                          ),
                          
                          mainPanel(
                            h3("Graphique d'une B-Spline"),
                            
                            h4("Points équidistants"),
                            textOutput("value"),
                            p("Les coefficients betas simulés pour chaque spline ont les valeurs suivantes : "),
                            checkboxInput("show_coefs", label='Numéro de la Spline et coefficient'),
                            checkboxInput("show_curve", label='Afficher la courbe estimée'),
                            
                            
                            tableOutput("coef_table"),
                            
                            
                            splitLayout(
                              plotOutput("BaseSpline"),
                              plotOutput("SplinePlot")
                            ),
                            p("Le coefficients de chaque spline est simulé aléatoirement par une loi uniforme entre 0 et 1."),
                            
                            h4("Points non équidistants"),
                            splitLayout(
                              plotOutput("BaseSplineNonEqui"),
                              plotOutput("SplinePlotNonEqui")
                            )
                          )
                        )),
               tabPanel("Personaliser un graphique avec 6 splines",
                        sidebarLayout(
                          sidebarPanel(
                            h6("Choix des positions des noeuds (ici au nombre de 3)"),
                            p("Choisir des valeurs entre 0 et 100 (dans l'ordre croissant)"),
                            numericInput("N1", "Premier noeud",value = 1, min = 0, max = 100),
                            numericInput("N2", "Deuxième noeud",value = 30, min = 0, max = 100),
                            numericInput("N3", "Troisième noeud",value = 70, min = 0, max = 100),
                            
                            h6("Choix des des coefficients de chaque spline (ici au nombre de 6)"),
                            p("Choisir des valeurs entre 0 et 1"),
                            sliderInput("C1", "Coefficient 1",value = 1, step = 0.01, min = 0, max = 1),
                            sliderInput("C2", "Coefficient 2",value = 1, step = 0.01, min = 0, max = 1),
                            sliderInput("C3", "Coefficient 3",value = 1, step = 0.01, min = 0, max = 1),
                            sliderInput("C4", "Coefficient 4",value = 1, step = 0.01, min = 0, max = 1),
                            sliderInput("C5", "Coefficient 5",value = 1, step = 0.01, min = 0, max = 1),
                            sliderInput("C6", "Coefficient 6",value = 1, step = 0.01, min = 0, max = 1)
                          ),
                          mainPanel(
                            h6("Base avec 6 spline (3 noeuds et splines quadratiques (degré 2)"),
                            plotOutput("BaseSpline6"),
                            h6("Résultats avec les coefficients choisis"),
                            plotOutput("SplinePlot6")
                          )
                        ))
               
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
  
  x = sort(runif(1000, 0, 100))
  
  output$coef_table = renderTable({ 
    if (input$show_coefs) {
      beta_text = sprintf('% .3f',beta())
      num_beta = seq(1,input$m+input$d+1, by=1)
      
      tab        = rbind('Numero'=num_beta, 
                         'Coefficient'=beta_text)  
    } # if tshow_coefs
  }, align='r', colnames = FALSE, spacing = "xs", rownames = TRUE
  )
  
  # Noeuds reactifs pour que les deux graphiques (base en résultats) aient les même noeuds
  equidistant.knots <- reactive({
    seq(min(x) + 0.761, max(x) - 0.761, length=input$m) #vecteur de noeuds 
  })
  
  # Graphique de la base de B-spline pour point equidistants
  BaseSpline <- eventReactive({input$ReloadBetas
                              input$show_curve},{
    equidistant.ret = bs(x, knots = equidistant.knots(), degree = input$d, intercept = TRUE, Boundary.knots = range(x)) #matrice de base spline : chaque colonne correspond Ã  une courbe spline
    
    beta <- sample(1, replace = TRUE, size=input$m+input$d+1) ## coefficient de chaque spline    
    
    equidistant.ret.2 <- equidistant.ret%*%as.matrix(beta,input$m+input$d+1,1) # on mutiplie la base par les coefficients pour chaque spline
    
    # on plot tout ça
    plot(equidistant.ret[,1]~x, ylim=c(0,max(equidistant.ret)), type='l', lwd=2, col="blue", 
         xlab="Cubic B-spline basis", ylab="")
    for (j in 2:ncol(equidistant.ret)) lines(equidistant.ret[,j]*beta[j]~x, lwd=2, col="blue")
    if (input$show_curve) {
      lines(x,equidistant.ret.2, col = "red", lwd = 4)
    }
    Y <- sample(0, replace = TRUE, size=length(equidistant.knots()))
    points(x=equidistant.knots(), y=Y, pch=20, cex=2)
  }, ignoreNULL = FALSE)
  output$BaseSpline = renderPlot({BaseSpline()})
  
  
  # Les betas doivent être les même pour les deux graphs résultats afin de pouvoir comparer les splines avec points equidistans des splines avec points non équidistans
  beta <- eventReactive({input$ReloadBetas
    input$d
    input$m},{
      runif(input$m+input$d+1,0,1) ## coefficient de chaque spline    
    }, ignoreNULL = FALSE)
  
  
  # Graphique de résultats des B-Spline pour des coefficients de beta simulés et points équidistans
  SplinePlot <- eventReactive({input$ReloadBetas
                              input$show_curve},{
      equidistant.ret = bs(x, knots = equidistant.knots(), degree = input$d, intercept = TRUE, Boundary.knots = range(x)) #matrice de base spline : chaque colonne correspond Ã  une courbe spline
      equidistant.ret.2 <- equidistant.ret%*%as.matrix(beta(),input$m+input$d+1,1) # on mutiplie la base par les coefficients pour chaque spline
      
      # on plot tout ça
      plot(equidistant.ret[,1]~x, ylim=c(0,max(equidistant.ret)), type='l', lwd=2, col="blue", 
           xlab="Cubic B-spline basis", ylab="")
      for (j in 2:ncol(equidistant.ret)) lines(equidistant.ret[,j]*beta()[j]~x, lwd=2, col="blue")
      if (input$show_curve) {
        lines(x,equidistant.ret.2, col = "red", lwd = 4)
      }
      Y <- sample(0, replace = TRUE, size=length(equidistant.knots()))
      points(x=equidistant.knots(), y=Y, pch=20, cex=2)
    }, ignoreNULL = FALSE)
  output$SplinePlot = renderPlot({SplinePlot()})
  
  
  # Noeuds reactifs pour que les deux graphiques (base en résultats) aient les même noeuds non équidistants
  knots <- reactive({
    sort(runif(input$m,min(x) + 0.761, max(x) - 0.761)) #vecteur de noeuds non équidistans
  })
  
  
  # Graphique de la base de B-spline pour point non equidistants
  BaseSplineNonEqui <- eventReactive({input$ReloadBetas
                                     input$show_curve}, {
    ret = bs(x, knots = knots(), degree = input$d, intercept = TRUE, Boundary.knots = range(x)) #matrice de base spline : chaque colonne correspond Ã  une courbe spline
    
    beta <- sample(1, replace = TRUE, size=input$m+input$d+1) ## coefficient de chaque spline    
    ret.2 <- ret%*%as.matrix(beta,input$m+input$d+1,1) # on mutiplie la base par les coefficients pour chaque spline
    
    # on plot tout ça
    plot(ret[,1]~x, ylim=c(0,max(ret)), type='l', lwd=2, col="blue", 
         xlab="Cubic B-spline basis", ylab="")
    for (j in 2:ncol(ret)) lines(ret[,j]*beta[j]~x, lwd=2, col="blue")
    if (input$show_curve) {
      lines(x,ret.2, col = "red", lwd = 4)
    }
    Y = seq(from = 0, to = 0, length=input$m)
    points(x=knots(), y=Y, pch=20, cex=2)
  }, ignoreNULL = FALSE)
  output$BaseSplineNonEqui = renderPlot({BaseSplineNonEqui()})
  
  
  
  # Graphique de résultats des B-Spline pour des coefficients de beta simulés et points non équidistans
  SplinePlotNonEqui <- eventReactive({input$ReloadBetas
                                      input$show_curve},{
      ret = bs(x, knots = knots(), degree = input$d, intercept = TRUE, Boundary.knots = range(x)) #matrice de base spline : chaque colonne correspond Ã  une courbe spline
      
      ret.2 <- ret%*%as.matrix(beta(),input$m+input$d+1,1) # on mutiplie la base par les coefficients pour chaque spline
      
      # on plot tout ça
      plot(ret[,1]~x, ylim=c(0,max(ret)), type='l', lwd=2, col="blue", 
           xlab="Cubic B-spline basis", ylab="")
      for (j in 2:ncol(ret)) lines(ret[,j]*beta()[j]~x, lwd=2, col="blue")
      if (input$show_curve) {
        lines(x,ret.2, col = "red", lwd = 4)
      }
      Y = seq(from = 0, to = 0, length=input$m)
      points(x=knots(), y=Y, pch=20, cex=2)
    }, ignoreNULL = FALSE)
  output$SplinePlotNonEqui = renderPlot({SplinePlotNonEqui()})
  
  output$value <- renderText({paste("Avec ces paramètres vous tracez ", input$m+input$d+1, " BSplines.
                                    En effet, vous avez demandé ", input$m, " noeuds, avec des splines de degré ", input$d )})
  
  output$betas <- renderText({round(beta(),2)})
  
  
  
  
  
  # Graphique de la base de spline pour 6 spline avec position des noeufs choisie
  BaseSpline6 <- reactive({
    knots <- c(input$N1, input$N2, input$N3)
    betas <- rep(1,6)
    ret = bs(x, knots = knots, degree = 2, intercept = TRUE, Boundary.knots = range(x)) #matrice de base spline : chaque colonne correspond Ã  une courbe spline
      
    ret.2 <- ret%*%as.matrix(betas,6,1) # on mutiplie la base par les coefficients pour chaque spline
      
    # on plot tout ça
    plot(ret[,1]~x, ylim=c(0,max(ret)), type='l', lwd=2, col="blue", 
         xlab="Quadratic B-spline basis", ylab="")
    for (j in 2:ncol(ret)) lines(ret[,j]*betas[j]~x, lwd=2, col="blue")
    lines(x,ret.2, col = "red", lwd = 4)
    Y = seq(from = 0, to = 0, length=3)
    points(x=knots, y=Y, pch=20, cex=2)
    })
  output$BaseSpline6 = renderPlot({BaseSpline6()})
  
  # Graphique de du résultat pour 6 spline avec position des noeuds choisie et coefficients choisis
  SplinePlot6 <- reactive({
    knots <- c(input$N1, input$N2, input$N3)
    betas <- c(input$C1, input$C2, input$C3, input$C4, input$C5, input$C6)
    ret = bs(x, knots = knots, degree = 2, intercept = TRUE, Boundary.knots = range(x)) #matrice de base spline : chaque colonne correspond Ã  une courbe spline
    
    ret.2 <- ret%*%as.matrix(betas,6,1) # on mutiplie la base par les coefficients pour chaque spline
    
    # on plot tout ça
    plot(ret[,1]~x, ylim=c(0,max(ret)), type='l', lwd=2, col="blue", 
         xlab="Quadratic B-spline basis", ylab="")
    for (j in 2:ncol(ret)) lines(ret[,j]*betas[j]~x, lwd=2, col="blue")
    lines(x,ret.2, col = "red", lwd = 4)
    Y = seq(from = 0, to = 0, length=3)
    points(x=knots, y=Y, pch=20, cex=2)
  })
  output$SplinePlot6 = renderPlot({SplinePlot6()})
}



shinyApp(ui, server)


