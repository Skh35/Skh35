library(shiny)
library(dplyr)
library(shinyWidgets)
library(bslib)
library("RColorBrewer")
require(splines)

ui = fluidPage(
  theme=bs_theme(version = 4, bootswatch = "default"),
  navbarPage(title = "Application pour comprendre comment fonctionnent les B-Splines",
             theme=bs_theme(version = 4, bootswatch = "default"),
             tabsetPanel(
               tabPanel("Définitions et caractéristiques",
                        # --- HEADER ---
                        headerPanel('Tout savoir sur les Splines'),
                        tableOutput(""),
                        
                        # --- FIRST DIV ---
                        p("Une spline est principalement caractérisée par :"),
                        p("- K points \\(\\tau_i\\) appelés nœuds. Le vecteur est appelé vecteur de nœud pour la spline \\((\\tau_0, . . . , \\tau_k\\)). Ils ne sont pas nécessairement répartis régulièrement sur l’intervalle."),
                        p("- Un degré d identique pour chaque polynôme qui compose la B-spline"),
                        p("- Des dérivées continues sur l'intervalle T"),
                        p("Les k points donnent k+1 polynômes de degré d."),
                        
                        p("Supposons que la fonction inconnue f est représentée par une fonction spline avec une séquence de nœuds fixe et un degré fixe d, alors nous pouvons écrire la relation suivante :"),
                        withMathJax('$$f(X)\\,=\\sum_{k=1}^{K+d+1} \\beta_k\\:B_k(X)$$'),
                        p("où les \\(B_k\\) sont un ensemble de fonctions de base et \\(\\beta_k\\) sont les coefficients splines associés. Avec k nœuds il y a k+1 polynômes de degré d avec d∗k contraintes, conduisant à (d+1)(k+1)−d∗k=k+d+1 paramètres libres (degrés de liberté). L'estimation de f se réduit à l'estimation des coefficients \\(\\beta_k\\). "),
               
                        p("Une base de B-splines est entièrement caractérisée par :"),
                        p("Un ordre m ou, une séquence de nœuds \\(\\tau\\)"),
                        br(),
                        p("Elle respecte aussi certaines propriétés :"),
                        p("- Nombre de fonctions de base = ordre + nombre de nœuds intérieurs (\\(\\tau_0\\) et \\(\\tau_k\\) ne comptent pas)"),
                        p("- Support compact : une fonction B-Spline d’ordre m est non-nulles et positive sur au plus m sous-intervalles adjacents"),
                        p("- Forme : la forme des B-Splines est définie par les nœuds. Avec des nœuds équidistants, les fonctions de base ont la même forme
                        Il y a toujours une baisse de continuité aux extrémités de l’intervalle T"),
                        p("- La somme des valeurs des fonctions de bases B-spline en tout point t est égale à 1."),
                        
                        p("Pour d > 0, la fonction de base de B-Spline de degré d est définie ainsi :"),
                        withMathJax('$$B_{k}^{d}(x)\\,=\\frac{x-\\tau_k}{\\tau_{k+d} - \\tau_k}B_{k}^{d-1}(x) - \\frac{\\tau_{k+d+1}-x}{\\tau_{k+d+1} - \\tau_{k+1}}B_{k+1}^{d-1}(x)$$'),
                        p("Avec k = 1,...K+d+1"),
                        p("où"),
                        withMathJax('$$\\begin{eqnarray} a^2 + b^2 &=& c^2 \\\ &=& 5  \\end{eqnarray}$$ '),
                        
                        ),
               tabPanel("Comprendre le nombre de noeuds et de le degré",
                        sidebarLayout(
                          sidebarPanel(
                            helpText("Vous pouvez selectionner différentes options :"),
                            br(),
                            
                            #Slider du nombre de noeuds
                            sliderInput("m", "Nombre de noeuds", min = 1, max = 50,value=5),
                            
                            #Slider du degré de chaque spline
                            sliderInput("d", "Degré de la spline", min = 1, max = 3,value=2, step = 1),
                            
                            actionButton("ReloadBetas", "Générer les coefficients pour chaque spline"),
                            helpText("Le coefficients de chaque spline est simulé aléatoirement par une loi uniforme entre 0 et 1."),
                            br(),
                            
                            checkboxInput("show_coefs", label='Afficher la valeur du coefficient de chaque spline'),
                            checkboxInput("show_curve", label='Afficher la courbe estimée'),
                            
                            materialSwitch(inputId = "mode", label = icon("moon"),
                                           right=TRUE,status = "success"),
                            
                            img(src='https://s1.qwant.com/thumbr/0x0/8/a/1e8b1431f670652e76767e9fd9bde1a1ed1972e4123df5c24bac354afa6dd4/2019-10-logo-ISPED-univ-Bordeaux.jpg?u=http%3A%2F%2Fwww.fondation.univ-bordeaux.fr%2Fwp-content%2Fuploads%2F2016%2F02%2F2019-10-logo-ISPED-univ-Bordeaux.jpg&q=0&b=1&p=0&a=0', height="60", width="150")
                          ),
                          
                          mainPanel(
                            h3("Graphique d'une B-Spline"),
                            
                            textOutput("value"),
                            h4("Points équidistants"),
                            textOutput("infosCoeffs"),

                            tableOutput("coef_table"),
                            
                            splitLayout(
                              plotOutput("BaseSpline"),
                              plotOutput("SplinePlot")
                            ),
                            
                            h4("Points non équidistants"),
                            p("La position de chaque noeud est choisit aléatoirement dans l'intervalle [0,100]"),
                            splitLayout(
                              plotOutput("BaseSplineNonEqui"),
                              plotOutput("SplinePlotNonEqui")
                            )
                          )
                        )),
               tabPanel("Comprendre la position des noeuds et coefficients",
                        sidebarLayout(
                          sidebarPanel(
                            h6("Choix des positions des noeuds (ici au nombre de 3)"),
                            p("Choisir des valeurs entre 0 et 100 (dans l'ordre croissant)"),
                            splitLayout(
                              numericInput("N1", "Noeud 1",value = 1, min = 0, max = 100),
                              numericInput("N2", "Noeud 2",value = 30, min = 0, max = 100),
                              numericInput("N3", "Noeud 3",value = 70, min = 0, max = 100)
                            ),
                            
                            h6("Choix des coefficients de chaque spline (ici au nombre de 6)"),
                            p("Choisir des valeurs entre 0 et 1"),
                            splitLayout(
                              sliderInput("C1", "Coefficient 1",value = 1, step = 0.01, min = 0, max = 1),
                              sliderInput("C2", "Coefficient 2",value = 1, step = 0.01, min = 0, max = 1)
                            ),
                            splitLayout(
                              sliderInput("C3", "Coefficient 3",value = 1, step = 0.01, min = 0, max = 1),
                              sliderInput("C4", "Coefficient 4",value = 1, step = 0.01, min = 0, max = 1) 
                            ),
                            splitLayout(
                              sliderInput("C5", "Coefficient 5",value = 1, step = 0.01, min = 0, max = 1),
                              sliderInput("C6", "Coefficient 6",value = 1, step = 0.01, min = 0, max = 1)
                            ),
                            checkboxInput("show_curve2", label='Afficher la courbe estimée'),
                            
                          ),
                          mainPanel(
                            p("Les coefficients \\(\\beta_k\\) simulés pour chaque spline ont les valeurs suivantes : "),
                            tableOutput("coef_table6"),
                            splitLayout(
                              h6("Base avec 6 splines (3 noeuds et splines quadratiques (degré 2)"),
                              h6("Résultats avec les coefficients choisis")
                            ),
                            splitLayout(
                              plotOutput("BaseSpline6"),
                              plotOutput("SplinePlot6")
                            ),
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
  
  output$infosCoeffs <- renderText({
    if (input$show_coefs){
      paste("Les coefficients simulés pour chaque spline ont les valeurs suivantes : ")
    }
  })
  
  output$coef_table = renderTable({ 
    if (input$show_coefs) {
      beta_text = sprintf('% .3f',beta())
      num_beta = seq(1,input$m+input$d+1, by=1)
      
      tab        = rbind('Numero'=num_beta, 
                         'Coefficient'=beta_text)  
    } # if tshow_coefs
  }, align='r', colnames = FALSE, spacing = "xs", rownames = TRUE
  )
  
  
  x = sort(runif(1000, 0, 100))
  
  # Noeuds reactifs pour que les deux graphiques (base en résultats) aient les même noeuds
  equidistant.knots <- reactive({
    seq(min(x) + 0.761, max(x) - 0.761, length=input$m) #vecteur de noeuds 
  })
  
  # Graphique de la base de B-spline pour point equidistants
  BaseSpline <- eventReactive({input$ReloadBetas
                              input$d
                              input$m},{
    equidistant.ret = bs(x, knots = equidistant.knots(), degree = input$d, intercept = TRUE, Boundary.knots = range(x)) #matrice de base spline : chaque colonne correspond Ã  une courbe spline
    
    beta <- sample(1, replace = TRUE, size=input$m+input$d+1) ## coefficient de chaque spline    
    
    equidistant.ret.2 <- equidistant.ret%*%as.matrix(beta,input$m+input$d+1,1) # on mutiplie la base par les coefficients pour chaque spline
    
    # on plot tout ça
    plot(equidistant.ret[,1]~x, ylim=c(0,max(equidistant.ret)), type='l', lwd=2, col="blue", 
         xlab="Base de B-spline", ylab="")
    for (j in 2:ncol(equidistant.ret)) lines(equidistant.ret[,j]*beta[j]~x, lwd=2, col="blue")
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
                              input$d
                              input$m
                              input$show_curve},{
      equidistant.ret = bs(x, knots = equidistant.knots(), degree = input$d, intercept = TRUE, Boundary.knots = range(x)) #matrice de base spline : chaque colonne correspond Ã  une courbe spline
      equidistant.ret.2 <- equidistant.ret%*%as.matrix(beta(),input$m+input$d+1,1) # on mutiplie la base par les coefficients pour chaque spline
      
      # on plot tout ça
      plot(equidistant.ret[,1]~x, ylim=c(0,max(equidistant.ret)), type='l', lwd=2, col="blue", 
           xlab="B-spline pondérée", ylab="")
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
                                      input$d
                                      input$m}, {
    ret = bs(x, knots = knots(), degree = input$d, intercept = TRUE, Boundary.knots = range(x)) #matrice de base spline : chaque colonne correspond Ã  une courbe spline
    
    beta <- sample(1, replace = TRUE, size=input$m+input$d+1) ## coefficient de chaque spline    
    ret.2 <- ret%*%as.matrix(beta,input$m+input$d+1,1) # on mutiplie la base par les coefficients pour chaque spline
    
    # on plot tout ça
    plot(ret[,1]~x, ylim=c(0,max(ret)), type='l', lwd=2, col="blue", 
         xlab="Base de B-spline", ylab="")
    for (j in 2:ncol(ret)) lines(ret[,j]*beta[j]~x, lwd=2, col="blue")
    Y = seq(from = 0, to = 0, length=input$m)
    points(x=knots(), y=Y, pch=20, cex=2)
  }, ignoreNULL = FALSE)
  output$BaseSplineNonEqui = renderPlot({BaseSplineNonEqui()})
  
  
  # Graphique de résultats des B-Spline pour des coefficients de beta simulés et points non équidistans
  SplinePlotNonEqui <- eventReactive({input$ReloadBetas
                                      input$d
                                      input$m
                                      input$show_curve},{
      ret = bs(x, knots = knots(), degree = input$d, intercept = TRUE, Boundary.knots = range(x)) #matrice de base spline : chaque colonne correspond Ã  une courbe spline
      
      ret.2 <- ret%*%as.matrix(beta(),input$m+input$d+1,1) # on mutiplie la base par les coefficients pour chaque spline
      
      # on plot tout ça
      plot(ret[,1]~x, ylim=c(0,max(ret)), type='l', lwd=2, col="blue", 
           xlab="B-spline Pondérée", ylab="")
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
  
  
  
  
  #Table des coefficients de chaque spline (pour le volet avec les 6 splines)
  output$coef_table6 = renderTable({ 
      beta_text = c(input$C1, input$C2, input$C3, input$C4, input$C5, input$C6)
      num_beta = c("1","2","3","4","5","6")
      tab        = rbind('Numero'=num_beta, 
                         'Coefficient'=beta_text)  
  }, align='r', colnames = FALSE, spacing = "xs", rownames = TRUE
  )
  
  # Graphique de la base de spline pour 6 splines avec position des noeufs choisie
  BaseSpline6 <- reactive({
    knots <- c(input$N1, input$N2, input$N3)
    betas <- rep(1,6)
    ret = bs(x, knots = knots, degree = 2, intercept = TRUE, Boundary.knots = range(x)) #matrice de base spline : chaque colonne correspond Ã  une courbe spline
      
    ret.2 <- ret%*%as.matrix(betas,6,1) # on mutiplie la base par les coefficients pour chaque spline
      
    # on plot tout ça
    plot(ret[,1]~x, ylim=c(0,max(ret)), type='l', lwd=2, col="blue", 
         xlab="Base de B-spline quadratique (degré 2)", ylab="")
    for (j in 2:ncol(ret)) lines(ret[,j]*betas[j]~x, lwd=2, col="blue")
    Y = seq(from = 0, to = 0, length=3)
    points(x=knots, y=Y, pch=20, cex=2)
    })
  output$BaseSpline6 = renderPlot({BaseSpline6()})
  
  # Graphique du résultat pour 6 splines avec position des noeuds choisie et coefficients choisis
  SplinePlot6 <- reactive({
    knots <- c(input$N1, input$N2, input$N3)
    betas <- c(input$C1, input$C2, input$C3, input$C4, input$C5, input$C6)
    ret = bs(x, knots = knots, degree = 2, intercept = TRUE, Boundary.knots = range(x)) #matrice de base spline : chaque colonne correspond Ã  une courbe spline
    
    ret.2 <- ret%*%as.matrix(betas,6,1) # on mutiplie la base par les coefficients pour chaque spline
    
    # on plot tout ça
    plot(ret[,1]~x, ylim=c(0,max(ret)), type='l', lwd=2, col="blue", 
         xlab="B-spline quadratique pondérée", ylab="")
    for (j in 2:ncol(ret)) lines(ret[,j]*betas[j]~x, lwd=2, col="blue")
    if (input$show_curve2) {
      lines(x,ret.2, col = "red", lwd = 4)
    }
    Y = seq(from = 0, to = 0, length=3)
    points(x=knots, y=Y, pch=20, cex=2)
  })
  output$SplinePlot6 = renderPlot({SplinePlot6()})
}



shinyApp(ui, server)


