library(shiny)
library(dplyr)
library(shinyWidgets)
library(bslib)
library("RColorBrewer")
library(splines)
library(reactable)

title <-tags$a(tags$img(src='https://s1.qwant.com/thumbr/0x0/8/a/1e8b1431f670652e76767e9fd9bde1a1ed1972e4123df5c24bac354afa6dd4/2019-10-logo-ISPED-univ-Bordeaux.jpg?u=http%3A%2F%2Fwww.fondation.univ-bordeaux.fr%2Fwp-content%2Fuploads%2F2016%2F02%2F2019-10-logo-ISPED-univ-Bordeaux.jpg&q=0&b=1&p=0&a=0', height="60", width="150"), "Comment fonctionnent les B-Splines" )


ui = fluidPage(
  theme=bs_theme(version = 4),
  navbarPage(title = title,
             footer = "Application réalisée par Marie Delmotte, Sikha Dabo et Floriane Samaria dans le cadre du projet tutoré de l'UE STG101 du Master 1 de Santé Publique de l'ISPED, année universitaire 2021-2022", 
             theme=bs_theme(version = 4),
             tabsetPanel(
               tabPanel("Définitions et caractéristiques",
                        headerPanel('Tout savoir sur les B-Splines'),
                        fluidRow(
                          column(7,
                                 tags$br(),
                                 h4("Base de fonctions spline \\(B_k^d\\)"),
                                 tags$br(),
                                 p("Une spline est principalement caractérisée par :"),
                                 tags$li("K+2 points \\(\\tau_i\\) appelés nœuds. Le vecteur est appelé vecteur de nœuds pour la spline \\((\\tau_0, . . . , \\tau_{k+1}\\)). Ils ne sont pas nécessairement répartis régulièrement sur l’intervalle avec \\(\\tau_0\\) et \\(\\tau_{k+1}\\) les noeuds externes et K noeuds internes \\((\\tau_1, . . . , \\tau_{k}\\))."),
                                 tags$li("Un degré d identique pour chaque polynôme qui compose la base de B-spline"),
                                 tags$br(),
                                 tags$hr(),
                                 p("Pour d > 0, la fonction de base de B-Spline de degré d est définie ainsi :"),
                                 withMathJax('$$B_{k}^{d}(x)\\,=\\frac{x-\\tau_k}{\\tau_{k+d} - \\tau_k}B_{k}^{d-1}(x) - \\frac{\\tau_{k+d+1}-x}{\\tau_{k+d+1} - \\tau_{k+1}}B_{k+1}^{d-1}(x)$$ Avec k = 1,...K+d+1'),
                                 withMathJax('où $$ B_{k}^{0}(x) = \\begin{cases} 1, \\tau_{k} \\le x \\le \\tau_{k+1}\\\\ 0, sinon\\end{cases}$$ '),
                                 tags$hr(),
                                 tags$li("Support compact : une fonction de base polynomiale de degré d (\\(B_k^d\\)) est non-nulle et positive sur au plus d+1 sous-intervalles adjacents"),
                                 tags$li("Nombre de fonctions de base = ordre + nombre de nœuds intérieurs (\\(\\tau_0\\) et \\(\\tau_k\\) ne comptent pas) ou degré + 1 + nombre de noeuds intérieurs"),
                                 tags$br(),tags$br(),tags$br(),tags$br(), tags$br()
                                 
                                 ),
                          column(5,
                                 tags$br(),
                                 h4("Fonction spline résultante f(X)"),
                                 tags$br(),
                                 p("Supposons que la fonction inconnue f est représentée par une fonction spline avec une séquence de nœuds fixe et un degré fixe d, alors nous pouvons écrire la relation suivante :"),
                                 tags$hr(),
                                 withMathJax('$$f(X)\\,=\\sum_{k=1}^{K+d+1} \\beta_k\\:B_k(X)$$'),
                                 tags$hr(),
                                 p("où :"),
                                 tags$li("les \\(B_k\\) sont un ensemble de fonctions de base"),
                                 tags$li("les \\(\\beta_k\\) sont les coefficients splines associés."),
                                 tags$li(" \\( X\\in\\) [\\(\\tau_0 ;\\tau_{k+1}\\)]"),
                                 tags$br(),
                                 p("L'estimation de \\(f(X)\\) se réduit à l'estimation des coefficients \\(\\beta_k\\)."),
                                 tags$br()
                                 )
                          )
                        ),
               tabPanel("Notions de noeuds et de degré",
                        sidebarLayout(
                          sidebarPanel(
                            helpText("Vous pouvez selectionner différentes options :"),
                            br(),
                            
                            #Slider du nombre de noeuds
                            sliderInput("m", "Nombre de noeuds intérieurs", min = 1, max = 50,value=5),
                            
                            #Slider du degré de chaque spline
                            sliderInput("d", "Degré des splines", min = 1, max = 3,value=2, step = 1),
                            
                            actionButton("ReloadBetas", "Regénérer les coefficients pour chaque spline"),
                            helpText("Le coefficient de chaque spline est simulé aléatoirement par une loi uniforme entre 0 et 1."),
                            br(),
                            
                            checkboxInput("show_curve", label='Afficher la fonction spline resultante'),
                            
                            checkboxInput("show_coefs", label='Afficher la valeur du coefficient de chaque spline'),
                            reactableOutput("coef_table"),
                            
                            ),
                          
                          mainPanel(
                            h3("Graphiques de base de B-Splines et B-Splines pondérées"),
                            
                            textOutput("value"),
                            h4("Noeuds équidistants"),
                            
                            splitLayout(
                              plotOutput("BaseSpline"),
                              plotOutput("SplinePlot")
                            ),
                            
                            h4("Noeuds non équidistants"),
                            p("La position de chaque noeud est tirée aléatoirement dans l'intervalle [0,100] par une loi uniforme"),
                            splitLayout (
                               plotOutput("BaseSplineNonEqui"),
                               plotOutput("SplinePlotNonEqui")
                          )
                          )
                        )),
               tabPanel("Impact de la position des noeuds et des coefficients (poids)",
                        sidebarLayout(
                          sidebarPanel(
                            h6("Choix des positions des noeuds intérieurs (ici au nombre de 3)"),
                            p("Choisir des valeurs entre 0 et 100 (dans l'ordre croissant)"),
                            splitLayout(
                              numericInput("N1", "Noeud 1",value = 10, min = 1, max = 99),
                              numericInput("N2", "Noeud 2",value = 30, min = 1, max = 99),
                              numericInput("N3", "Noeud 3",value = 70, min = 1, max = 99)
                            ),
                            
                            h6("Choix des coefficients (poids) de chaque spline (ici au nombre de 6)"),
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
                            checkboxInput("show_curve2", label='Afficher la fonction spline resultante'),
                            
                          ),
                          mainPanel(
                            h3("Base et graphique pondéré avec 6 splines (3 noeuds et splines quadratiques)"),
                            tags$br(),
                            p("Les coefficients (poids) \\(\\beta_k\\) simulés pour chaque spline ont les valeurs suivantes : "),
                            tableOutput("coef_table6"),
                            h5("Résultats avec les coefficients (poids) choisis"),
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
  
  ########################## Outpus pour le volet 2 #############################
  
  # Table des betas (s'affiche si on a coché la checkbox correspondante)
  output$coef_table = renderReactable({ 
    if (input$show_coefs) {
      beta = round(beta(),2)
      num = seq(1,input$m+input$d+1, by=1)
      tab <- cbind(num, beta)
      reactable(tab, pagination = TRUE, highlight = TRUE, height = 200)
    } # if tshow_coefs
  })
  
  # Simulation d'un vecteur pour les x en abscisse (on choisit 1000 car on aura alors des splines plus lisses)
  x = sort(runif(1000, 0, 100))
  
  # Noeuds reactifs pour que les deux graphiques (base en résultats) aient les même noeuds
  equidistant.knots <- reactive({
    seq(min(x) + 5, max(x) - 5, length=input$m) #vecteur de noeuds intérieurs (on ne prend pas les extrémités)
  })
  
  # Graphique réactif de la base de B-spline pour point equidistants 
  # réactif au changment de nombre de noeuds et au degré 
  BaseSpline <- eventReactive({input$d
                              input$m},{
    equidistant.ret = bs(x, knots = equidistant.knots(), degree = input$d, intercept = TRUE, Boundary.knots = range(x)) #matrice de base spline : chaque colonne correspond Ã  une courbe spline
    
    # on plot tout ça
    cols <- hcl(h = seq(60, 240, length = ncol(equidistant.ret)), c =90, l = 70)
    plot(equidistant.ret[,1]~x, ylim=c(0,max(equidistant.ret)), type='l', lwd=2, col=cols[1], 
       main="Base de B-spline"  ,xlab="x", ylab="Bk")
    for (j in 2:ncol(equidistant.ret)) lines(equidistant.ret[,j]~x, lwd=2, col=cols[j])
    Y <- sample(0, replace = TRUE, size=length(equidistant.knots()))
    points(x=equidistant.knots(), y=Y, pch=20, cex=2)
  }, ignoreNULL = FALSE)
  
  # output du graphique de Base de B-Spline avec noeuds equidistants
  output$BaseSpline = renderPlot({BaseSpline()})
  
  
  # Les betas doivent être les même pour les deux graphs résultats afin de pouvoir comparer les splines avec points equidistans des splines avec points non équidistans
  # ils sont réactifs au changment de degré, de nombre de noeuds, et au bouton pour demander à recharger les betas
  beta <- eventReactive({input$ReloadBetas
                        input$d
                        input$m},{
      runif(input$m+input$d+1,0,1)    
    }, ignoreNULL = FALSE)
  
  
  # Graphique réactif de résultats des B-Spline pour des coefficients de beta simulés et points équidistans
  # reactif au changement de degré, nombre de noeuds, si on demande a afficher la courbe, ou a recharger les betas
  SplinePlot <- eventReactive({input$ReloadBetas
                              input$d
                              input$m
                              input$show_curve},{
      equidistant.ret = bs(x, knots = equidistant.knots(), degree = input$d, intercept = TRUE, Boundary.knots = range(x)) #matrice de base spline : chaque colonne correspond Ã  une courbe spline
      equidistant.ret.2 <- equidistant.ret%*%as.matrix(beta(),input$m+input$d+1,1) # on mutiplie la base par les coefficients pour chaque spline
      
      # on plot tout ça
      cols <- hcl(h = seq(60, 240, length = ncol(equidistant.ret)), c =90, l = 70)
      plot(equidistant.ret[,1]*beta()[1]~x, ylim=c(0,max(equidistant.ret)), type='l', lwd=2, col=cols[1], 
          main = "Base de B-spline après pondération"  ,xlab="x", ylab="Beta k Bk ")
      for (j in 2:ncol(equidistant.ret)) lines(equidistant.ret[,j]*beta()[j]~x, lwd=2, col=cols[j])
      if (input$show_curve) {
        lines(x,equidistant.ret.2, col = "red", lwd = 3)
        legend('topright', legend="Fonction spline resultante", col='red', lty=1, lwd=3)
      }
      Y <- sample(0, replace = TRUE, size=length(equidistant.knots()))
      points(x=equidistant.knots(), y=Y, pch=20, cex=2)
      
    }, ignoreNULL = FALSE)
  
  # output du graphique du résultats de B-Spline pondérée avec noeuds équidistans
  output$SplinePlot = renderPlot({SplinePlot()})
  
  
  
  # Noeuds reactifs pour que les deux graphiques (base en résultats) aient les même noeuds non équidistants
  knots <- reactive({
    sort(runif(input$m,min(x)+1, max(x)-1)) #vecteur de noeuds non équidistans
  })
  
  
  # Graphique réactif de la base de B-spline pour points non equidistants
  # réactif au changement du nombre de noeuds et du degré
  BaseSplineNonEqui <- eventReactive({input$d
                                      input$m}, {
    ret = bs(x, knots = knots(), degree = input$d, intercept = TRUE, Boundary.knots = range(x)) #matrice de base spline : chaque colonne correspond Ã  une courbe spline
    
    # on plot tout ça
    cols <- hcl(h = seq(60, 240, length = ncol(ret)), c =90, l = 70)
    plot(ret[,1]~x, ylim=c(0,max(ret)), type='l', lwd=2, col=cols[1], 
        main = "Base de B-spline",xlab="x", ylab="Bk")
    for (j in 2:ncol(ret)) lines(ret[,j]~x, lwd=2, col=cols[j])
    Y = seq(from = 0, to = 0, length=input$m)
    points(x=knots(), y=Y, pch=20, cex=2)
  }, ignoreNULL = FALSE)
  
  # outpur du graphique de la base de B-Spline pour noeuds non équidistants
  output$BaseSplineNonEqui = renderPlot({BaseSplineNonEqui()})
  
  
  # Graphique réactif de résultats des B-Spline pour des coefficients de beta simulés et points non équidistans
  # réactif au changement de nombre de noeuds, du degré et si on demande à rechager les betas ou a afficher la courbe rouge
  SplinePlotNonEqui <- eventReactive({input$ReloadBetas
                                      input$d
                                      input$m
                                      input$show_curve},{
      ret = bs(x, knots = knots(), degree = input$d, intercept = TRUE, Boundary.knots = range(x)) #matrice de base spline : chaque colonne correspond Ã  une courbe spline
      
      ret.2 <- ret%*%as.matrix(beta(),input$m+input$d+1,1) # on mutiplie la base par les coefficients pour chaque spline
      
      # on plot tout ça
      cols <- hcl(h = seq(60, 240, length = ncol(ret)), c =90, l = 70)
      plot(ret[,1]*beta()[1]~x, ylim=c(0,max(ret)), type='l', lwd=2, col=cols[1], 
           main="Base de B-spline après pondération" ,xlab="x", ylab="Beta k Bk")
      for (j in 2:ncol(ret)) lines(ret[,j]*beta()[j]~x, lwd=2, col=cols[j])
      if (input$show_curve) {
        lines(x,ret.2, col = "red", lwd = 3)
        legend('topright', legend="Fonction spline resultante", col='red', lty=1, lwd=3)
      }
      Y = seq(from = 0, to = 0, length=input$m)
      points(x=knots(), y=Y, pch=20, cex=2)
    }, ignoreNULL = FALSE)
  
  # output du graphique résultat de la base de B-Spline pondérée avec noeuds non équidistants
  output$SplinePlotNonEqui = renderPlot({SplinePlotNonEqui()})
  
  # Output pour la phrase informative sur le nombre de splines générées avec les coefficients choisis
  output$value <- renderText({paste("Avec ces paramètres vous tracez ", input$m+input$d+1, " BSplines.
                                    En effet, vous avez demandé ", input$m, " noeuds, avec des splines de degré ", input$d )})
  
  
  
  ########################## Outpus pour le volet 3 #############################
  
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
    ret = bs(x, knots = knots, degree = 2, intercept = TRUE, Boundary.knots = range(x)) #matrice de base spline : chaque colonne correspond Ã  une courbe spline
      
    # on plot tout ça
    cols <- hcl(h = seq(60, 240, length = ncol(ret)), c =90, l = 70)
    plot(ret[,1]~x, ylim=c(0,max(ret)), type='l', lwd=2, col=cols[1], 
         main="Base de B-spline quadratique (degré 2)", xlab="x", ylab="Bk")
    for (j in 2:ncol(ret)) lines(ret[,j]~x, lwd=2, col=cols[j])
    Y = seq(from = 0, to = 0, length=3)
    points(x=knots, y=Y, pch=20, cex=2)
    })
  # Output du graphique précédent
  output$BaseSpline6 = renderPlot({BaseSpline6()})
  
  # Graphique du résultat pour 6 splines avec position des noeuds choisie et coefficients choisis
  SplinePlot6 <- reactive({
    knots <- c(input$N1, input$N2, input$N3)
    betas <- c(input$C1, input$C2, input$C3, input$C4, input$C5, input$C6)
    ret = bs(x, knots = knots, degree = 2, intercept = TRUE, Boundary.knots = range(x)) #matrice de base spline : chaque colonne correspond Ã  une courbe spline
    
    ret.2 <- ret%*%as.matrix(betas,6,1) # on mutiplie la base par les coefficients pour chaque spline
    
    # on plot tout ça
    cols <- hcl(h = seq(60, 240, length = ncol(ret)), c =90, l = 70)
    plot(ret[,1]*betas[1]~x, ylim=c(0,max(ret)), type='l', lwd=2, col=cols[1], 
         main="Base de B-spline quadratique après pondération", ylab="Beta k bk")
    for (j in 2:ncol(ret)) lines(ret[,j]*betas[j]~x, lwd=2, col=cols[j])
    if (input$show_curve2) {
      lines(x,ret.2, col = "red", lwd = 3)
      legend('topright', legend="Fonction spline resultante", col='red', lty=1, lwd=3)
    }
    Y = seq(from = 0, to = 0, length=3)
    points(x=knots, y=Y, pch=20, cex=2)
  })
  # Output du graphique précédent
  output$SplinePlot6 = renderPlot({SplinePlot6()})
}



shinyApp(ui, server)


