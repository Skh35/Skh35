
library(shiny)
library("RColorBrewer")
library(ggplot2)



### lA PARTIE UI (sert à faire la partie l'interface)
ui <- fluidPage(tabsetPanel(
    
    titlePanel("Les fonctions splines"),
    
    sidebarLayout(
        sidebarPanel(
                
                sliderInput("datasize", "Nombre de noeuds",  # Select number of datapoints to be shown/generated
                            min = 1, max = 50,value=1, step = 1),
                
                sliderInput('degrees', 'Degrés',  # Select the degree of the spline to render
                            min = 1, max = 20, value = 1, step = 1),
            
           
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(
                
                tabPanel("Définitions et caractéristiques des fonctions Splines",
                         headerPanel('Tout savoir sur les splines'),
                         tableOutput(""),
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
                
                hr(),
                h3 ("Base de spline (B-spline)"),
                 p("Une base de spline d’ordre m et de séquence de noeuds τ est une famille de fonctions tq :"),
                p("(i) chaque fonction de base est une spline (toute combinaison linéaire de ces fonctions
est donc encore une fonction spline) ;"),
                br(),
                p("(ii) toute spline d’ordre m et de séquence de noeuds τ peut s’exprimer comme combinaison linéaire de ces fonctions de base ;"),
                br(),
                p("(iii) les fonctions de bases sont linéairement indépendantes (pas nécessairement orthonormées)."),
                br(),
                p("Une base de B-splines est entièrement caractérisée par :"),
                br(),
                p("• un ordre m ou, de manière équivalente, le degré maximal m − 1 des morceaux de polynômes."),
                br(),
                p("• une séquence de noeuds τ, qui entraine la connaissance des points de rupture (jointure) des sous-intervalles."),
                
                hr(),
                h3("Quelques propriétés"),
                p("Le nombre de fonctions de base est égal à : ordre + nombre de noeuds intérieurs ( τ0 et τL ne sont pas comptés")),
                p("La forme des B-splines est définie par les noeuds. Pour des noeuds équidistants par exemple, toutes les fonctions de base ont la même forme. "),
                p("Il y a toujours une baisse de continuité aux extrémités de l’intervalle T. "),
                p("La somme des valeurs des fonctions de bases B-spline en tout point t est égale à 1. "),
               
                
                hr(),
                h3("Exemple de bases B-splines noeuds équidistants tous distincts sur [0, 10]."),
                imageOutput("plot1"),
                
                
                tabPanel("Gérérer une B-spline",
                         plotOutput("plot"),
                         headerPanel(""),
                         tableOutput(""))
                )
                
                ),
        
)

    )      
        
    )


# Définir la logique du serveur

server <- function(input, output, session) {
    
    output$plot1 <- renderImage({
        filename <- normalizePath(file.path('./images',
                                            paste('Spline', input$n, '.png', sep='')))
        
        # Return a list containing the filename and alt text
        list(src = filename,
             alt = paste("Spline", input$n))
    }, deleteFile = FALSE)
}

shinyApp(ui, server)
