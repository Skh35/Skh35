
library(shiny)
library("RColorBrewer")
library(ggplot2)



### lA PARTIE UI (sert à faire la partie l'interface)
ui <- fluidPage(
    
    titlePanel("Les fonctions splines"),
    
    sidebarLayout(
        sidebarPanel(
                selectInput('data', "Selectionner la table à utiliser",  # Select between mtcars and randomly generated data
                            choices = c('A','B')),
                
                sliderInput("datasize", "Nombre de points ??? Demander si il faut les mettre",  # Select number of datapoints to be shown/generated
                            min = 10, max = 200, value = 50, step = 10),
                
                uiOutput('knotNumber'),  # Select the number of knots to render
                
                sliderInput('degrees', 'Degrés',  # Select the degree of the spline to render
                            min = 1, max = 20, value = 1, step = 1),
                
                numericInput(inputId = "numLines",
                             label = "Nombre de noeuds:",
                             value = 20),
            
            #Render the plots
            mainPanel(
                plotOutput('splinePlot')  # Display the rendered graph
            )
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(
                
                tabPanel("Définitions et caractéristiques des fonctions Splines",
                         headerPanel('Définition'),
                         tableOutput(""),
                         p("Une spline sur un intervalle T = [a, b] est une fonction polynomiale par morceaux, avec conditions de continuité sur la fonction et ses dérivées aux jointures. Elle est caractérisée par :"),
                         h5("- des noeuds"),
                         h5("- un ordre / degrés" ),
                        h5("- des dérivées continues"),

                
                p("Il faut savoir que plus l’ordre est élevé, plus la spline est régulière, plus le nombre de noeuds est grand, plus on gagne en précision. 
                 Et enfin, toute combinaison linéaire de fonctions spline est encore une fonction spline."),
                
                p("Pour r = 2, une spline d'ordre 2 est donc une fonction continue et linéaire par morceaux. Les splines les plus fréquemment utilisées sont les splines d'ordre 4 dites splines cubiques", style = "font-family: 'times'; font-si16pt"),
                
               # strong("strong() makes bold text."),
               # em("em() creates italicized (i.e, emphasized) text."),
               ## br(),
               # code("code displays your text similar to computer code"),
               # div("div creates segments of text with a similar style. This division of text is all blue because I passed the argument 'style = color:blue' to div", style = "color:blue"),
               # br(),
               # p("span does the same thing as div, but it works with",
                 # span("groups of words", style = "color:blue"),
                 # "that appear inside a paragraph.")
                
                ),
            
 
                
                
                tabPanel("La fonction",
                         plotOutput("plot"),
                         headerPanel(""),
                         tableOutput(""))

            )
        )
    )
)

# Définir la logique du serveur


# Run the application 
shinyApp(ui = ui, server = server)
