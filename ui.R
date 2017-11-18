#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Let's compare Regression, Random Forests and Neural Nets !"),
  h3(div("Samir Ghoudrani, 04/08/2017", style = "color: blue;")),
  h4(div("Procedure: the 3 algo are trained over 80% of total data (you build bellow), than the RMSE test is done on the 20% remaining testing set. You can set various parameters to get intuition about each algo's behaviour.", style = "color: black;")),
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      h2(div("1/ Simulation of data", style = "color: brown;")),
      
      sliderInput("N_data","Size of dataset",
                  100,1000,value=c(400)),
      selectInput("relation", "Define the relation between Output and Entry 1 / Entry 2 / Entry 3:",
                  c("Linear" = "lin",
                    "Add_Square_terms" = "sq",
                    "Add_Cubic_terms" = "cub")),
      sliderInput("noise","Noise to add to Output:",
                  0,5,value=c(0)),
      dataTableOutput("table"),
      
      h2(div("2/ Settings for Neural Network", style = "color: brown;")),
      ##h4(div("Settings for Regression", style = "color: black;")),
      ##sliderInput("Nb_UO","Number of weeks of observation before transformation",
        ##          0,150,value=c(80)),
      ##h4(div("Settings for Random Forests", style = "color: black;")),
      ##sliderInput("Nb_UO","Number of weeks of observation before transformation",
        ##          0,150,value=c(80)),
      ##h4(div("Settings for Neural Networks", style = "color: black;")),
      sliderInput("Nb_layers","Number of layers",
                  1,10,value=c(4)),
      sliderInput("Nb_neurons","Number of neurons / layer",
                  1,10,value=c(4))
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      ##img(src='image.png',align = "center",width=1200),
       div(img(src='image.png'), style="text-align: center;"),
       plotOutput("distPlot1"),
       ##plotOutput("distPlot2")
       ##plotOutput("distPlot3")
       plotOutput("distPlot5"),
       plotOutput("distPlot4")
       
    )
  )
))
