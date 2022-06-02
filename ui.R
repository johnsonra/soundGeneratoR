# ui.R
# https://github.com/johnsonra/soundGeneratoR
# 

library(shiny)
library(shinydashboard)

library(tuneR)

# number of iterations in the harmonic series
k <- 32

# Define UI for application that allows playing around with sound waves
dashboardPage(
    #################### Header ####################
    dashboardHeader(title = "The Harmonic Series", titleWidth = 400),
    
    ################### Sidebar ####################
    dashboardSidebar(disable = TRUE),
    
    ##################### Body #####################
    dashboardBody(
        fluidRow(
            # interactive frequency and amplitude
            column(width = 3, offset = 0.5,
                   # fundamental tone
                   textInput("fnd_frq", "Frequency:", 440),
                   
                   # relative amplitude (volume) of each component
                   sliderInput("amp_1", "Frequency 1 (fundamental):", 0, 1, 1),
                   lapply(2:k, function(i) sliderInput(paste0("amp_", i), 
                                                       paste0("Frequency ", i, ":"),
                                                       0, 1, 1/i))
            ),
            
            # Plots
            column(width = 5,
                   plotOutput("componentWaves"),
                   plotOutput("summedWave")),
         
            # Other settings
            column(width = 3,
                   actionButton("play", "Play Sample"),
                   actionButton("arpeggio", "Play arpeggio"),
                   
                   # radioButtons("wave_shape", "Wave Shape", 
                   #              choices = c('sin', 'sawtooth', 'triangle', 'square'),
                   #              selected= 'sin'),

                   h5(strong("Preset Amplitudes")),
                   actionButton("fund_only", "Pure Tone"),
                   actionButton("trumpet", "Trumpet"),
                   actionButton("one_over_n", "Sawtooth"),
                   actionButton("square", "Square"),
                   # actionButton("triangle", "Triangle"),
                   actionButton("illusion1", "Drop Fundamental"),
                   actionButton("undo_illusion", "Add Fundamental"))
        )
    )
)