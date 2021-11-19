# ui.R
# https://github.com/johnsonra/soundGeneratoR
# 

library(shiny)
library(shinydashboard)

library(tuneR)


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
                   sliderInput("amp_1", "Amplitude 1 (fundamental):", 0, 1, 1),
                   sliderInput("amp_2", "Amplitude 2:", 0, 1, 1/2),
                   sliderInput("amp_3", "Amplitude 3:", 0, 1, 1/3),
                   sliderInput("amp_4", "Amplitude 4:", 0, 1, 1/4),
                   sliderInput("amp_5", "Amplitude 5:", 0, 1, 1/5),
                   sliderInput("amp_6", "Amplitude 6:", 0, 1, 1/6),
                   sliderInput("amp_7", "Amplitude 7:", 0, 1, 1/7),
                   sliderInput("amp_8", "Amplitude 8:", 0, 1, 1/8)
            ),
            
            # Plots
            column(width = 5,
                   plotOutput("componentWaves"),
                   plotOutput("summedWave")),
         
            # Other settings
            column(width = 3,
                   actionButton("play", "Play Sample"),
                   
                   radioButtons("wave_shape", "Wave Shape", 
                                choices = c('sin', 'sawtooth', 'triangle', 'square'),
                                selected= 'sin'),
        )
    )
)