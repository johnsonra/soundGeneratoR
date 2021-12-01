# ui.R
# https://github.com/johnsonra/soundGeneratoR
# 

library(shiny)
library(shinydashboard)

library(tuneR)

# number of iterations in the harmonic series
k <- 8

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
                   
                   radioButtons("wave_shape", "Wave Shape", 
                                choices = c('sin', 'sawtooth', 'triangle', 'square'),
                                selected= 'sin'),
                   
                   h5(strong("Reset Amplitudes")),
                   actionButton("one_over_n", "amp = 1/n"),
                   actionButton("one_over_n2", "amp = 1/n^2"),
                   actionButton("lin_decay", "amp = 1 - (n-1)/k"),
                   actionButton("fund_only", "amp = {1, 0, ..., 0}"),
                   actionButton("illusion1", "amp = {0, 1/n}"))
        )
    )
)