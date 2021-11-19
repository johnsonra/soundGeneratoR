# server.R

library(shiny)

library(tuneR)
# set wave player for Mac
# see this post for more information on porting to the web: https://stackoverflow.com/questions/36205419/r-shiny-audio-playback
setWavPlayer('/usr/bin/afplay')

library(pracma)

library(dplyr)
options(dplyr.summarise.inform = FALSE)

library(ggplot2)
library(cowplot)
theme_set(theme_cowplot())

# check this out for hand-drawn wave shapes: https://stackoverflow.com/questions/41701807/way-to-free-hand-draw-shapes-in-shiny
# another interesting article: https://en.wikipedia.org/wiki/Harmonic_series_(music)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {

    # bit rate variable for amplitude
    bit_rate <- reactive(16)
    samp_rate <- reactive(80000)
    base_amp <- reactive(2^15-1)
    
    #' generate a sin wave
    #' @param t vector of points over time
    #' @param frq frequency of wave
    #' @param amp amplitude of wave
    #' @param shape character string determining the shape of the sound wave
    soundwave <- function(t, frq, amp, shape = 'sin')
    {
        if(shape == 'sin')
            return(amp*sin(2*pi*frq*t))
    }
    
    # component waves for plots (ignoring frequency)
    waves <- reactive({
        tibble(t  = seq(from = 0, to = 2, length = 500),
               a1 = soundwave(t, 1, base_amp()*input$amp_1),
               a2 = soundwave(t, 2, base_amp()*input$amp_2),
               a3 = soundwave(t, 3, base_amp()*input$amp_3),
               a4 = soundwave(t, 4, base_amp()*input$amp_4),
               a5 = soundwave(t, 5, base_amp()*input$amp_5),
               a6 = soundwave(t, 6, base_amp()*input$amp_6),
               a7 = soundwave(t, 7, base_amp()*input$amp_7),
               a8 = soundwave(t, 8, base_amp()*input$amp_8))
    })
    
    ##### sample plots #####
    output$componentWaves <- renderPlot({
        # see https://tinyurl.com/wjm6ryj
        cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
        
        wv <- waves()

        ggplot(wv, aes(t, a1)) +
            geom_line(color = cbbPalette[1]) +
            geom_line(data = wv, mapping = aes(t, a2), color = cbbPalette[2]) +
            geom_line(data = wv, mapping = aes(t, a3), color = cbbPalette[3]) +
            geom_line(data = wv, mapping = aes(t, a4), color = cbbPalette[4]) +
            geom_line(data = wv, mapping = aes(t, a5), color = cbbPalette[5]) +
            geom_line(data = wv, mapping = aes(t, a6), color = cbbPalette[6]) +
            geom_line(data = wv, mapping = aes(t, a7), color = cbbPalette[7]) +
            geom_line(data = wv, mapping = aes(t, a8), color = cbbPalette[8]) +

            geom_hline(yintercept = 0) +
            theme(axis.title.x=element_blank(),
                  axis.text.x=element_blank(),
                  axis.ticks.x=element_blank(),
                  axis.line.x=element_blank(),
                  axis.title.y=element_blank(),
                  axis.text.y=element_blank(),
                  axis.ticks.y=element_blank())
    })
    
    output$summedWave <- renderPlot({
        
        # sum up all waves
        group_by(waves(), t) %>%
            summarize(w = a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8) %>%
            ungroup() %>%
            
            # plot summed waves
            ggplot(aes(t, w)) +
            geom_line() +
            geom_hline(yintercept = 0) +
            theme(axis.title.x=element_blank(),
                  axis.text.x=element_blank(),
                  axis.ticks.x=element_blank(),
                  axis.line.x=element_blank(),
                  axis.title.y=element_blank(),
                  axis.text.y=element_blank(),
                  axis.ticks.y=element_blank())
    })

    ##### Play sample #####
    observeEvent(input$play, 
    {   
        # three second tone at 80 kHz (good for sample)
        t <- seq(0, 3, 1/samp_rate())
        
        fnd_frq <- as.numeric(input$fnd_frq)
        
        if(!is.na(fnd_frq))
        {
            samp_raw <- soundwave(t, fnd_frq  , input$amp_1) +
                        soundwave(t, fnd_frq*2, input$amp_2) +
                        soundwave(t, fnd_frq*3, input$amp_3) +
                        soundwave(t, fnd_frq*4, input$amp_4) +
                        soundwave(t, fnd_frq*5, input$amp_5) +
                        soundwave(t, fnd_frq*6, input$amp_6) +
                        soundwave(t, fnd_frq*7, input$amp_7) +
                        soundwave(t, fnd_frq*8, input$amp_8)
            
            samp <- Wave(samp_raw / max(samp_raw) * base_amp(), # normalize for 16-bit sample
                         samp.rate = samp_rate(), bit = bit_rate())
        
            play(samp)
        }
    })
})
