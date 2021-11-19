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

# number of iterations in the harmonic series
k <- 8

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
    soundwave <- function(t, frq, amp, shape)
    {
        if(shape == 'sawtooth')
            return((-2*amp / pi) * atan(cot(pi*frq*t)))
        
        if(shape == 'triangle')
            return((2*amp / pi) * asin(sin(2*pi*frq*t)))
        
        if(shape == 'square')
            return(amp*sign(sin(2*pi*frq*t)))
        
        # else return sin
        return(amp*sin(2*pi*frq*t))
    }
    
    # component waves for plots (ignoring frequency)
    waves <- reactive({
        retval <- tibble(t  = seq(from = 0, to = 2, length = 500))
        
        # add harmonic series
        for(i in 1:k)
        {
            retval[[paste0('a', i)]] <- soundwave(retval$t, i, 
                                                  base_amp()*input[[paste0("amp_", i)]],
                                                  input$wave_shape)
        }
        
        return(retval)
    })
    
    ##### sample plots #####
    output$componentWaves <- renderPlot({
        # see https://tinyurl.com/wjm6ryj
        cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
        
        wv <- waves()

        g <- ggplot(wv, aes(t, a1)) +
            geom_line(color = cbbPalette[1])
        
        for(i in 2:k)
        {
            g <- g + geom_line(data = wv, 
                               mapping = aes_string('t', paste0('a', i)), 
                               color = cbbPalette[i])
        }

        g + geom_hline(yintercept = 0) +
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
        wvs <- waves() %>%
            mutate(w = a1)
        
        for(i in 2:k)
        {
            wvs$w <- wvs$w + wvs[[paste0('a', i)]]
        }

        # plot summed waves
        ggplot(wvs, aes(t, w)) +
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
            samp_raw <- soundwave(t, fnd_frq  , input$amp_1, input$wave_shape)
            
            for(i in 2:k)
            {
                samp_raw <- samp_raw + soundwave(t, fnd_frq*i, 
                                                 input[[paste0("amp_", i)]], input$wave_shape)
            }
            
            samp <- Wave(samp_raw / max(abs(samp_raw)) * base_amp(), # normalize for 16-bit sample
                         samp.rate = samp_rate(), bit = bit_rate())
        
            play(samp)
        }
    })
    
    ##### Reset Amplitude #####
    observeEvent(input$one_over_n,
                 {
                     for(i in 1:k)
                     {
                         updateSliderInput(session, paste0('amp_', i), value = 1 / i)
                     }
                 })
    
    observeEvent(input$one_over_n2,
    {
        for(i in 1:k)
        {
            updateSliderInput(session, paste0('amp_', i), value = 1 / i^2)
        }
    })
    
    observeEvent(input$lin_decay,
    {
        for(i in 1:k)
        {
            updateSliderInput(session, paste0('amp_', i), value = 1 - (i-1)/8)
        }
    })
    
    observeEvent(input$fund_only,
    {
        updateSliderInput(session, 'amp_1', value = 1)

        for(i in 2:k)
        {
            updateSliderInput(session, paste0('amp_', i), value = 0)
        }
    })
})
