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
k <- 32

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {

    # bit rate variable for amplitude
    bit_rate <- reactive(16)
    samp_rate <- reactive(80000)
    base_amp <- reactive(2^15-1)
    
    # default instrument
    vals <- reactiveValues(instrument = 'none')
    
    #' generate a sin wave
    #' @param t vector of points over time
    #' @param frq frequency of wave
    #' @param amp amplitude of wave
    #' @param adj character string to identify adjustment (theta) of the sound wave for more complex wave forms
    soundwave <- function(t, frq, amp, n, adj = isolate(vals$instrument))
    {
        theta <- list(none = rep(0, k),
                      trumpet = c(0, -2.2199, 1.6727, -2.5454, .6607, -2.039, 2.1597, -1.0467, 1.8581, -2.3925, rep(0, k)))
        
        # return sine wave of correct frequency and amplitude
        return(amp*sin(2*pi*frq*t - theta[[adj]][n]))
    }
    
    # component waves for plots (ignoring frequency)
    waves <- reactive({
        retval <- tibble(t  = seq(from = 0, to = 2, length = 500))
        
        # add harmonic series
        for(i in 1:k)
        {
            retval[[paste0('a', i)]] <- soundwave(retval$t, i, 
                                                  base_amp()*input[[paste0("amp_", i)]],
                                                  i)
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
            samp_raw <- soundwave(t, fnd_frq  , input$amp_1, 1)
            
            for(i in 2:k)
            {
                samp_raw <- samp_raw + soundwave(t, fnd_frq*i, 
                                                 input[[paste0("amp_", i)]], i)
            }
            
            samp <- Wave(samp_raw / max(abs(samp_raw)) * base_amp(), # normalize for 16-bit sample
                         samp.rate = samp_rate(), bit = bit_rate())
        
            play(samp)
        }
    })
    
    observeEvent(input$arpeggio,
    {
        # ten second tone at 80 kHz (good for arpeggio)
        t <- seq(0, 10, 1/samp_rate())
        
        fnd_frq <- as.numeric(input$fnd_frq)
        
        if(!is.na(fnd_frq))
        {
            samp_raw <- soundwave(t, fnd_frq  , input$amp_1, 1)
            
            # add new note every 1/2 second, with everything in after 8 seconds
            for(i in 2:min(k, 16))
            {
                # only add the ith harmonic after i/2 seconds
                clip <- round(samp_rate()/2 * (i-1)):length(t)
                
                samp_raw[clip] <- samp_raw[clip] + 
                    soundwave(t, fnd_frq*i, input[[paste0("amp_", i)]], i)[clip]
            }
            
            if(k > 16)
            {
                for(i in 17:k)
                {
                    # add the rest in at 8.5 seconds (if there are any remaining to add)
                    clip <- round(samp_rate()/2 * 8):length(t)
                    
                    samp_raw[clip] <- samp_raw[clip] + 
                        soundwave(t, fnd_frq*i, input[[paste0("amp_", i)]], i)[clip]
                }
            }
            
            samp <- Wave(samp_raw / max(abs(samp_raw)) * base_amp(), # normalize for 16-bit sample
                         samp.rate = samp_rate(), bit = bit_rate())
            
            play(samp)
        }
    })
    
    ##### Reset Amplitude #####
    observeEvent(input$one_over_n, # this turns out to be a sawtooth wave
                 {
                     vals$instrument <- 'none'
                     
                     for(i in 1:k)
                     {
                         updateSliderInput(session, paste0('amp_', i), value = 1 / i)
                     }
                 })
    
    observeEvent(input$fund_only,
    {
        vals$instrument <- 'none'
        
        updateSliderInput(session, 'amp_1', value = 1)

        for(i in 2:k)
        {
            updateSliderInput(session, paste0('amp_', i), value = 0)
        }
    })
    
    observeEvent(input$illusion1,
                 {
                     # if there is something other than the fundamental - drop it
                     # otherwise ignore the button click
                     if(sum(sapply(paste0('amp_', 2:k), function(.x) input[[.x]])) > 0)
                        updateSliderInput(session, 'amp_1', value = 0)
                 })
    
    observeEvent(input$undo_illusion,
                 {
                     # add fundamental back in
                     updateSliderInput(session, 'amp_1', value = 1)
                 })
    
    observeEvent(input$square,
                 {
                     vals$instrument <- 'none'
                     
                     for(i in 1:k)
                     {
                         if(i %in% (1:50 * 2 - 1)) # odd
                         {
                            updateSliderInput(session, paste0('amp_', i), value = 1 / i)
                         }else{
                             updateSliderInput(session, paste0('amp_', i), value = 0)
                         }
                     }
                 })
    
    observeEvent(input$triangle,
                 {
                     vals$instrument <- 'none'
                     
                     for(i in 1:k)
                     {
                         if(i %in% (1:50 * 2 - 1)) # odd
                         {
                             updateSliderInput(session, paste0('amp_', i), value = (-1)^((i-1)/2) / i^2)
                         }else{
                             updateSliderInput(session, paste0('amp_', i), value = 0)
                         }
                     }
                 })
    
    observeEvent(input$trumpet,
                 {
                     vals$instrument <- 'trumpet'
                     
                     # see page 18 of https://web.eecs.umich.edu/~fessler/course/100/misc/course-notes-ay-jf.pdf
                     ck <- c(.1155, .3417, .1789, .1232, .0678, .0473, .0260, .0045, .0020) / .3417
                     
                     for(i in 1:min(k, 9))
                         updateSliderInput(session, paste0('amp_', i), value = ck[i])
                     
                     if(k > 9)
                         for(i in 10:k)
                             updateSliderInput(session, paste0('amp_', i), value = 0)
                 })
})
