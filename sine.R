
library(dplyr)
library(ggplot2)
library(gganimate)
library(patchwork)
library(cowplot)
theme_set(theme_cowplot())

# sine wave gif
l <- 360

dat <- tibble(deg = seq(from = 0, to = 360, length = l),
              rad = 2*pi * deg / 360,
              y = sin(rad),
              x = cos(rad))

axis_labels1 <- tibble(x = c(c(1, 0, -1, -.01) - .07,
                             c(.5, -.08, -.5, -.09)),
                      y = c(c(.07, 1.05, .07, -.95),
                            c(.07, .5, .07, -.5)),
                      lab = c('1.0', '1.0', '-1.0', '-1.0',
                              '0.5', '0.5', '-0.5', '-0.5'))
axis_ticks1x <- tibble(x = c(.5, -.5),
                       y = rep(0, 2))
axis_ticks1y <- tibble(x = rep(0, 2),
                       y = c(.5, -.5))

axis_labels2 <- tibble(x = c(0, 90, 180, 270, 360),
                       y = 0,
                       yadj = -0.07,
                       lab = as.character(x))

png("sineWave/Rplot%03d.png", width = 10, height = 5, units = 'in', res = 200)
for(i in c(1:nrow(dat), (nrow(dat)-1):1))
{
    # circle
    circ <- ggplot(dat, aes(x, y)) +
        geom_path() +
       
        xlab('') +
        ylab('') +
        theme(axis.text = element_blank(),
              axis.title = element_blank(),
              axis.ticks = element_blank(),
              axis.line = element_blank()) +
        geom_hline(yintercept = 0) +
        geom_vline(xintercept = 0) +
        geom_text(aes(x, y, label = lab), data = axis_labels1) +
        geom_point(aes(x, y), data = axis_ticks1x, pch = '|', cex = 3) +
        geom_point(aes(x, y), data = axis_ticks1y, pch = '-', cex = 6) +
        
        geom_point(data = dat[i,], color = 'red')
        

    # sine wave
    wav <- ggplot(dat, aes(deg, y)) +
        geom_path() +
        
        theme(axis.text.x = element_blank(),
              axis.title = element_blank(),
              axis.ticks.x = element_blank(),
              axis.line.x = element_blank()) +
        geom_hline(yintercept = 0) +
        geom_text(aes(x, yadj, label = lab), data = axis_labels2) +
        geom_point(aes(x, y), data = axis_labels2, pch = '|', cex = 3) +
        
        geom_point(data = dat[i,], color = 'red')
    
    print(circ + wav)
}
dev.off()

system('convert -delay 3 -loop 0 sineWave/*.png sinewav.gif')
