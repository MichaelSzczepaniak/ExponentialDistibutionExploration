# using base R graphics
overlayUseBaseRGraphics <- function() {
    g <- rnorm(1000)
    h <- hist(g, breaks=10, density=10, col="lightgray",
              xlab="Accuracy", main="Overall") 
    xfit <- seq(min(g),max(g),length=40) 
    yfit <- dnorm(xfit,mean=mean(g),sd=sd(g)) 
    yfit <- yfit*diff(h$mids[1:2])*length(g) 
    lines(xfit, yfit, col="black", lwd=2)
}

createSimData <- function(exp.rate = 0.2) {
    m.sim <- 1 / exp.rate
    df = data.frame(h = rnorm(1000, mean = m.sim))
    return(df)
}

# using ggplot2
overlayUseGgplot <- function(mean.sim = 5, data.sample) {
    library(ggplot2)
    # get frequency instead of density
    fnorm <- function(scale, ...) { return(scale * dnorm(...)) }
    
    # create some data to work with
    data.sample <- createSimData()
    bw <- 0.25
    freq.scale <- 1000 * bw
    # overlay histogram, empirical density and normal density
    p <- ggplot(data.sample, geom = 'blank')
    p <- p + aes(x = h, colour = 'simulation')
    #p <- p + geom_line(aes(y = ..density.., colour = 'Empirical'), stat = 'density')
    # this works fine
    # p <- p + stat_function(fun = dnorm, arg = list(mean=-1), aes(colour = 'left'))
    # p <- p + stat_function(fun = fnorm, arg = list(mean=0), aes(colour = 'center'))
    p <- p + stat_function(fun = fnorm,
                           arg = list(scale=freq.scale, mean=mean.sim),
                           aes(colour = 'normal'), size = 2)
    p <- p + geom_histogram(aes(x = h), alpha = 0.4, binwidth = bw)
    p <- p + scale_colour_manual(name = '', values = c('red', 'blue'))
    p <- p + theme(legend.position = c(0.85, 0.85),
                   legend.title = element_blank())
    
    print(p)
}