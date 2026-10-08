## server for exponential growth

library(shiny)

## if code is here it will run ONCE (when app is launched)
## plot.age.pyramid <- function(N0=1, R=.01, maxtime = 30, ...)
## {
##     N.vec <- NULL
##     N.vec[1] <- N0
##     time.placeholder <- 1:maxtime
##     N.placeholder <- N.vec[1]*exp(R*(time.placeholder-1))
##     plot(time.placeholder, N.placeholder, type = "n",
##          xlab = "Time", ylab = "Population")
##     for (i in 1:(maxtime-1))
##     {
##         N.vec[i+1] <- N.vec[i]* exp(R*1)
## ##            Sys.sleep(0.01)
##         points(i, N.vec[i], ...)
##     }
## }

## get.Lx <- function(country.ABB, period)
## {
##     ## read the file
##     myfile <- paste0("~/Documents/hmd/hmd_statistics/lt_both/bltper_5x10/",
##                      country.ABB, ".bltper_5x10.txt")
##     df <- read.table(myfile, skip = 2, header = T, as.is = T)
##     period.vec <- names(table(df$Year))

##     my.year <- period
##     if (sum(period.vec %in% my.year) != 1) {
##         stop(paste("exact period \"", period, "\"", "not found\n"),
##         "try one in \n",

##         paste(period.vec, collapse = " "))
##     }

##     ## get raw Lx
## ##     my.year <- period.vec[1]
##     Lx.raw <- df$Lx[df$Year == my.year]
##     x.raw <- df$Age[df$Year == my.year]
##     ## reformat Lx
##     ## now combine 0-1 and 1-4, and drop 110+
##     s <- x.raw %in% c("0", "1-4")
##     Lx <- c(sum(Lx.raw[s]), Lx.raw[!s])
##     ## drop oldest
##     Lx <- Lx[-length(Lx)]
##     radix <- 100000
##     Lx <- Lx/radix
##     x <- seq(0, 105, 5)
##     names(Lx) <- x

##     ## check with life expectancy, ok
##     e0.hat <- sum(Lx)
##     e0.reported <- df$ex[df$Year == my.year & df$Age == "0"]
## ##     print(e0.hat)
## ##    print(e0.reported)
##     if (abs(e0.hat - e0.reported) > .1)
##         print(paste("error: e0.hat = ", e0.hat,
##                     "e0.reported = ", e0.reported))
##     return(Lx)
## }


get.period <- function(my.period, period.vec)
{
    ## parse period.vec to get start of each period
    start.vec <- unlist(lapply(strsplit(period.vec, "-"), "[", 1))
    start.vec <- as.numeric(start.vec)
    ## now find highest number that is lower than my.period
    s <- start.vec < my.period
    if (sum(s) > 0)
        period <- period.vec[which.max(start.vec[s])]
    if (sum(s) == 0)
        period <- period.vec[1]
    return(period)
}

get.Lx <- function(country.ABB, period)
{

    ## read the file
##     myfile <- paste0("~/Documents/hmd/hmd_statistics/lt_both/bltper_5x10/",
##                     country.ABB, ".bltper_5x10.txt")
    myfile <- paste0("./", country.ABB, ".bltper_5x10.txt")
    df <- read.table(myfile, skip = 2, header = T, as.is = T)
    period.vec <- names(table(df$Year))

    my.year <- get.period(period, period.vec)
    ## get raw Lx
    Lx.raw <- df$Lx[df$Year == my.year]
    x.raw <- df$Age[df$Year == my.year]
    ## reformat Lx
    ## now combine 0-1 and 1-4, and drop 110+
    s <- x.raw %in% c("0", "1-4")
    Lx <- c(sum(Lx.raw[s]), Lx.raw[!s])
    ## drop oldest
    Lx <- Lx[-length(Lx)]
    radix <- 100000
    Lx <- Lx/radix
    x <- seq(0, 105, 5)
    names(Lx) <- x

    ## check with life expectancy, ok
    e0.hat <- sum(Lx)
    e0.reported <- df$ex[df$Year == my.year & df$Age == "0"]
    if (abs(e0.hat - e0.reported) > .1)
        print(paste("error: e0.hat = ", e0.hat,
                    "e0.reported = ", e0.reported))
    return(Lx)
}


my.Lx <- get.Lx(country.ABB = "SWE", period = c("1751-1759"))

barplot.age.pyramid <- function(Lx = my.Lx,
                             x = as.numeric(names(Lx)),
                             r = .01,
                             ...)
{
    b <- 1 / sum(Lx * exp(-r * x))
    cx <- b * Lx * exp(-r * x)
    ## check to see cx sums to 1
    if (round(sum(cx),4) != 1)
        print(paste("error: cx sums to", sum(cx)))
    barplot(cx, horiz = T, names.arg = x, las = 1, ## axes = F,
##         col = c(rep("red", 3), rep("green",9), rep("red", 9)),
            col = "orange",
        xlab = "Proportion in each age group",
            main = paste0("growth rate ", round(r, 3)*100, "%"))
}



get.r <- function(tfr, e0, e0.table)
{
    my.period <- e0.table$my.period.vec[e0.table$my.e0.vec == floor(e0)]
    Lx <- get.Lx(country.ABB = "SWE", period = my.period)
    l.mu <- Lx["25"]/5
    r = log(tfr * .4886 * l.mu)/30
    r
}

e0.table <- read.table("e0_index_table.txt", as.is = T)
## plot.age.pyramid <- function(r, period)
plot.age.pyramid <- function(tfr, e0, e0.table)
{
    my.period <- e0.table$my.period.vec[e0.table$my.e0.vec == floor(e0)]
    Lx <- get.Lx(country.ABB = "SWE", period = my.period)
    l.mu <- Lx["25"]/5
    r = log(tfr * .4886 * l.mu)/30
##     par(mfrow = c(1,2))
    barplot.age.pyramid(r = r, x = as.numeric(names(Lx)), Lx)
##     title(paste("period", period, ": e0 = ", round(sum(Lx)),1),
##          line = .5)
    title(paste("Sweden life expectancy period:", my.period), line = .5)
    ## plot 2
    if (0) {
    plot(y = as.numeric(names(Lx)), x = Lx/5, type = "o",
         xlab = "Chance of survival",
         ylab = "age",
##         as.numeric(names(Lx)),
         las = 1)
##     title(paste("period", period, ": e0 = ", round(sum(Lx)),1),
##          line = .5)
    title(paste("period:", my.period), line = .5)
}
}


## Define server logic required to draw growth plot

shinyServer(function(input, output) {

    ## Expression that generates the plot. The expression is
    ## wrapped ina a call to renderPlot to indicate that
    ##
    ## 1) It is "reactive", re-executing automatically when inputs
    ##     change
    ## 2) Output type is a plot

    output$plot.age.pyramid <- renderPlot({
        ## draw with defaults
        par(mfrow = c(1,1))
        plot.age.pyramid(tfr=input$tfr,
                         e0 = input$e0,
                         e0.table = e0.table)
##    })
            },
                                          width = 400)
})
