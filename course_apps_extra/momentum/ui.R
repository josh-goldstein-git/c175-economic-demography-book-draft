## ui.R for momentum

## this is the user-interface
## a slider with time "t"

library(shiny)

shinyUI(
    fluidPage(
        ## Application title
        titlePanel("Momentum simulation"),
        ## h4("Choose parameters"),
        p("(1) Hit play, or (2) adjust parameters and hit play, (3) or adjust parameters after hitting play"),
        ## Sidebar with a slider input for the number of bins
        sidebarLayout(
            sidebarPanel(
                width = 3,
                sliderInput("this_T",
                            "T: current projection step",
                            min = 0,
                            max = 20,
                            value = 0,
                            animate = TRUE),
                sliderInput("f1",
                            "firstrow[1]",
                            min = 0,
                            max = 5,
                            step = .1,
                            value = 0.8),
                sliderInput("f2",
                            "firstrow[2]",
                            min = 0,
                            max = 5,
                            step = .1,
                            value = 0.8),
                sliderInput("f3",
                            "firstrow[3]",
                            min = 0,
                            max = 5,
                            step = .1,
                            value = 0),
                sliderInput("f4",
                            "firstrow[4]",
                            min = 0,
                            max = 20,
                            value = 0),
                sliderInput("f5",
                            "firstrow[5]",
                            min = 0,
                            max = 20,
                            value = 0),
                sliderInput("s1",
                            "subdi[1]",
                            min = 0,
                            max = 1,
                            value = .95),
                sliderInput("s2",
                            "subdi[2]",
                            min = 0,
                            max = 1,
                            value = .9),
                sliderInput("s3",
                            "subdi[3]",
                            min = 0,
                            max = 1,
                            value = .8),
                sliderInput("s4",
                            "subdi[4]",
                            min = 0,
                            max = 1,
                            value = .6)
            ),
            ## Show a plot of the generated distribution
            mainPanel(
                plotOutput("momentumPlot"),
##                h6("Note: insert note here")
            )
        )
    )
)

