## ui.R for exponential growth

library(shiny)

## Define UI for application that draws dynmic points for exponential
## growth

shinyUI(fluidPage(

    # Application title
    titlePanel("Stable Age Pyramid"),

    # Sidebar with a slider for input for R
    sidebarLayout(
        sidebarPanel(
            ## sliderInput("r",
            ##             "Stable growth rate r:",
            ##             min = -.03,
            ##             max = .03,
            ##             value = .00,
            ##             step = .005,
            ##             animate = TRUE),
            ## sliderInput("period",
            ##             "Swedish mortality for the following period:",
            ##             min = 1750,
            ##             max = 2010,
            ##             value = 1900,
            ##             step = 10,
            ##             animate = TRUE)
            sliderInput("tfr",
                        "Total Fertility Rate: ",
                        min = 1,
                        max = 8,
                        value = 2,
                        step = .1,
                        animate = TRUE),
            sliderInput("e0",
                        "Life expectancy at birth: ",
                        min = 35,
                        max = 80,
                        value = 60,
                        step = 1,
                        animate = TRUE)

            ),


        # Show the plot of growth
        mainPanel(
            plotOutput("plot.age.pyramid")
            )
        )
    ))

