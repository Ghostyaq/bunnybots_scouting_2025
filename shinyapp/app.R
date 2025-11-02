library(shiny)
library(DT)
library(ggplot2)
library(plotly)
library(dplyr)
library(scales)
library(shinyWidgets)
library(tidyverse)
library(shinythemes)

blair_red <- "#a7000a"
in_rstudio <- requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()

# Define UI for application that draws a histogram
ui <- fluidPage(
    navbarPage(theme = shinytheme("yeti"), 
               "449 Bunnybots Scouting",
        tabPanel("Match",
            sidebarLayout(
                sidebarPanel(
                    sliderInput("bins1", "Number of bins:",min = 1,max = 50,value = 30)
                ),
                mainPanel(
                    plotOutput("distPlot1")
                )
            )
        ),
               
        tabPanel("Compare Teams",
            sidebarLayout(
                sidebarPanel(
                    sliderInput("bins2", "Number of bins:",min = 1,max = 50,value = 30)
                ),
                mainPanel(
                    plotOutput("distPlot2")
                )
            ),
        ),
               
        tabPanel("Single Team",
            sidebarLayout(
                sidebarPanel(
                    sliderInput("bins3","Number of bins:",min = 1,max = 50,value = 30)
                ),
                mainPanel(
                    plotOutput("distPlot3")
                )
            )
        ),
    ),
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    raw <- read.csv("data_files/fake_data.csv")

    output$distPlot1 <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins1 + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')
    })
    
    output$distPlot2 <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins2 + 1)
        
        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')
    })
    
    output$distPlot3 <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins3 + 1)
        
        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')
    })
    
    #COMPARE TEAMS
    auto_bar_graph <- function(raw, selection) {
        data <- raw |>
            select(auto_lunites_high,team_number,auto_lunites_low,moved) |>
            filter(team_number %in% selection) |>
            group_by(team_number) |>
            summarize(
                ALH=mean(auto_lunites_high)*7,
                ALL=mean(auto_lunites_low)*4,
                move=mean(moved)*4
            )|>
            pivot_longer(cols=c(ALL,ALH,move),
                         names_to="type",
                         values_to="points")
        
        ggplot(data, aes(x=factor(team_number),y=points, fill=type))+
            geom_bar(position="stack",stat="identity",width=0.3)+
            labs(title=paste("Auto Points for Team",select))
    }
}

# Run the application 
shinyApp(ui = ui, server = server)
