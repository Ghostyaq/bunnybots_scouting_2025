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
        tabPanel("Event Summary",
            sidebarLayout(
                sidebarPanel(
                    sliderInput("bins1", "Number of bins:", min = 1, max = 50, value = 30)
                ),
                mainPanel(
                    plotOutput("distPlot1")
                )
            )
        ),
        tabPanel("Match",
            sidebarLayout(
                sidebarPanel(
                    sliderInput("bins1", "Number of bins:", min = 1, max = 50, value = 30)
                ),
                mainPanel(
                    plotOutput("distPlot1")
                )
            )
        ),
        
        tabPanel("Compare Team",
            sidebarLayout(
                sidebarPanel(
                    selectInput("bins2", "Number of bins:", choices = NULL),
                    selectInput("typeGraph", "Choose graph:", choices = c("Points Large Bar Graph", "Cylces graph"))                ),
                mainPanel(
                    plotOutput("Single Teams Graphs")
                )
            )
        ),
        tabPanel("Scouts",
                   sidebarLayout(
                       sidebarPanel(
                           
                       ),
                       mainPanel(

                       )
                   ),
        ),
    ),
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    teams <- read.csv("data_files/fake_teams.csv")
    raw <- read.csv("data_files/fake_data.csv")
    
    observe({
        updateSelectInput(session, "bins2", choices = teams$team[order(teams$team)])
    })
    output$'Single Teams Graphs' <- renderPlot({
        
        
        if (input$typeGraph == "Points Large Bar Graph") {
            boxplot(raw, input$bins2)
        }
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
    
    boxplot <- function(raw, selection) {
        data <- raw |>
            mutate(
                total_score = 
                    auto_lunites_high * 7 + auto_lunites_low * 4 + pre_high_lunites_scored * 5 + pre_low_lunites_scored * 2 + post_high_lunites_scored * 5 + post_high_lunites_scored * 2 + moved * 4 + ifelse(end_position == "linked", 5, 0) 
            ) |>
            select(match_number, team_number, total_score) |>
            filter(team_number == selection)
        ggplot(data, aes(x = match_number, y = total_score)) +
            geom_boxplot() +
            ggbeeswarm::geom_quasirandom(#created dots representing every match score
                shape = 21, color = "white", 
                alpha = 0.8, size = 3,
                aes(fill = "red")
            )  + theme_bw() +
            labs(
                title ="Score per Match", 
                x = "Match Number",
                y = "Total Score", 
                fill = "Alliance Number"
            )
    }
}

# Run the application 
shinyApp(ui = ui, server = server)
