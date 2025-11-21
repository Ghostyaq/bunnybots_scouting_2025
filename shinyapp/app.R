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
                    pickerInput("teams_selected", "Select Team", choices = sort(unique(raw$team_number)), multiple = TRUE, options = list(maxOptions = 2)),
                    selectInput("compare_teams_graph", "Choose Graph", choices = c("Cycles Over Time")),
                ),
                mainPanel(
                    plotOutput("compare_teams")
                )
            ),
        ),
               
        tabPanel("Single Team",
            sidebarLayout(
                sidebarPanel(
                    pickerInput("team_selected", "Select Team", choices = sort(unique(raw$team_number)), multiple = FALSE, options = list(maxOptions = 2)),
                    selectInput("single_team_graph", "Choose Graph", choices = c("Cycles Over Time","Auto Points Graph","Endgame Points Graph")),
                ),
                mainPanel(
                    plotOutput("single_team")
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
    
    output$single_team <- renderPlot({
        if (input$single_team_graph == "Cycles Over Time"){
            cycle_history(raw,input$team_selected)
        } 
        else if (input$single_team_graph == "Endgame Points Graph"){
            endgame_bar_graph(raw,input$team_selected)
        }
        else if (input$single_team_graph == "Auto Points Graph"){
            auto_bar_graph(raw,input$team_selected)
        }
    })
    
    output$compare_teams <- renderPlot({
        cycle_history(raw,input$teams_selected)
    })
    
    #COMPARE TEAMS
    auto_bar_graph <- function(raw, selection) {
        data <- raw |>
            select(auto_lunites_high,team_number,auto_lunites_low,moved) |>
            filter(team_number%in%selection) |>
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
    
    endgame_bar_graph<- function(raw,selection) {
        data <-raw|>
            select(bunnies_inside,team_number,end_position)|>
            filter(team_number%in%selection)
        
        data<-data|>
            mutate(
                points=case_when(data$end_position == "none" ~ 0, data$end_position == "linked" ~ 5, .default=0)
            )
        
        ggplot(data, aes(x = end_position)) + 
            geom_bar()
        
        data<-data|>
            group_by(team_number)|>
            summarize(
                Bunnies=mean(bunnies_inside)*6,
                End=mean(points),
            )|>
            
            pivot_longer(cols=c(Bunnies,End),
                         names_to="type",
                         values_to="points")
        
        ggplot(data, aes(x=factor(team_number),y=points, fill=type))+
            geom_bar(position="stack",stat="identity",width=0.3)+
            labs(title=paste("Endgame Points for Team",selected),x="Team Number")+
            theme_bw()
    }
    
    cycle_history<-function(raw,selection) {
        data<- filter(raw, team_number%in%selection)|>
            mutate(auto_cycles=auto_lunites_high+auto_lunites_low+auto_lunites_missed
                   ,teleop_cycles=pre_high_lunites_scored+pre_low_lunites_scored+pre_lunites_missed+pre_lunites_passed+post_high_lunites_scored+post_low_lunites_scored+post_lunites_missed+post_lunites_passed,
                   total_cycles=auto_cycles+teleop_cycles)|>
            select(match_number,auto_cycles,teleop_cycles,total_cycles)
        
        ggplot(data,aes(x=match_number))+
            geom_line(aes(y=auto_cycles,color="red"))+
            geom_line(aes(y=teleop_cycles),color="darkgreen",show.legend = TRUE)+
            geom_line(aes(y=total_cycles),color="black",show.legend = TRUE)+
            labs(x="Match Number", y="Cycles")
    }
    
    boxplot <- function(raw, selection) {
        data <- raw |>
            mutate(
                total_score = 
                    auto_lunites_high * 7 + auto_lunites_low * 4 + pre_high_lunites_scored * 5 + pre_low_lunites_scored * 2 + post_high_lunites_scored * 5 + post_high_lunites_scored * 2 + moved * 4 + ifelse(end_position == "linked", 5, 0) 
            ) |>
            select(match_number, team_number, total_score) |>
            filter(team_number == 449)
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
