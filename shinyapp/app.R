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

ui <- fluidPage(
    navbarPage(theme = shinytheme("yeti"), 
               "449 Bunnybots Scouting",
        tabPanel("Event Summary",
            fluidRow(
                column(12,
                    plotOutput("event_summary"),
                    DTOutput("event_table")
                ),
            )
        ),
        tabPanel("Match",
            sidebarLayout(
                sidebarPanel(
                    radioButtons("selected_mode", "", choices = c("Predictory", "Historical", "Playoffs")),
                    conditionalPanel(
                        condition = "input.selected_mode == 'Playoffs'",
                        numericInput("red_alliance", "Red Alliance", value = 1),
                        numericInput("blue_alliance", "Blue Alliance", value = 8)
                    ),
                    conditionalPanel(
                        condition = "input.selected_mode != 'Playoffs'",
                        numericInput("selected_match", "Match Number", value = 1, min = 0),
                    )
                ),
                mainPanel(
                    uiOutput("score_prediction"),
                    plotOutput("match_boxplot"),
                    plotOutput("match_all_pts"),
                    plotOutput("match_auto_pts"),
                    plotOutput("match_tele_cycles"),
                    plotOutput("match_tele_pts"),
                    DTOutput("match_table")
                    
                )
            )
        ),
        tabPanel("Match",
            sidebarLayout(
                mainPanel(
                    pickerInput("teams_selected", "Select Team", choices = NULL, multiple = TRUE, options = list(maxOptions = 2)),
                    selectInput("compare_teams_graph", "Choose Graph", choices = c("Cycles Over Time")),
                ),
                mainPanel(
                    plotOutput("compare_boxplots"),
                    plotOutput("compare_all_pts"),
                    plotOutput("match_hist"),
                    plotOutput("compare_comments"),
                    DTOutput("compare_table")
                )
            ),
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
                 fluidRow(
                     column(12,
                            plotOutput("scouts_num")
                     )
                 ),
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    teams <- read.csv("data_files/fake_teams.csv")
    raw <- read.csv("data_files/fake_data.csv")

    observe({
        updatePickerInput(session, "teams_selected", choices = sort(unique(raw$team_number)))
        updatePickerInput(session, "team_selected", choices = sort(unique(raw$team_number)))
    })
    output$event_summary <- renderPlot({
        event_summary(raw)
    })
    
    output$event_table <- renderDT({
        datatable(
            modified, 
            options = 
                list(
                    dom = "ft", 
                    lengthChange = FALSE, 
                    rowNames = FALSE, 
                    scrollX = TRUE, 
                    scrollY = 500, 
                    pageLength = nrow(modified)))
    })
    
    output$score_prediction <- renderUI({
        
    })
    
    output$match_boxplot <- renderPlot({
        
    })
    
    output$match_all_pts <- renderPlot({
        
    })
    
    output$match_auto_pts <- renderPlot({
        
    })
    
    output$match_tele_cycles <- renderPlot({
        
    })
    
    output$match_tele_pts <- renderPlot({
        
    })
    
    output$match_table <- renderDT({
        
    })
    
    output$compare_boxplots <- renderPlot({
        teams <- input$teams_selected
        boxplot(raw, teams)
    })
    
    output$compare_all_pts <- renderPlot({
        teams <- input$teams_selected
    })
    
    output$match_hist <- renderPlot({
        teams <- input$teams_selected
    })
    
    output$compare_comments <- renderPlot({
        teams <- input$teams_selected
    })
    
    #EVENT SUMMARY
    event_summary <- function(raw){
        data <- raw |>
            group_by(team_number) |>
            summarize(
                auto_low = mean(auto_lunites_low) * 4,
                auto_high = mean(auto_lunites_high) * 7,
                auto_missed = mean(auto_lunites_missed),
                tele_highs = mean(pre_high_lunites_scored + post_high_lunites_scored) * 5,
                tele_low = mean(pre_low_lunites_scored + post_low_lunites_scored) * 2,
                tele_missed = mean(pre_lunites_missed + post_lunites_missed),
                tele_passed = mean(pre_lunites_passed + post_lunites_passed),
                move = mean(moved) * 4,
                end = mean(end_position == "linked") * 5,
                avg_score = auto_low + auto_high + auto_missed + tele_highs +
                    tele_low + tele_missed + tele_passed + move + end
            )|>
            pivot_longer(
                cols = c(
                    move, end, auto_low, auto_high, auto_missed, tele_highs, 
                    tele_low, tele_missed, tele_passed), 
                names_to = "type", values_to = "points_score")
        
        unique_teams <- data |>
            select(team_number, avg_score) |>
            distinct() |>
            arrange(desc(avg_score))
        
        # Use the unique sorted teams to create a proper factor
        data$team_number <- factor(data$team_number, 
                               levels = unique_teams$team_number, 
                               ordered = TRUE)

        ggplot(data, aes(x = team_number, y = points_score, fill = type)) +
            geom_bar(position = "stack", stat = "identity") +
            labs(title = paste("Level Summary for Team", data$team_number), x = "Team", y = "Score", fill = "Points") + 
            scale_fill_manual(
                values = c(
                    "plum1", "plum3", "plum4", "#FFF68F", "#FFC156", "#3D5A80",
                    "#98C1D9", "#7DAA92", "#003B36"
                ),
                labels = c(
                    "auto_low" = "Auto Low", "auto_high" = "Auto High",
                    "auto_missed" = "Auto Missed", "tele_highs" = "Tele High",
                    "tele_low" = "Tele Low", "tele_missed" = "Tele Missed",
                    "tele_passed" = "Tele Passed", "end" = "End", "move" = "Move"
                )
            ) +
            coord_flip() + 
            theme_bw()
    }
        
    
    
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
            labs(title= paste("Auto Points for Team", data$team_number))
    }
    
    endgame_bar_graph <- function(raw,selection) {
        data <- raw|>
            select(bunnies_inside,team_number,end_position)|>
            filter(team_number %in% selection) |>
            mutate(
                points = 
                    case_when(
                        end_position == "none" ~ 0, 
                        end_position == "linked" ~ 5, 
                        .default=0)
            ) |>
            group_by(team_number)|>
            summarize(
                Bunnies = mean(bunnies_inside)*6,
                End=mean(points),
            )|>
            pivot_longer(cols=c(Bunnies,End),
                         names_to="type",
                         values_to="points")
        
        ggplot(data, aes(x=factor(team_number),y=points, fill=type))+
            geom_bar(position="stack", stat="identity",width=0.3)+
            labs(title = paste("Endgame Points for Team", selection), x="Team Number") +
            theme_bw()
    }
    
    cycle_history<-function(raw,selection) {
        data <- filter(raw, team_number %in% selection)|>
            mutate(
                auto_cycles= 
                    auto_lunites_high + 
                    auto_lunites_low + 
                    auto_lunites_missed,
                teleop_cycles= 
                    pre_high_lunites_scored + pre_low_lunites_scored +
                    pre_lunites_missed + pre_lunites_passed + 
                    post_high_lunites_scored + post_low_lunites_scored +
                    post_lunites_missed + post_lunites_passed,
                total_cycles=auto_cycles + teleop_cycles
                )|>
            select(match_number, auto_cycles, teleop_cycles, total_cycles)
        
        ggplot(data,aes(x=match_number))+
            geom_line(aes(y=auto_cycles,color="red"))+
            geom_line(aes(y=teleop_cycles),color="darkgreen",show.legend = TRUE)+
            geom_line(aes(y=total_cycles),color="black",show.legend = TRUE)+
            labs(x="Match Number", y="Cycles")
    }
    
    boxplot <- function(raw, selection) {
        data <- raw |>
            filter(team_number %in% selection) |>
            mutate(
                total_score = 
                    auto_lunites_high * 7 + auto_lunites_low * 4 + 
                    pre_high_lunites_scored * 5 + pre_low_lunites_scored * 2 + 
                    post_high_lunites_scored * 5 + post_high_lunites_scored * 2 + 
                    moved * 4 + ifelse(end_position == "linked", 5, 0),
                team_number = factor(team_number, levels = selection)
            ) |>
            select(match_number, team_number, total_score) |>
            filter(team_number == selection)
        ggplot(data, aes(x = match_number, y = total_score)) +
            select(match_number, team_number, total_score)
        ggplot(data, aes(x = total_score, y = team_number)) +
            geom_boxplot() +
            ggbeeswarm::geom_quasirandom(#created dots representing every match score
                shape = 21, color = "white", 
                alpha = 0.8, size = 3,
                aes(fill = "red")
            )  + theme_bw() +
            labs(
                title ="Score per Match", 
                x = "Total Score",
                y = "Team Number", 
                fill = "Alliance Number"
            )
    }
    
    modified <- raw |>
        group_by(team_number) |>
        summarize(
            total_cycles = 
                round(mean(
                    auto_lunites_high + 
                    auto_lunites_low + 
                    auto_lunites_missed + 
                    pre_high_lunites_scored +
                    pre_low_lunites_scored +
                    pre_lunites_missed +
                    pre_lunites_passed +
                    post_high_lunites_scored +
                    post_low_lunites_scored +
                    post_lunites_missed +
                    post_lunites_passed
                ), digits = 2),
            
            total_pts = 
                round(mean(
                    auto_lunites_high * 7 + 
                    auto_lunites_low * 4 + 
                    auto_lunites_missed * 0 + 
                    pre_high_lunites_scored * 5 +
                    pre_low_lunites_scored * 2 +
                    pre_lunites_missed * 0 +
                    pre_lunites_passed * 0 +
                    post_high_lunites_scored * 5 +
                    post_low_lunites_scored * 2 +
                    post_lunites_missed * 0 +
                    post_lunites_passed * 0
                ), digits = 2),
            
            taxid = paste(sum(moved),"/",n()),
            linked = paste(sum(end_position == "linked"),"/",n()),
            
        )
}

# Run the application 
shinyApp(ui = ui, server = server)
