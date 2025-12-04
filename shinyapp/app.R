library(shiny)
library(DT)
library(ggplot2)
library(plotly)
library(dplyr)
library(scales)
library(shinyWidgets)
library(tidyverse)
library(shinythemes)
library(ggbeeswarm)

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
                    DTOutput("match_table"),
                    DTOutput("match_comments")
                    
                )
            )
        ),
        tabPanel("Compare Teams",
            sidebarLayout(
                sidebarPanel(
                    pickerInput("teams_selected", "Select Team", choices = NULL, multiple = TRUE, options = list(maxOptions = 2)),
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

        tabPanel("Scouts",
                 fluidRow(
                     column(12,
                            plotOutput("scouts_num"),
                            plotOutput("yap_graph")
                     )
                 ),
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    teams <- read.csv("data_files/fake_teams.csv")
    raw <- read.csv("data_files/fake_data.csv")
    schedule <- read.csv("data_files/fake_schedule.csv")

    observe({
        updatePickerInput(session, "teams_selected", choices = sort(unique(raw$team_number)))
        updatePickerInput(session, "team_selected", choices = sort(unique(raw$team_number)))
    })
    output$event_summary <- renderPlot({
        all_pts(raw, unique(raw$team_number), TRUE)
    })
    
    output$event_table <- renderDT({
        datatable(
            create_modified(raw), 
            options = 
                list(
                    dom = "ft", 
                    lengthChange = FALSE, 
                    rowNames = FALSE, 
                    scrollX = TRUE, 
                    scrollY = 500, 
                    pageLength = nrow(create_modified(raw))))
    })
    
    output$score_prediction <- renderText({
        match <- input$selected_match 
        if (input$selected_mode == "Historical") {
            data <- raw[raw$match_number == match,] 
        } else {data <- raw}
        modified <- create_modified(data)
        red_alliance_score <- 
            modified[modified$team_number == schedule[match, 2],]$total_pts + 
            modified[modified$team_number == schedule[match, 3],]$total_pts
        blue_alliance_score <- 
            modified[modified$team_number == schedule[match, 4],]$total_pts + 
            modified[modified$team_number == schedule[match, 5],]$total_pts
        red_alliance_cycles <- 
            modified[modified$team_number == schedule[match, 2],]$total_cycles + 
            modified[modified$team_number == schedule[match, 3],]$total_cycles
        blue_alliance_cycles <- 
            modified[modified$team_number == schedule[match, 4],]$total_cycles + 
            modified[modified$team_number == schedule[match, 5],]$total_cycles
        paste0("Predicted Scores: ", 
               "<span style='color:red;'>", round(red_alliance_score, digits = 0), 
               "<span style='color:black;'>", " - ", 
               "<span style='color:blue;'>", round(blue_alliance_score, digits = 0),
               "<span style='color:black;'>", "\tPredicted Cycles: ", 
               "<span style='color:red;'>", round(red_alliance_cycles, digits = 0), 
               "<span style='color:black;'>", " - ", 
               "<span style='color:blue;'>", round(blue_alliance_cycles, digits = 0))
    })
    
    output$match_boxplot <- renderPlot({
        match <- input$selected_match 
        if (input$selected_mode == "Historical") {
            data <- raw[raw$match_number == match,] 
        } else {data <- raw}
        boxplot(data, schedule[match, 2:5])
    })
    
    output$match_all_pts <- renderPlot({
        match <- input$selected_match
        if (input$selected_mode == "Historical") {
            data <- raw[raw$match_number == match,] 
        } else {data <- raw}
        all_pts(data, schedule[match, 2:5], FALSE)
    })
    
    output$match_auto_pts <- renderPlot({
        match <- input$selected_match 
        if (input$selected_mode == "Historical") {
            data <- raw[raw$match_number == match,] 
        } else {data <- raw}
        auto_bar_graph(data, schedule[match, 2:5])
    })
    
    output$match_tele_cycles <- renderPlot({
        match <- input$selected_match
        if (input$selected_mode == "Historical") {
            data <- raw[raw$match_number == match,] 
        } else {data <- raw}
        tele_cycles_graph(data, schedule[match, 2:5])
    })
    
    output$match_tele_pts <- renderPlot({
        match <- input$selected_match
        if (input$selected_mode == "Historical") {
            data <- raw[raw$match_number == match,] 
        } else {data <- raw}
        tele_pts_graph(data, schedule[match, 2:5])
    })
    
    output$match_table <- renderDT({
        match <- input$selected_match
        if (input$selected_mode == "Historical") {
            data <- raw[raw$match_number == match,] 
        } else {data <- raw}
        limited_table <- filter(
            create_modified(data), 
            team_number %in% schedule[input$selected_match, 2:5])
        datatable(
            limited_table, 
            options = 
                list(
                    dom = "ft", 
                    lengthChange = FALSE, 
                    rowNames = FALSE, 
                    scrollX = TRUE, 
                    pageLength = nrow(create_modified(data))))
    })
    
    output$match_comments <- renderDT({
        match <- input$selected_match
        if (input$selected_mode == "Historical") {
            data <- raw[raw$match_number == match,] 
        } else {data <- raw}
        limited_table <- data |>
            filter(team_number %in% schedule[input$selected_match, 2:5]) |>
            rowwise() |>
            mutate(
                Team = team_number,
                Match = match_number,
                `General Comments` = 
                    paste(
                        if (!identical(grep("0", general_comments), integer(0))) {
                            "Played Defense"
                        },
                        if (!identical(grep("1", general_comments), integer(0))) {
                            "Inconsis. Shooting"
                        },
                        if (!identical(grep("2", general_comments), integer(0))) {
                            "Inconsis. Intake"
                        },
                        if (!identical(grep("3", general_comments), integer(0))) {
                            "CG Issues"
                        }, ""
                    ),
                `Specific Comments` = commentsOpen
            ) |>
            select(Team, Match, `General Comments`, `Specific Comments`)
        
        datatable(
            limited_table, 
            options = 
                list(
                    dom = "ft", 
                    lengthChange = FALSE, 
                    rowNames = FALSE, 
                    scrollX = TRUE, 
                    scrollY = 500, 
                    pageLength = nrow(limited_table)))
    })
    
    output$compare_boxplots <- renderPlot({
        teams <- input$teams_selected
        boxplot(raw, teams)
    })
    
    output$compare_all_pts <- renderPlot({
        teams <- input$teams_selected
        all_pts(raw, teams, FALSE)
    })
    
    output$match_hist <- renderPlot({
        teams <- input$teams_selected
        match_history(raw, teams)
    })
    
    output$compare_comments <- renderPlot({
        teams <- input$teams_selected
    })
    
    output$scouts_num<-renderPlot({
        create_scouts(raw)
    })
    
    output$yap_graph<-renderPlot({
        create_yap(raw)
    })
    
    all_pts <- function(raw, selection, flipped){
        data <- raw |>
            filter(team_number %in% selection) |>
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
            {if(flipped) coord_flip()} + 
            theme_bw()
    }
        
    
    
    #COMPARE TEAMS
    auto_bar_graph <- function(raw, selection) {
        data <- raw |>
            select(auto_lunites_high,team_number,auto_lunites_low,moved, auto_lunites_missed) |>
            filter(team_number %in% selection) |>
            group_by(team_number) |>
            summarize(
                auto_lunites_high=mean(auto_lunites_high),
                auto_lunites_low=mean(auto_lunites_low),
                auto_lunites_missed=mean(auto_lunites_missed),
                moved=mean(moved)
            )|>
            pivot_longer(cols=c(auto_lunites_high, auto_lunites_missed, auto_lunites_low,moved),
                         names_to="type",
                         values_to="points")
        
        ggplot(data, aes(x=factor(team_number),y=points, fill=type))+
            geom_bar(position="stack",stat="identity")+
            labs(title = paste("Auto Points for Team", data$team_number),
                 x = "Team", y = "Auto Points") +
            theme_bw()+
            scale_fill_manual(
                values = c(
                    "plum1", "plum3", "plum4", "#FFC156"
                ),
                labels = c(
                    "auto_lunites_low" = "Auto Low", "auto_lunites_high" = "Auto High",
                    "auto_lunites_missed" = "Auto Missed", "moved" = "Move"
                )) +
            labs(fill="Score")
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
            geom_bar(position="stack", stat="identity")+
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
                team_number = factor(team_number, levels = selection),
                alliance_color = factor(
                    ifelse(team_number %in% c(selection[3], selection[4]), "blue", "red"), 
                    levels = c("red", "blue")),
            ) |>
            select(match_number, team_number, total_score, alliance_color)
        ggplot(data, aes(x = total_score, y = team_number)) +
            geom_boxplot() +
            ggbeeswarm::geom_quasirandom(#created dots representing every match score
                shape = 21, color = "white", 
                alpha = 0.8, size = 3,
                aes(fill = alliance_color)
            )  + theme_bw() +
            labs(
                title ="Score per Match", 
                x = "Total Score",
                y = "Team Number", 
                fill = "Alliance Number"
            )
    }
    
    tele_cycles_graph <- function(raw, selected){
        data <- raw |>
            select(team_number, pre_high_lunites_scored,pre_low_lunites_scored,pre_lunites_missed,pre_lunites_passed,post_high_lunites_scored,post_low_lunites_scored,post_lunites_missed,post_lunites_passed) |>
            filter(team_number %in% selected) |>
            group_by(team_number)|>
            summarize(
                tp_preshut_highls = mean(pre_high_lunites_scored),
                tp_postshut_highls = mean(post_high_lunites_scored),
                tp_preshut_lowls = mean(pre_low_lunites_scored),
                tp_postshut_lowls = mean(post_low_lunites_scored),
                tp_preshut_lmiss = mean(pre_lunites_missed),
                tp_postshut_lmiss = mean(post_lunites_missed),
                tp_preshut_lpass = mean(pre_lunites_passed),
                tp_postshut_lpass = mean(post_lunites_passed),
            )|>
            pivot_longer(cols=c(tp_preshut_highls, tp_preshut_lowls, tp_preshut_lmiss, tp_preshut_lpass, tp_postshut_highls, tp_postshut_lowls, tp_postshut_lmiss, tp_postshut_lpass), names_to = "type", values_to = "points_score")
        ggplot(data, aes(x = factor(team_number), y = points_score, fill = type)) +
            geom_bar(position = "stack", stat = "identity") +
            labs(title = paste("Level Summary for Team", data$team_number), x = "Team", y = "Tele Cycles", fill = "Score")+
            scale_fill_manual(
                values = c(
                    "#3D5A80","#2b405c",
                    "#98C1D9","#80a5ba",
                    "#7DAA92","#517362",
                    "#026960","#003B36"
                ),
                labels = c(
                    "tp_preshut_highls" = "Preshutdown High",
                    "tp_preshut_lowls" = "Preshutdown Low", 
                    "tp_preshut_lmiss" = "Preshutdown Missed",
                    "tp_preshut_lpass" = "Preshutdown Passed",
                    "tp_postshut_highls" = "Postshutdown High",
                    "tp_postshut_lowls" = "Postshutdown Low", 
                    "tp_postshut_lmiss" = "Postshutdown Missed",
                    "tp_postshut_lpass" = "Postshutdown Passed"
                )
            ) +
            theme_bw()
    }
    
    tele_pts_graph <- function(raw, selected){
        data <- raw |>
            select(team_number, pre_high_lunites_scored,pre_low_lunites_scored,pre_lunites_missed,pre_lunites_passed,post_high_lunites_scored,post_low_lunites_scored,post_lunites_missed,post_lunites_passed) |>
            filter(team_number %in% selected) |>
            group_by(team_number)|>
            summarize(
                tp_preshut_highls = mean(pre_high_lunites_scored) * 5,
                tp_preshut_lowls = mean(pre_low_lunites_scored) * 2,
                tp_postshut_highls = mean(post_high_lunites_scored) * 5,
                tp_postshut_lowls = mean(post_low_lunites_scored) * 2,
            )|>
            pivot_longer(cols=c(tp_preshut_highls, tp_preshut_lowls, tp_postshut_highls, tp_postshut_lowls), names_to = "type", values_to = "points_score")
        ggplot(data, aes(x = factor(team_number), y = points_score, fill = type)) +
            geom_bar(position = "stack", stat = "identity") +
            labs(title = paste("Level Summary for Team", data$team_number), x = "Team", y = "Tele Points", fill = "Points")+
            scale_fill_manual(
                values=c("#3D5A80","#98C1D9", "#7DAA92", "#003B36"),
                labels = c("tp_preshut_highls" = "Pre-Shutdown High",
                           "tp_preshut_lowls" = "Pre-Shutdown Low",
                           "tp_postshut_highls" = "Post-Shutdown High",
                           "tp_postshut_lowls" = "Post-Shutdown Low")) +
            theme_bw()
    }
    
    match_history <- function(raw, selected) {
        data <- raw |>
            group_by(match_number) |>
            filter(team_number %in% selected) |>
            mutate(
                tele_passed = pre_lunites_passed + post_lunites_passed,
                tele_missed = pre_lunites_missed + post_lunites_missed,
                tele_high = pre_high_lunites_scored + post_high_lunites_scored,
                tele_low = pre_low_lunites_scored + post_low_lunites_scored,
                end = ifelse(end_position == "linked", 1, 0)
            ) |>
            pivot_longer(
                cols=c(auto_lunites_missed, auto_lunites_high, auto_lunites_low,
                       moved, tele_passed, tele_missed, tele_high, tele_low, end),
                names_to = "type", 
                values_to = "points_score")
        
        base_plot <- ggplot(data, aes(x = factor(match_number), y = points_score, fill = type)) + 
            geom_bar(position = "stack", stat = "identity") +
            scale_fill_manual(
                values = c(
                    "plum1", "plum3", "plum4", "#FFF68F", "#FFC156", "#3D5A80",
                    "#98C1D9", "#7DAA92", "#003B36"
                ),
                labels = c(
                    "auto_lunites_low" = "Auto Low", "auto_lunites_high" = "Auto High",
                    "auto_lunites_missed" = "Auto Missed", "tele_high" = "Tele High",
                    "tele_low" = "Tele Low", "tele_missed" = "Tele Missed",
                    "tele_passed" = "Tele Passed", "end" = "End", "moved" = "Move"
                )
            ) +
            labs(title = "Match History", x = "Matches", y = "Cycles") + 
            theme_bw()
        
        base_plot + facet_wrap(~ team_number, scales = "free")
    }
    
    create_modified <- function(raw) {
        modified <- raw |>
            group_by(team_number) |>
            summarize(
                total_cycles = 
                    round(mean(
                        auto_lunites_high + auto_lunites_low + 
                        auto_lunites_missed + pre_high_lunites_scored +
                        pre_low_lunites_scored + pre_lunites_missed +
                        pre_lunites_passed + post_high_lunites_scored +
                        post_low_lunites_scored + post_lunites_missed +
                        post_lunites_passed
                    ), digits = 2),
                
                total_pts = 
                    round(mean(
                        auto_lunites_high * 7 + auto_lunites_low * 4 + 
                        auto_lunites_missed * 0 + pre_high_lunites_scored * 5 +
                        pre_low_lunites_scored * 2 + pre_lunites_missed * 0 +
                        pre_lunites_passed * 0 + post_high_lunites_scored * 5 +
                        post_low_lunites_scored * 2 + post_lunites_missed * 0 +
                        post_lunites_passed * 0
                    ), digits = 2),
                
                taxid = paste(sum(moved),"/",n()),
                auto_cycles = 
                    round(mean(
                        auto_lunites_high + auto_lunites_low + 
                            auto_lunites_missed), digits = 2),
                tele_cycles = 
                    round(mean(
                        pre_high_lunites_scored + pre_low_lunites_scored + 
                        pre_lunites_missed + post_lunites_passed +
                        pre_lunites_passed + post_high_lunites_scored +
                        post_low_lunites_scored + post_lunites_missed
                    ), digits = 2),
                total_high = 
                    round(mean(
                        auto_lunites_high + pre_high_lunites_scored +
                        post_high_lunites_scored), digits = 2),
                total_low = 
                    round(mean(
                        auto_lunites_low + pre_low_lunites_scored +
                        post_low_lunites_scored), digits = 2),
                total_missed = 
                    round(mean(
                        auto_lunites_missed + pre_lunites_missed +
                        post_lunites_missed), digits = 2),
                total_passed = 
                    round(mean(
                        pre_lunites_passed + post_lunites_passed), digits = 2),
                #total_shutoff = 
                #    sum(
                #        unlist(lapply(
                #            c(which(team_number == schedule$R1), 
                #              which(team_number == schedule$R2), 
                #              which(team_number == schedule$B1), 
                ##              which(team_number == schedule$B2)),
                 #           determine_shutoff, 
                 #           raw = raw, 
                 #           team = team_number))),
                ground = paste(sum(ground_intake_auto), "/", n()),
                linked = paste(sum(end_position == "linked"),"/",n())
            )
    }
    
    determine_shutoff <- function(raw, team, match) {
        if (team %in% c(schedule[match, ]$R1, schedule[match, ]$R2)) {
            opponents = c(schedule[match, ]$B1, schedule[match, ]$B2)
        } else {
            opponents = c(schedule[match, ]$R1, schedule[match, ]$R2)
        }
        
        data <- raw[raw$match_number == match, ]
        opponents_post_score = 
            sum(
                data[data$team_number %in% opponents,]$post_high_lunites_scored + 
                data[data$team_number %in% opponents,]$post_low_lunites_scored + 
                data[data$team_number %in% opponents,]$post_lunites_missed + 
                data[data$team_number %in% opponents,]$post_lunites_passed)
        ifelse(opponents_post_score == 0, FALSE, TRUE)
    }
}

create_scouts <- function(raw) {
    raw <- raw |>
        select("scout_initials")
    data <- group_by(raw, scout_initials) |>
        count(scout_initials) |>
        arrange(desc(n))
    
    data$scout_initials <- factor(data$scout_initials, levels = data$scout_initials)
    
    p<-ggplot(data, aes(x = factor(scout_initials), y = n, fill = "navy"))+
        geom_histogram(position = "stack", stat = "identity") +
        labs(x = "Scout Initials", y = "Times", title = "Scout Summary") +
        scale_fill_manual(values="blue3") + 
        theme_bw()
    p + theme(legend.position = "none")
}

create_yap <- function(raw) {
    raw <-raw |>
        select("scout_initials", "commentsOpen") |>
        mutate(comment_count = nchar(commentsOpen)) |>
        group_by(scout_initials) |>
        summarize(characters = mean(comment_count)) |>
        arrange(desc(characters))
    
    raw$scout_initials<-factor(raw$scout_initials, levels=raw$scout_initials)
    
    p<-ggplot(raw, aes(x = factor(scout_initials), y = characters, fill = "navy")) +
        geom_histogram(position = "stack", stat = "identity") +
        labs(x = "Scout Initials", y="Characters",title = paste("Number of Yaps by Scout")) +
        scale_fill_manual(values = "navy") + 
        theme_bw()
    p + theme(legend.position = "none")
}

# Run the application 
shinyApp(ui = ui, server = server)
