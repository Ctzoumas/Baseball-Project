library(shiny)
library(shinydashboard)
library(DT)
library(dplyr)

# Assuming your data frames and model are already loaded:
# splitteamsbat, positionplayerdata, fullbatlm

ui <- dashboardPage(
  skin = "blue", # Change color theme (e.g., blue, black, purple, etc.)
  dashboardHeader(title = "Free Agency Data App"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Free Agency Tool Position Players", tabName = "main", icon = icon("dashboard")),
      menuItem("Find Similar Players & Contracts", tabName = "new_page", icon = icon("cogs"))
      
    )
  ),
  
  dashboardBody(
    tabItems(
      # Existing page (Position Players)
      tabItem(tabName = "main",
              fluidRow(
                # Filters for Team & Season
                box(
                  title = "Team & Season Filters", status = "primary", solidHeader = TRUE, width = 4,
                  selectInput("team", "Select Team:", choices = unique(splitteamsbat$Team)),
                  selectInput("team_season", "Select Season:", choices = unique(splitteamsbat$Season))
                ),
                # Filters for Player
                box(
                  title = "Player Filters", status = "primary", solidHeader = TRUE, width = 4,
                  selectInput("player", "Select Player:", choices = unique(positionplayerdata$Name)),
                  selectInput("player_season", "Select Season:", choices = unique(positionplayerdata$Season)),
                  selectInput("position", "Select Position:",
                              choices = c("cf", "3b", "lf", "rf", "1b", "c", "ss", "dh", "of", "2b", "inf"))
                ),
                # Predicted Contract
                box(
                  title = "Predicted Contract", status = "success", solidHeader = TRUE, width = 4,
                  DT::dataTableOutput("predictionTable")
                )
              ),
              fluidRow(
                # Player Statistics
                box(
                  title = "Player Statistics 2018-2024", status = "warning", solidHeader = TRUE, width = 12, 
                  DT::dataTableOutput("playerStatsTable")
                )
              ),
              fluidRow(
                # Team Statistics
                box(
                  title = "Team Statistics", status = "info", solidHeader = TRUE, width = 12,
                  DT::dataTableOutput("teamSeasonTable")
                )
              ),
      ),
      
      tabItem(tabName = "new_page",
              fluidRow(
                column(12, align = "center", h3("Coming soon"))
              )
              
      )
    )
  )
)

server <- function(input, output) {
  # Filter for Team & Season Data
  team_season_filtered <- reactive({
    req(input$team, input$team_season)
    filtered_data <- splitteamsbat %>% filter(Team == input$team, Season == input$team_season) %>% select(
      Name, Team, Season, G, PA, HR, R, RBI, SB, AVG, OBP, SLG, ISO, BABIP, wOBA, WAR, BsR, Def, Off, cWAR
    ) %>%
      mutate(
        AVG = format(round(AVG, 3), nsmall = 3),
        OBP = format(round(OBP, 3), nsmall = 3),
        SLG = format(round(SLG, 3), nsmall = 3),
        ISO = format(round(ISO, 3), nsmall = 3),
        BABIP = format(round(BABIP, 3), nsmall = 3),
        wOBA = format(round(wOBA, 3), nsmall = 3),
        WAR = format(round(WAR, 3), nsmall = 3),
        BsR = format(round(BsR, 3), nsmall = 3),
        Def = format(round(Def, 3), nsmall = 3),
        Off = format(round(Off, 3), nsmall = 3),
        cWAR = format(round(cWAR, 3), nsmall = 3)
      )
    if (nrow(filtered_data) == 0) {
      showNotification("No data found for the selected team and season.", type = "error")
      return(NULL)
    }
    filtered_data
  })
  
  # Player Data for Selected Player (not filtered by season)
  player_stats_filtered <- reactive({
    req(input$player)
    positionplayerdata %>% filter(Name == input$player) %>% select(
      Name, Team, Season, G, PA, HR, R, RBI, SB, AVG, OBP, SLG, ISO, BABIP, wOBA, WAR, BsR, Def, Off, cWAR
    ) %>%
      mutate(
        AVG = format(round(AVG, 3), nsmall = 3),
        OBP = format(round(OBP, 3), nsmall = 3),
        SLG = format(round(SLG, 3), nsmall = 3),
        ISO = format(round(ISO, 3), nsmall = 3),
        BABIP = format(round(BABIP, 3), nsmall = 3),
        wOBA = format(round(wOBA, 3), nsmall = 3),
        WAR = format(round(WAR, 3), nsmall = 3),
        BsR = format(round(BsR, 3), nsmall = 3),
        Def = format(round(Def, 3), nsmall = 3),
        Off = format(round(Off, 3), nsmall = 3),
        cWAR = format(round(cWAR, 3), nsmall = 3)
      )
  })
  
  # Player-Specific Prediction
  selected_player_data <- reactive({
    req(input$player, input$player_season, input$position)
    positionplayerdata %>%
      filter(Name == input$player, Season == input$player_season) %>%
      mutate(Pos = input$position)
  })
  
  prediction <- reactive({
    req(nrow(selected_player_data()) > 0)
    validate(
      need(all(c("Age", "WAR", "last2WAR", "cWAR", "Season", "Pos") %in% colnames(selected_player_data())),
           "Required columns are missing in the selected player data")
    )
    
    new_data <- selected_player_data() %>%
      select(Age, WAR, last2WAR, cWAR, Season, Pos)
    
    tryCatch({
      prediction_raw <- predict(fullbatlm, newdata = new_data, interval = "prediction")
      years <- round(prediction_raw[, 1])
      aav <- round(prediction_raw[, 2] / 500000) * 500000
      peakaav <- aav + 4000000
      total_value <- years * aav
      
      data.frame(
        Years = years,
        AAV = prettyNum(aav, big.mark = ",", scientific = FALSE),
        PeakAAV = prettyNum(peakaav, big.mark = ",", scientific = FALSE),
        TotalValue = prettyNum(total_value, big.mark = ",", scientific = FALSE)
      )
    }, error = function(e) {
      data.frame(Years = NA, AAV = NA, PeakAAV = NA, TotalValue = NA, Error = paste("Error:", e$message))
    })
  })
  
 
  # Render Team Season Data Table
  output$teamSeasonTable <- DT::renderDataTable({
    req(team_season_filtered())
    DT::datatable(team_season_filtered(), options = list(pageLength = 10, scrollX = TRUE))
  })
  
  # Render Prediction Table
  output$predictionTable <- DT::renderDataTable({
    DT::datatable(
      prediction(),
      options = list( 
        pageLength = 1,      # Set page length to 1 to display one row
        dom = "t",           # Only show the table, no extra controls like search or page length
        searching = FALSE,   # Remove the search bar
        paging = FALSE,      # Remove pagination if you only want to display one row
        columnDefs = list(
          list(targets = 0, visible = FALSE)  # Hide the first column (row number)
        )
      )
    ) 
  })
  
  # Render Player Stats Table
  output$playerStatsTable <- DT::renderDataTable({
    req(player_stats_filtered())
    DT::datatable(player_stats_filtered(), options = list(pageLength = 10, scrollX = TRUE))
  })
  
 

}

shinyApp(ui = ui, server = server)
