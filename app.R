library(ggtext)
library(tidyverse)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinycssloaders) 
library(DT)
library(DBI)
library(RPostgres)
library(RPostgreSQL)
library(dbplyr)
library(lubridate)
library(plotly)
library(RSQLite)
library(odbc)   
library(pool) 
library(tibbletime)
library(ggiraph)
library(ggplot2)
library(scales)
library(quantmod)
library(reactable)

# the following are the tidyverse packages used in mcoo
# library(purrr)
# library(stringr)
# library(tidyr)
# library(dplyr)


source('splash_colors_functions_prod.R')

mac_simulation <- read_csv("mac_simulation.csv") #%>%
  # mutate(allMACPct = ifelse(season == 2016, allMACPct/100, allMACPct))
mac_simulation_team <- read_csv("mac_simulation_team.csv")
regional_simulation <- read_csv("regional_simulation.csv")
regional_simulation_team <- read_csv("regional_team_simulation.csv")
nationals_simulation <- read_csv("nationals_simulation.csv")
nationals_simulation_team <- read_csv("nationals_simulation_team.csv")
runner_data <- read_csv("app_data.csv") %>%
  mutate(final_adjusted_time = ifelse(distance>=4, final_adjusted_avg_mile*4.974684, final_adjusted_avg_mile*3.729508))

rating_column <- function(maxWidth = 100, ...) {
  colDef(maxWidth = maxWidth, align = "center", class = "cell number", ...)
}

make_color_pal <- function(colors, bias = 1) {
  get_color <- colorRamp(colors, bias = bias)
  function(x) rgb(get_color(x), maxColorValue = 255)
}

# UI ------
ui <- fluidPage(theme = "splash_theme.css",
                #favicon of splash logo
                # tags$head(tags$link(rel="shortcut icon", href="splash-fav-32.ico")),
                dashboardPage(title = "NCAA XC Simulator",
                              dashboardHeader(
                              ),
                              dashboardSidebar( 
                                sidebarMenu(id = 'sidebar_menu',
                                            menuItem('Meet Simulator', tabName = "ncaa_xc_tab", selected = T)
                                            
                                )
                              ),
                              dashboardBody(  
                                tabItems(
                                  tabItem(
                                    tabName = "ncaa_xc_tab",
                                    fluidRow(
                                      column(
                                        width = 12,
                                        tags$div(
                                          class = "ggplot_box",
                                          h3(strong('Adjusted results are set to 8k for men and 6k for women')),
                                      )
                                      )
                                      
                                    ),
                                    fluidRow(
                                      column(
                                        width = 1
                                      ),
                                      column(
                                        width = 2,
                                        selectInput("select_gender", "Select Gender", c("Male", "Female"), selected = "Male")
                                      ),
                                      column(
                                        width = 2
                                      ),
                                      column(
                                        width = 2,
                                        selectInput("select_year","Select Year", c("2021", "2019", "2018", "2017", "2016", "2015"), selected = "2021")
                                      ),
                                      column(
                                        width = 2
                                      ),
                                      column(
                                        width = 2,
                                        selectInput("select_race", "Select Race", c("MAC Championships", "NCAA Great Lakes Regional", "NCAA Nationals"), selected = "NCAA Great Lakes Regional")
                                      ),
                                      column(
                                        width = 1
                                      )
                                    ),
                                    fluidRow(
                                      column(
                                        width = 7,
                                        tags$div(
                                          class = "ggplot_box",
                                          h1("Individual Results"),
                                          reactableOutput("individual_results_rt")   %>% shinycssloaders::withSpinner(type = 8, color = getOption("spinner.color", default = "#B4C5E4")
                                          )
                                        )
                                      ),
                                      column(
                                        width = 5,
                                        tags$div(
                                          class = "ggplot_box",
                                          h1("Team Results"),
                                          reactableOutput("team_results_rt")   %>% shinycssloaders::withSpinner(type = 8, color = getOption("spinner.color", default = "#B4C5E4")
                                          )                                                                     
                                        )
                                      )
                                    ),
                                    fluidRow(
                                      column(
                                        width = 12,
                                        tags$div(
                                          class = "ggplot_box",
                                          h3(strong('Process')),
                                          p(strong('1. Scrape All 2012-2021 XC Meet Data from TFRRS:'),
                                            " the scrape was done using a selenium bot in R and gathered a total of 6,550 meets and over 1.3 million individual performances", br(),
                                            strong("2. Data Processing and Cleansing: "),
                                            "clean the names of runners, meets, locations, etc. to prepare for analysis", br(),
                                            strong("3. Create Adjustment Values using Linear Regression: "),
                                            "a basic linear regression model, with the average pace per mile of athletes as the Y variable, was used to create 3 adjustment values for each race (course, race, and distance). 
                                            The course adjustment accounts for how challenging a course is or isn't (hills, turns, etc.). The race adjustment looks specifically at the race an athlete was 
                                            in and accounts for both weather and race tactics, because in large meets, 2 different races might have 2 completely different time outcomes based on how the strategy of the race
                                            plays out early on. For example, one race might have the leaders go out very aggressively from the gun, which would pull the field along, but
                                            sit and kick races (leaders go out slow and finish hard) will result in slower times across the field. The distance adjustment accounts for longer or shorter races that will naturally result in
                                             slower or faster times.", br(),
                                            strong("4. Create Adjusted Times and Create Simulation Data: "),
                                            "use the values above to create adjusted times for athletes and evaluate each athlete based on the average of their times 
                                            as well as their level of risk (standard deviation). This helps in the simulation to properly account for runners that
                                            are not consistent and have boom/bust potential in races.", br(),
                                            strong("5. Run 1000 Simulations on each Meet and Create Predictions: "), "this final step is what creates the data 
                                            shown below"
                                          ),
                                          br(), h3(strong("Adjustments")),
                                          p(strong('1. Exclude "Blow-Up" Races: '),
                                            "it can be pretty common for certain runners to have a race every now and then or once a season 
                                            that would qualify as a blow-up race and a statistical outlier. These are races where the athlete might have tweaked an injury, 
                                            not had enough sleep, or something went wrong that resulted in a time significantly slower than their season average. These races would not qualify as 
                                            just a bad race because a bad race is one that is slightly slower than the  
                                            normal performance while the blow up was a race that they should not have finished but did for the sake of pride. When I did not 
                                            remove these races, it punished those runners and skewed the results of the simulations to be far less accurate due to the one
                                             race skewing their average. This rule could also be named after myself due to my 2017 Pre-Nationals Performance in case anybody 
                                            wants to see a direct example.", br(),
                                            strong('2. The "Didn\'t Try at Regionals" Rule: '),
                                            " early on in the testing phase, the biggest issue that I came across was very inaccurate Nationals rankings that seemed to
                                            consistently undervalue the heavy favorites for both individuals and teams. After doing some research, I discovered that the undervalued favorites were
                                             typically caused by their performances at regionals. This phenomenon makes a lot of sense when you consider the nature of Division 1 XC. 
                                            During the season, teams typically run 8k races and have 2 weeks to recover between them. With regionals and nationals, however, 
                                            that distance gets bumped up to 10k and the 14 days of recovery becomes only 8. To keep their legs rested and to be at their best at 
                                            nationals, it is extremely common for the best teams and athletes to run at a tempo or sub max effort and only try hard enough to punch their tickets to 
                                            the national meet. In the data, this shows up as simply a bad race and not slow enough to be considered a blow up, resulting in skewed predictions. The same 
                                            thing also happened in the women's races, just not as often or as bad as it did in the men's. To correct this, regional races are excluded from the national's simulation 
                                            IF they are more than 1% slower than the average adjusted pace per mile of the season. Doing so accounts for runners who did not try at regionals 
                                            while also not punishing runners who ran extremely well."),
                                          br(), h3(strong("Model Limitations")),
                                          p(strong("1. Coaches/Teams that Peak Well: "), "the model consistently under ranks teams and athletes from programs that are great at peaking 
                                            their athletes for the post season. These are teams that consistently produce their best results of the season at nationals or at the conference meet. 
                                            The best example of this is how the model almost always under ranks Northern Arizona (2016-2018 champions), who would typically run well during the season,
                                             but have their best race by far at nationals. This is an issue that I'm looking to fix with an adjustment score based on the coach/program.", br(),
                                            strong('2. Top Runners That Do Not Run "Fast Enough" in the Build Up to Nationals: '),
                                            "there are a few years of the nationals simulation that undervalue the heavy favorites like Justyn Knight (2017) and Morgan Mcdonald (2018). 
                                            This undervaluation is caused by their times not being fast enough to separate them from other athletes that were not in the same races. Both 
                                            Justyn Knight and Morgan Mcdonald won their regular season races but ran the races to win and not to break course records. Even though they could faster than they did, it is the smart play to save the hardest efforts for when it counts. Because they 
                                            did this, they end up being undervalued in the final simulation. An example of a favorite that did race for course records during the season 
                                            and was not undervalued would be Weini Kelati in 2019, who routinely won her races by 30+ seconds.", br(),
                                            strong("3. Nationals Athletes that don't compete at Large Meets Prior: "),
                                            "the model overvalues certain athletes that ran fast times throughout the regular season but did not compete against elite competition or at large meets. The best example
                                            would be the Wildschutt brothers from Coastal Carolina in 2019. They both deserved to be in the elite tier because of how 
                                            fast they ran but ended up being over ranked because they did not compete against elite competition until 
                                            Regionals. Their fast times also skewed the model during that year, resulting in several overvalued athletes from the southeast regional.", br(),
                                            strong("4. Eastern Michigan at the 2018 MAC Championships: "), "in 2018, Eastern Michigan is missing from the MAC championship simulation because they did 
                                            not go to a single meet during the regular season that showed up on TFRRS. After doing some research, they did run 2 8k's prior to MAC's, but one of them 
                                            was against Michigan community college teams and the other was against Canadian college teams. Because these were such low tier meets, especially compared 
                                            to their normal schedule, they did not have any results on TFRRS for me to include in the analysis.")
                                        ),
                                      )
                                      
                                    )
                                  )
                                ) # End Tab Items
                              ) #end Dashboard Body
                ) # end DashboardPage
) # end FluidPage (for theme css)

# Server ------------
server <- function(input, output, session) {
  
  
  #### Tab 7: NCAA XC ####
  
  #### Individual ####
  individual_results_rdf <- reactive({
    if(input$select_race == "MAC Championships") {
      df <- mac_simulation %>%
        filter(season == input$select_year,
               gender == input$select_gender) %>%
        group_by() %>%
        mutate(rank = rank(avgPlace),
               winPct = winPct/100,
               firstTeamPct = firstTeamPct/100,
               allMACPct = allMACPct/100) %>%
        ungroup() %>%
        select(rank, NAME, TEAM, avgPlace, maxPlace, minPlace, allMACPct, firstTeamPct, winPct, season, gender)
        
    } else {
      if(input$select_race == "NCAA Great Lakes Regional") {
        df <- regional_simulation %>%
          filter(season == input$select_year,
                 gender == input$select_gender) %>%
          group_by() %>%
          mutate(rank = rank(avgPlace),
                 winPct = winPct/100,
                 all_regionPct = all_regionPct/100,
                 auto_qualifierPct = auto_qualifierPct/100) %>%
          ungroup() %>%
          select(rank, NAME, TEAM, avgPlace, maxPlace, minPlace, all_regionPct, auto_qualifierPct, winPct, season, gender)
      } else {
        df <- nationals_simulation %>%
          filter(season == input$select_year,
                 gender == input$select_gender) %>%
          group_by() %>%
          mutate(rank = rank(avgPlace),
                 winPct = winPct/100,
                 allAmericanPct = allAmericanPct/100) %>%
          ungroup() %>%
          select(rank, NAME, TEAM, avgPlace, maxPlace, minPlace, allAmericanPct, winPct, season, gender) 
      }
    }
    
    df <- df %>%
      arrange(rank)

  })
  
  runner_results_rdf <- reactive({
    selection_df <- individual_results_rdf() %>%
      select(NAME, TEAM, season)
    
    runner_df <- runner_data %>%
      inner_join(selection_df %>%
                   select(NAME, TEAM, season), by = c("NAME", "TEAM", "season")) %>%
      mutate(first_part = floor(final_adjusted_time/60),
             second_part = as.character(round(final_adjusted_time-(60*first_part))),
             second_part = ifelse(nchar(second_part) > 1, second_part, paste0("0", second_part)),
             final_adjusted_time = paste0(first_part, ":", second_part)) %>%
      select(-first_part, -second_part) %>%
      mutate(first_part = floor(TIME/60),
             second_part = as.character(round(TIME-(60*first_part))),
             second_part = ifelse(nchar(second_part) > 1, second_part, paste0("0", second_part)),
             TIME = paste0(first_part, ":", second_part)) %>%
      select(-first_part, -second_part) %>%
      select(NAME, TEAM, DATE, MEET, race_length, PL, TIME, final_adjusted_time) %>%
      arrange(desc(DATE))
      
    runner_df
  })
  
  output$individual_results_rt <- renderReactable({
    df <- individual_results_rdf()
    
    runner_df <- runner_results_rdf()
    
    knockout_pct_color <- make_color_pal(c("#ffffff", "#F4D35E"))
    
    format_pct <- function(value) {
      if (value < 0.01) " <1%"
      else if (value > 0.99) ">99%"
      else formatC(paste0(round(value * 100), "%"), width = 4)
    }
    
    award_column <- function(minWidth = 70, class = NULL, ...) {
      colDef(
        align = "center",
        # format = colFormat(
        #   percent = TRUE,
        #   digits = 1
        # ),
        cell = format_pct,
        minWidth = minWidth,
        class = paste("cell number", class),
        style = function(value) {
          # Lighter color for <1%
          if (value < 0.001) {
            list(color = "#aaa")
          } else {
            list(color = "#111", background = knockout_pct_color(value))
          }
        },
        ...
      )
    }
    
    if(input$select_race == "MAC Championships") {
      table <- reactable(
        df,
        pagination = FALSE,
        striped = TRUE,
        filterable = TRUE,
        defaultColGroup = colGroup(headerClass = "group-header"),
        style = list(fontSize = "12px"),
        defaultColDef = colDef(headerClass = "header"),
        columnGroups = list(
          colGroup(name = "Runner Info", columns = c("rank", "NAME", "TEAM")),
          colGroup(name = "Placing", columns = c("avgPlace", "maxPlace", "minPlace")),
          colGroup(name = "Awards", columns = c("winPct", "firstTeamPct", "allMACPct"))
        ),
        columns = list(
          rank = colDef(
            name = "Rank",
            align = "left",
            minWidth = 45
          ),
          NAME = colDef(
            name = "Name",
            align = "left",
            minWidth = 130
          ),
          TEAM = colDef(
            name = "Team",
            align = "left",
            minWidth = 100
          ),
          avgPlace = colDef(
            name = "Avg",
            align = "center",
            minWidth = 50,
            format = colFormat(
              digits = 1
            ),
            class = "border-left"
          ),
          maxPlace = colDef(
            name = "Max",
            align = "center",
            minWidth = 40
          ),
          minPlace = colDef(
            name = "Min",
            align = "center",
            minWidth = 40
          ),
          allMACPct = award_column(
            name = "All-MAC",
            class = "border-left"
          ),
          firstTeamPct = award_column(
            name = "1st Team"
          ),
          winPct = award_column(
            name = "Win"
          ),
          season = colDef(
            show = FALSE
          ),
          gender = colDef(
            show = FALSE
          )
        ),
        details = function(index) {
          runner_data <- runner_df[runner_df$NAME == df$NAME[index], ]
          htmltools::div(style = "padding: 16px",
                         reactable(runner_data,
                                   outlined = TRUE,
                                   pagination = FALSE,
                                   striped = TRUE,
                                   columns = list(
                                     NAME = colDef(
                                       show = FALSE
                                     ),
                                     TEAM = colDef(
                                       show = FALSE
                                     ),
                                     DATE = colDef(
                                       name = "Date",
                                       align = "center"
                                     ),
                                     MEET = colDef(
                                       name = "Meet",
                                       align = "left"
                                     ),
                                     race_length = colDef(
                                       name = "Distance",
                                       align = "center"
                                     ),
                                     PL = colDef(
                                       name = "Place",
                                       align = "center"
                                     ),
                                     TIME = colDef(
                                       name = "Time",
                                       align = "center"
                                     ),
                                     final_adjusted_time = colDef(
                                       name = "Adj. Time",
                                       align = "center"
                                     )
                                   ))
          )
        }
      )

    } else {
      if(input$select_race == "NCAA Great Lakes Regional") {
        table <- reactable(
          df,
          pagination = FALSE,
          striped = TRUE,
          filterable = TRUE,
          style = list(fontSize = "12px"),
          defaultColGroup = colGroup(headerClass = "group-header"),
          defaultColDef = colDef(headerClass = "header"),
          columnGroups = list(
            colGroup(name = "Runner Info", columns = c("rank", "NAME", "TEAM")),
            colGroup(name = "Placing", columns = c("avgPlace", "maxPlace", "minPlace")),
            colGroup(name = "Awards", columns = c("winPct", "all_regionPct", "auto_qualifierPct"))
          ),
          columns = list(
            rank = colDef(
              name = "Rank",
              align = "left",
              minWidth = 45
            ),
            NAME = colDef(
              name = "Name",
              align = "left",
              minWidth = 130
            ),
            TEAM = colDef(
              name = "Team",
              align = "left",
              minWidth = 100
            ),
            avgPlace = colDef(
              name = "Avg",
              align = "center",
              # maxWidth = 45,
              minWidth = 50,
              format = colFormat(
                digits = 1
              ),
              class = "border-left"
            ),
            maxPlace = colDef(
              name = "Max",
              align = "center",
              # maxWidth = 45,
              minWidth = 40
            ),
            minPlace = colDef(
              name = "Min",
              align = "center",
              # maxWidth = 45,
              minWidth = 40
            ),
            all_regionPct = award_column(
              name = "All-Region",
              class = "border-left"
            ),
            auto_qualifierPct = award_column(
              name = "Auto-Qualify"
            ),
            winPct = award_column(
              name = "Win"
            ),
            season = colDef(
              show = FALSE
            ),
            gender = colDef(
              show = FALSE
            )
          ),
          details = function(index) {
            runner_data <- runner_df[runner_df$NAME == df$NAME[index], ]
            htmltools::div(style = "padding: 16px",
                           reactable(runner_data,
                                     outlined = TRUE,
                                     pagination = FALSE,
                                     striped = TRUE,
                                     columns = list(
                                       NAME = colDef(
                                         show = FALSE
                                       ),
                                       TEAM = colDef(
                                         show = FALSE
                                       ),
                                       DATE = colDef(
                                         name = "Date",
                                         align = "center"
                                       ),
                                       MEET = colDef(
                                         name = "Meet",
                                         align = "left"
                                       ),
                                       race_length = colDef(
                                         name = "Distance",
                                         align = "center"
                                       ),
                                       PL = colDef(
                                         name = "Place",
                                         align = "center"
                                       ),
                                       TIME = colDef(
                                         name = "Time",
                                         align = "center"
                                       ),
                                       final_adjusted_time = colDef(
                                         name = "Adj. Time",
                                         align = "center"
                                       )
                                     ))
            )
          }
        )
      } else {
        table <- reactable(
          df,
          pagination = FALSE,
          striped = TRUE,
          filterable = TRUE,
          style = list(fontSize = "12px"),
          defaultColGroup = colGroup(headerClass = "group-header"),
          defaultColDef = colDef(headerClass = "header"),
          columnGroups = list(
            colGroup(name = "Runner Info", columns = c("rank", "NAME", "TEAM")),
            colGroup(name = "Placing", columns = c("avgPlace", "maxPlace", "minPlace")),
            colGroup(name = "Awards", columns = c("winPct", "allAmericanPct"))
          ),
          columns = list(
            rank = colDef(
              name = "Rank",
              align = "left",
              minWidth = 40,
            ),
            NAME = colDef(
              name = "Name",
              align = "left",
              minWidth = 130
            ),
            TEAM = colDef(
              name = "Team",
              align = "left",
              # maxWidth = 120,
              minWidth = 100
            ),
            avgPlace = colDef(
              name = "Avg",
              align = "center",
              # maxWidth = 45,
              minWidth = 45,
              format = colFormat(
                digits = 1
              ),
              class = "border-left"
            ),
            maxPlace = colDef(
              name = "Max",
              align = "center",
              # maxWidth = 45,
              minWidth = 40
            ),
            minPlace = colDef(
              name = "Min",
              align = "center",
              # maxWidth = 45,
              minWidth = 40
            ),
            allAmericanPct = award_column(
              name = "All-American",
              class = "border-left"
            ),
            winPct = award_column(
              name = "Win"
            ),
            season = colDef(
              show = FALSE
            ),
            gender = colDef(
              show = FALSE
            )
          ),
          details = function(index) {
            runner_data <- runner_df[runner_df$NAME == df$NAME[index], ]
            htmltools::div(style = "padding: 16px",
                           reactable(runner_data,
                                     outlined = TRUE,
                                     pagination = FALSE,
                                     striped = TRUE,
                                     columns = list(
                                       NAME = colDef(
                                         show = FALSE
                                       ),
                                       TEAM = colDef(
                                         show = FALSE
                                       ),
                                       DATE = colDef(
                                         name = "Date",
                                         align = "center"
                                       ),
                                       MEET = colDef(
                                         name = "Meet",
                                         align = "left"
                                       ),
                                       race_length = colDef(
                                         name = "Distance",
                                         align = "center"
                                       ),
                                       PL = colDef(
                                         name = "Place",
                                         align = "center"
                                       ),
                                       TIME = colDef(
                                         name = "Time",
                                         align = "center"
                                       ),
                                       final_adjusted_time = colDef(
                                         name = "Adj. Time",
                                         align = "center"
                                       )
                                     ))
            )
          }
        )
      }
    }

   table
  })
  
  #### TEAM ####
  team_results_rdf <- reactive({
    if(input$select_race == "MAC Championships") {
      df <- mac_simulation_team %>%
        filter(season == input$select_year,
               gender == input$select_gender) %>%
        group_by() %>%
        mutate(rank = rank(avgPoints),
               pctWin = pctWin/100,
               pctPodium = pctPodium/100) %>%
        ungroup() %>%
        select(rank, TEAM, avgPoints, avgPlace, maxPlace, minPlace, pctPodium, pctWin)
      
    } else {
      if(input$select_race == "NCAA Great Lakes Regional") {
        df <- regional_simulation_team %>%
          filter(season == input$select_year,
                 gender == input$select_gender) %>%
          group_by() %>%
          group_by() %>%
          mutate(rank = rank(avgPoints),
                 pct_win = pct_win/100,
                 pct_auto_qual = pct_auto_qual/100) %>%
          ungroup() %>%
          select(rank, TEAM, avgPoints, avgPlace, maxPlace, minPlace, pct_auto_qual, pct_win)
      } else {
        df <- nationals_simulation_team %>%
          filter(season == input$select_year,
                 gender == input$select_gender) %>%
          group_by() %>%
          group_by() %>%
          mutate(rank = rank(avgPoints),
                 pctWin = pctWin/100,
                 pctPodium = pctPodium/100) %>%
          ungroup() %>%
          select(rank, TEAM, avgPoints, avgPlace, maxPlace, minPlace, pctPodium, pctWin)
      }
    }
    
    df <- df %>%
      arrange(rank)
    
  })
  
  output$team_results_rt <- renderReactable({
    df <- team_results_rdf()
    
    knockout_pct_color <- make_color_pal(c("#ffffff", "#F4D35E"))
    
    format_pct <- function(value) {
      if (value < 0.01) " <1%"
      else if (value > 0.99) ">99%"
      else formatC(paste0(round(value * 100), "%"), width = 4)
    }
    
    award_column <- function(minWidth = 65, class = NULL, ...) {
      colDef(
        align = "center",
        # format = colFormat(
        #   percent = TRUE,
        #   digits = 1
        # ),
        minWidth = minWidth,
        cell = format_pct,
        class = paste("cell number", class),
        style = function(value) {
          # Lighter color for <1%
          if (value < 0.001) {
            list(color = "#aaa")
          } else {
            list(color = "#111", background = knockout_pct_color(value))
          }
        },
        ...
      )
    }
    
    if(input$select_race == "MAC Championships") {
      table <- reactable(
        df,
        pagination = FALSE,
        striped = TRUE,
        filterable = TRUE,
        defaultColGroup = colGroup(headerClass = "group-header"),
        defaultColDef = colDef(headerClass = "header"),
        columnGroups = list(
          colGroup(name = "Team", columns = c("rank", "TEAM")),
          colGroup(name = "Placing", columns = c("avgPoints","avgPlace", "maxPlace", "minPlace")),
          colGroup(name = "Awards", columns = c("pctWin", "pctPodium"))
        ),
        columns = list(
          rank = colDef(
            name = "Rank",
            align = "left",
            minWidth = 50,
          ),
          # NAME = colDef(
          #   name = "Name",
          #   align = "left",
          #   minWidth = 140
          # ),
          TEAM = colDef(
            name = "Team",
            align = "left",
            minWidth = 100
          ),
          avgPoints = colDef(
            name = "Points",
            align = "center",
            minWidth = 60,
            format = colFormat(
              digits = 1
            ),
            class = "border-left"
          ),
          avgPlace = colDef(
            name = "Avg",
            align = "center",
            minWidth = 40,
            format = colFormat(
              digits = 1
            ),
          ),
          maxPlace = colDef(
            name = "Max",
            align = "center",
            minWidth = 45
          ),
          minPlace = colDef(
            name = "Min",
            align = "center",
            minWidth = 40
          ),
          pctPodium = award_column(
            name = "Podium",
            class = "border-left"
          ),
          pctWin = award_column(
            name = "Win"
          )
        )
      )
      
    } else {
      if(input$select_race == "NCAA Great Lakes Regional") {
        table <- reactable(
          df,
          pagination = FALSE,
          striped = TRUE,
          filterable = TRUE,
          defaultColGroup = colGroup(headerClass = "group-header"),
          defaultColDef = colDef(headerClass = "header"),
          columnGroups = list(
            colGroup(name = "Team", columns = c("rank", "TEAM")),
            colGroup(name = "Placing", columns = c("avgPoints","avgPlace", "maxPlace", "minPlace")),
            colGroup(name = "Awards", columns = c("pct_win", "pct_auto_qual"))
          ),
          columns = list(
            rank = colDef(
              name = "Rank",
              align = "left",
              minWidth = 50
            ),
            # NAME = colDef(
            #   name = "Name",
            #   align = "left",
            #   minWidth = 100
            # ),
            TEAM = colDef(
              name = "Team",
              align = "left",
              minWidth = 100
            ),
            avgPoints = colDef(
              name = "Points",
              align = "center",
              minWidth = 60,
              format = colFormat(
                digits = 1
              ),
              class = "border-left"
            ),
            avgPlace = colDef(
              name = "Avg",
              align = "center",
              minWidth = 45,
              format = colFormat(
                digits = 1
              ),
            ),
            maxPlace = colDef(
              name = "Max",
              align = "center",
              minWidth = 42
            ),
            minPlace = colDef(
              name = "Min",
              align = "center",
              minWidth = 40
            ),
            pct_auto_qual = award_column(
              name = "Auto-Qualify",
              class = "border-left"
            ),
            pct_win = award_column(
              name = "Win"
            )
          )
        )
      } else {
        table <- reactable(
          df,
          pagination = FALSE,
          striped = TRUE,
          filterable = TRUE,
          defaultColGroup = colGroup(headerClass = "group-header"),
          defaultColDef = colDef(headerClass = "header"),
          style = list(fontSize = "12px"),
          columnGroups = list(
            colGroup(name = "Team", columns = c("rank", "TEAM")),
            colGroup(name = "Placing", columns = c("avgPoints","avgPlace", "maxPlace", "minPlace")),
            colGroup(name = "Awards", columns = c("pctWin", "pctPodium"))
          ),
          columns = list(
            rank = colDef(
              name = "Rank",
              align = "left",
              minWidth = 40
            ),
            # NAME = colDef(
            #   name = "Name",
            #   align = "left",
            #   minWidth = 150,
            #   maxWidth = 150
            # ),
            TEAM = colDef(
              name = "Team",
              align = "left",
              minWidth = 100
            ),
            avgPoints = colDef(
              name = "Points",
              align = "center",
              minWidth = 60,
              format = colFormat(
                digits = 1
              ),
              class = "border-left"
            ),
            avgPlace = colDef(
              name = "Avg",
              align = "center",
              minWidth = 45,
              format = colFormat(
                digits = 1
              ),
            ),
            maxPlace = colDef(
              name = "Max",
              align = "center",
              minWidth = 42
            ),
            minPlace = colDef(
              name = "Min",
              align = "center",
              minWidth = 42
            ),
            pctPodium = award_column(
              name = "Podium",
              class = "border-left"
            ),
            pctWin = award_column(
              name = "Win"
            )
          )
        )
      }
    }
    
    table
  })
  
}

shinyApp(ui = ui, server = server)
