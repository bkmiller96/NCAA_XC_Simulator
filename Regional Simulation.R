library(tidyverse)

#### Regionals 2015 ####
mac_df <- mens_data_final_2 %>%
  inner_join(
    mens_data_final_2 %>%
      mutate(DATE = as.Date(DATE, "%m/%d/%y")) %>%
      mutate(season = year(DATE),
             month = month(DATE),
             day = day(DATE)) %>%
      filter(MEET == "NCAA Division I Great Lakes Region Cross Country Championships",
             year(DATE) == 2015) %>%
      select(NAME, TEAM), by = c("NAME", "TEAM")
  ) %>%
  filter(season == "2015") %>%
  # filter(TEAM %in% c("Ohio", "Eastern Michigan", "Central Michigan", "Toledo", "Kent State",
  #                    "Bowling Green", "Miami (Ohio)", "Akron", "Michigan", "Notre Dame", "Wisconsin", "Michigan State",
  #                    "Ohio State", "Indiana", "Purdue", "Butler", "Indiana State", "Dayton", "IUPUI", "Marquette",
  #                    "Youngstown St.", "Oakland", "Cincinnati", "Wis.-Milwaukee", "Detroit", "Xavier (Ohio)",
  #                    "Wright State", "Wis.-Green Bay", "Western Michigan", "Ball State", "Cleveland St."),
  #        # !MEET %in% c("Mel Brodt XC Invitational", "Indiana Intercollegiate Championships", "Summit League Championships"),
  #        season == "2015") %>%
  filter(DATE <= as.Date("2015-11-07")) %>%
  na.omit() %>%
  mutate(adj_time = final_adjusted_avg_mile*4.97) %>%
  arrange(desc(DATE)) %>%
  group_by(NAME, TEAM, season) %>%
  mutate(adjusted_avg = mean(adj_time),
         pct_to_avg = (adjusted_avg-adj_time)/adj_time) %>%
  filter(pct_to_avg >= -.017) %>%
  summarise(adjusted_avg = mean(adj_time),
            last_race_adj_time = first(adj_time),
            st_dev = sd(adj_time),
            fastest = min(adj_time)) %>%
  ungroup() %>%
  mutate(st_dev = ifelse(is.na(st_dev), mean(st_dev, na.rm = TRUE), st_dev),
         st_dev = st_dev*.6,
         adjusted_avg = (adjusted_avg+last_race_adj_time)/2) %>%
  select(NAME, TEAM, season, adjusted_avg, st_dev, fastest) %>%
  arrange(adjusted_avg)

PersonAnalysisFinal3 <- mac_df %>%
  # mutate(points = cumsum(count)) %>%
  # filter(placeOnTeam <= 5) %>%
  # group_by(TEAM) %>%mac_df %>%
  group_by(TEAM) %>%
  mutate(count = 1,
         placeOnTeam = cumsum(count)) %>%
  ungroup() %>%
  filter(placeOnTeam <= 7) %>%
  mutate(countOverall = ifelse(placeOnTeam <= 7, 1, 0),
         points = cumsum(countOverall),
         points = ifelse(countOverall == 0, NA, points),
         place = cumsum(count),
         fastestTimeRank = rank(fastest))
# summarise(points = sum(points))

simulationBase <- PersonAnalysisFinal3 %>%
  rename(avg_adj_time = adjusted_avg) %>%
  select(NAME, TEAM, avg_adj_time, place, points) %>%
  mutate(simNumber = 1)

for(i in 2:1000){
  print(i)
  
  simulation <- mac_df %>%
    rowwise() %>%
    mutate(adjusted_avg = rnorm(1, adjusted_avg, st_dev)) %>%
    ungroup() %>%
    arrange(adjusted_avg) %>%
    group_by(TEAM) %>%
    mutate(count = 1,
           placeOnTeam = cumsum(count)) %>%
    ungroup() %>%
    filter(placeOnTeam <= 9) %>%
    mutate(countOverall = ifelse(placeOnTeam <= 7, 1, 0),
           points = cumsum(countOverall),
           points = ifelse(countOverall == 0, NA, points),
           place = cumsum(count),
           fastestTimeRank = rank(fastest)) %>%
    rename(avg_adj_time = adjusted_avg) %>%
    select(NAME, TEAM, avg_adj_time, place, points) %>%
    mutate(simNumber = i)
  
  simulationBase <- rbind(simulationBase, simulation)
}


simulationResults_2015 <- simulationBase %>%
  mutate(win = ifelse(place == 1, 1, 0),
         auto_qualifier = ifelse(place <= 4, 1, 0),
         all_region = ifelse(place <= 25, 1, 0)) %>%
  group_by(NAME, TEAM) %>%
  summarise(avgTime = mean(avg_adj_time),
            avgPlace = mean(place),
            maxPlace = max(place),
            minPlace = min(place),
            winPct = (sum(win)/1000)*100,
            auto_qualifierPct = (sum(auto_qualifier)/1000)*100,
            all_regionPct = (sum(all_region)/1000)*100) %>%
  mutate(season = 2015)

simulationTeamResults_2015 <- simulationBase %>%
  group_by(TEAM) %>%
  mutate(count = 1,
         total_run = sum(count)) %>%
  ungroup() %>%
  filter(total_run >= 5000) %>%
  mutate(count = 1) %>%
  group_by(TEAM, simNumber) %>%
  mutate(placeOnTeam = cumsum(count)) %>%
  filter(placeOnTeam <= 5) %>%
  group_by(TEAM, simNumber) %>%
  summarise(totalPoints = sum(points)) %>%
  arrange(simNumber, totalPoints) %>%
  group_by(simNumber) %>%
  mutate(count = 1,
         place = cumsum(count)) %>%
  ungroup() %>%
  mutate(win = ifelse(place == 1, 1, 0),
         auto_qual = ifelse(place <= 2, 1, 0)) %>%
  group_by(TEAM) %>%
  summarise(avgPlace = mean(place),
            avgPoints = mean(totalPoints),
            maxPlace = max(place),
            minPlace = min(place),
            pct_auto_qual = (sum(auto_qual)/1000)*100,
            pct_win = (sum(win)/1000)*100) %>%
  mutate(season = 2015)

#### Regionals 2016 ####
mac_df <- mens_data_final_2 %>%
  inner_join(
    mens_data_final_2 %>%
      mutate(DATE = as.Date(DATE, "%m/%d/%y")) %>%
      mutate(season = year(DATE),
             month = month(DATE),
             day = day(DATE)) %>%
      filter(MEET == "NCAA Division I Great Lakes Region Cross Country Championships",
             year(DATE) == 2016) %>%
      select(NAME, TEAM), by = c("NAME", "TEAM")
  ) %>%
  filter(season == "2016",
         !MEET == "UE Cross Country Invitational 2016") %>%
  # filter(TEAM %in% c("Ohio", "Eastern Michigan", "Central Michigan", "Toledo", "Kent State",
  #                    "Bowling Green", "Miami (Ohio)", "Akron", "Michigan", "Notre Dame", "Wisconsin", "Michigan State",
  #                    "Ohio State", "Indiana", "Purdue", "Butler", "Indiana State", "Dayton", "IUPUI", "Marquette",
  #                    "Youngstown St.", "Oakland", "Cincinnati", "Wis.-Milwaukee", "Detroit", "Xavier (Ohio)",
  #                    "Wright State", "Wis.-Green Bay", "Western Michigan", "Ball State", "Cleveland St."),
  #        # !MEET %in% c("Mel Brodt XC Invitational", "Indiana Intercollegiate Championships", "Summit League Championships"),
  #        season == "2016") %>%
  filter(DATE <= as.Date("2016-11-07")) %>%
  na.omit() %>%
  mutate(adj_time = final_adjusted_avg_mile*4.97) %>%
  arrange(desc(DATE)) %>%
  group_by(NAME, TEAM, season) %>%
  mutate(adjusted_avg = mean(adj_time),
         pct_to_avg = (adjusted_avg-adj_time)/adj_time) %>%
  filter(pct_to_avg >= -.017) %>%
  summarise(adjusted_avg = mean(adj_time),
            last_race_adj_time = first(adj_time),
            st_dev = sd(adj_time),
            fastest = min(adj_time)) %>%
  ungroup() %>%
  mutate(st_dev = ifelse(is.na(st_dev), mean(st_dev, na.rm = TRUE), st_dev),
         st_dev = st_dev*.6,
         adjusted_avg = (adjusted_avg+last_race_adj_time)/2) %>%
  select(NAME, TEAM, season, adjusted_avg, st_dev, fastest) %>%
  arrange(adjusted_avg)

PersonAnalysisFinal3 <- mac_df %>%
  # mutate(points = cumsum(count)) %>%
  # filter(placeOnTeam <= 5) %>%
  # group_by(TEAM) %>%mac_df %>%
  group_by(TEAM) %>%
  mutate(count = 1,
         placeOnTeam = cumsum(count)) %>%
  ungroup() %>%
  filter(placeOnTeam <= 7) %>%
  mutate(countOverall = ifelse(placeOnTeam <= 7, 1, 0),
         points = cumsum(countOverall),
         points = ifelse(countOverall == 0, NA, points),
         place = cumsum(count),
         fastestTimeRank = rank(fastest))
# summarise(points = sum(points))

simulationBase <- PersonAnalysisFinal3 %>%
  rename(avg_adj_time = adjusted_avg) %>%
  select(NAME, TEAM, avg_adj_time, place, points) %>%
  mutate(simNumber = 1)

for(i in 2:1000){
  print(i)
  
  simulation <- mac_df %>%
    rowwise() %>%
    mutate(adjusted_avg = rnorm(1, adjusted_avg, st_dev)) %>%
    ungroup() %>%
    arrange(adjusted_avg) %>%
    group_by(TEAM) %>%
    mutate(count = 1,
           placeOnTeam = cumsum(count)) %>%
    ungroup() %>%
    filter(placeOnTeam <= 9) %>%
    mutate(countOverall = ifelse(placeOnTeam <= 7, 1, 0),
           points = cumsum(countOverall),
           points = ifelse(countOverall == 0, NA, points),
           place = cumsum(count),
           fastestTimeRank = rank(fastest)) %>%
    rename(avg_adj_time = adjusted_avg) %>%
    select(NAME, TEAM, avg_adj_time, place, points) %>%
    mutate(simNumber = i)
  
  simulationBase <- rbind(simulationBase, simulation)
}


simulationResults_2016 <- simulationBase %>%
  mutate(win = ifelse(place == 1, 1, 0),
         auto_qualifier = ifelse(place <= 4, 1, 0),
         all_region = ifelse(place <= 25, 1, 0)) %>%
  group_by(NAME, TEAM) %>%
  summarise(avgTime = mean(avg_adj_time),
            avgPlace = mean(place),
            maxPlace = max(place),
            minPlace = min(place),
            winPct = (sum(win)/1000)*100,
            auto_qualifierPct = (sum(auto_qualifier)/1000)*100,
            all_regionPct = (sum(all_region)/1000)*100) %>%
  mutate(season = 2016)

simulationTeamResults_2016 <- simulationBase %>%
  group_by(TEAM) %>%
  mutate(count = 1,
         total_run = sum(count)) %>%
  ungroup() %>%
  filter(total_run >= 5000) %>%
  mutate(count = 1) %>%
  group_by(TEAM, simNumber) %>%
  mutate(placeOnTeam = cumsum(count)) %>%
  filter(placeOnTeam <= 5) %>%
  group_by(TEAM, simNumber) %>%
  summarise(totalPoints = sum(points)) %>%
  arrange(simNumber, totalPoints) %>%
  group_by(simNumber) %>%
  mutate(count = 1,
         place = cumsum(count)) %>%
  ungroup() %>%
  mutate(win = ifelse(place == 1, 1, 0),
         auto_qual = ifelse(place <= 2, 1, 0)) %>%
  group_by(TEAM) %>%
  summarise(avgPlace = mean(place),
            avgPoints = mean(totalPoints),
            maxPlace = max(place),
            minPlace = min(place),
            pct_auto_qual = (sum(auto_qual)/1000)*100,
            pct_win = (sum(win)/1000)*100) %>%
  mutate(season = 2016)

#### Regionals 2017 ####
mac_df <- mens_data_final_2 %>%
  inner_join(
    mens_data_final_2 %>%
      mutate(DATE = as.Date(DATE, "%m/%d/%y")) %>%
      mutate(season = year(DATE),
             month = month(DATE),
             day = day(DATE)) %>%
      filter(MEET == "NCAA Division I Great Lakes Region Cross Country Championships",
             year(DATE) == 2017) %>%
      select(NAME, TEAM), by = c("NAME", "TEAM")
  ) %>%
  filter(season == "2017",
         !MEET == "UE Cross Country Invitational 2016") %>%
  # filter(TEAM %in% c("Ohio", "Eastern Michigan", "Central Michigan", "Toledo", "Kent State",
  #                    "Bowling Green", "Miami (Ohio)", "Akron", "Michigan", "Notre Dame", "Wisconsin", "Michigan State",
  #                    "Ohio State", "Indiana", "Purdue", "Butler", "Indiana State", "Dayton", "IUPUI", "Marquette",
  #                    "Youngstown St.", "Oakland", "Cincinnati", "Wis.-Milwaukee", "Detroit", "Xavier (Ohio)",
  #                    "Wright State", "Wis.-Green Bay", "Western Michigan", "Ball State", "Cleveland St."),
  #        !NAME %in% c("Zimmerman, Zach", "Wise, Adam"),
  #        # !MEET %in% c("Mel Brodt XC Invitational", "Indiana Intercollegiate Championships", "Summit League Championships"),
  #        season == "2017") %>%
  filter(DATE <= as.Date("2017-11-07")) %>%
  na.omit() %>%
  mutate(adj_time = final_adjusted_avg_mile*4.97) %>%
  arrange(desc(DATE)) %>%
  group_by(NAME, TEAM, season) %>%
  mutate(adjusted_avg = mean(adj_time),
         pct_to_avg = (adjusted_avg-adj_time)/adj_time) %>%
  filter(pct_to_avg >= -.017) %>%
  summarise(adjusted_avg = mean(adj_time),
            last_race_adj_time = first(adj_time),
            st_dev = sd(adj_time),
            fastest = min(adj_time)) %>%
  ungroup() %>%
  mutate(st_dev = ifelse(is.na(st_dev), mean(st_dev, na.rm = TRUE), st_dev),
         st_dev = st_dev*.6,
         adjusted_avg = (adjusted_avg+last_race_adj_time)/2) %>%
  select(NAME, TEAM, season, adjusted_avg, st_dev, fastest) %>%
  arrange(adjusted_avg)

PersonAnalysisFinal3 <- mac_df %>%
  # mutate(points = cumsum(count)) %>%
  # filter(placeOnTeam <= 5) %>%
  # group_by(TEAM) %>%mac_df %>%
  group_by(TEAM) %>%
  mutate(count = 1,
         placeOnTeam = cumsum(count)) %>%
  ungroup() %>%
  filter(placeOnTeam <= 7) %>%
  mutate(countOverall = ifelse(placeOnTeam <= 7, 1, 0),
         points = cumsum(countOverall),
         points = ifelse(countOverall == 0, NA, points),
         place = cumsum(count),
         fastestTimeRank = rank(fastest))
# summarise(points = sum(points))

simulationBase <- PersonAnalysisFinal3 %>%
  rename(avg_adj_time = adjusted_avg) %>%
  select(NAME, TEAM, avg_adj_time, place, points) %>%
  mutate(simNumber = 1)

for(i in 2:1000){
  print(i)
  
  simulation <- mac_df %>%
    rowwise() %>%
    mutate(adjusted_avg = rnorm(1, adjusted_avg, st_dev)) %>%
    ungroup() %>%
    arrange(adjusted_avg) %>%
    group_by(TEAM) %>%
    mutate(count = 1,
           placeOnTeam = cumsum(count)) %>%
    ungroup() %>%
    filter(placeOnTeam <= 9) %>%
    mutate(countOverall = ifelse(placeOnTeam <= 7, 1, 0),
           points = cumsum(countOverall),
           points = ifelse(countOverall == 0, NA, points),
           place = cumsum(count),
           fastestTimeRank = rank(fastest)) %>%
    rename(avg_adj_time = adjusted_avg) %>%
    select(NAME, TEAM, avg_adj_time, place, points) %>%
    mutate(simNumber = i)
  
  simulationBase <- rbind(simulationBase, simulation)
}


simulationResults_2017 <- simulationBase %>%
  mutate(win = ifelse(place == 1, 1, 0),
         auto_qualifier = ifelse(place <= 4, 1, 0),
         all_region = ifelse(place <= 25, 1, 0)) %>%
  group_by(NAME, TEAM) %>%
  summarise(avgTime = mean(avg_adj_time),
            avgPlace = mean(place),
            maxPlace = max(place),
            minPlace = min(place),
            winPct = (sum(win)/1000)*100,
            auto_qualifierPct = (sum(auto_qualifier)/1000)*100,
            all_regionPct = (sum(all_region)/1000)*100) %>%
  mutate(season = 2017)

simulationTeamResults_2017 <- simulationBase %>%
  group_by(TEAM) %>%
  mutate(count = 1,
         total_run = sum(count)) %>%
  ungroup() %>%
  filter(total_run >= 5000) %>%
  mutate(count = 1) %>%
  group_by(TEAM, simNumber) %>%
  mutate(placeOnTeam = cumsum(count)) %>%
  filter(placeOnTeam <= 5) %>%
  group_by(TEAM, simNumber) %>%
  summarise(totalPoints = sum(points)) %>%
  arrange(simNumber, totalPoints) %>%
  group_by(simNumber) %>%
  mutate(count = 1,
         place = cumsum(count)) %>%
  ungroup() %>%
  mutate(win = ifelse(place == 1, 1, 0),
         auto_qual = ifelse(place <= 2, 1, 0)) %>%
  group_by(TEAM) %>%
  summarise(avgPlace = mean(place),
            avgPoints = mean(totalPoints),
            maxPlace = max(place),
            minPlace = min(place),
            pct_auto_qual = (sum(auto_qual)/1000)*100,
            pct_win = (sum(win)/1000)*100) %>%
  mutate(season = 2017)

#### Regionals 2018 ####
mac_df <- mens_data_final_2 %>%
  inner_join(
    mens_data_final_2 %>%
      mutate(DATE = as.Date(DATE, "%m/%d/%y")) %>%
      mutate(season = year(DATE),
             month = month(DATE),
             day = day(DATE)) %>%
      filter(MEET == "NCAA Division I Great Lakes Region Cross Country Championships",
             year(DATE) == 2018) %>%
      select(NAME, TEAM), by = c("NAME", "TEAM")
  ) %>%
  filter(season == "2018") %>%
  # filter(TEAM %in% c("Ohio", "Eastern Michigan", "Central Michigan", "Toledo", "Kent State",
  #                    "Bowling Green", "Miami (Ohio)", "Akron", "Michigan", "Notre Dame", "Wisconsin", "Michigan State",
  #                    "Ohio State", "Indiana", "Purdue", "Butler", "Indiana State", "Dayton", "IUPUI", "Marquette",
  #                    "Youngstown St.", "Oakland", "Cincinnati", "Wis.-Milwaukee", "Detroit", "Xavier (Ohio)",
  #                    "Wright State", "Wis.-Green Bay", "Western Michigan", "Ball State", "Cleveland St."),
  #        # !MEET %in% c("Mel Brodt XC Invitational", "Indiana Intercollegiate Championships", "Summit League Championships"),
  #        season == "2018") %>%
  filter(DATE <= as.Date("2018-11-07")) %>%
  na.omit() %>%
  mutate(adj_time = final_adjusted_avg_mile*4.97) %>%
  arrange(desc(DATE)) %>%
  group_by(NAME, TEAM, season) %>%
  mutate(adjusted_avg = mean(adj_time),
         pct_to_avg = (adjusted_avg-adj_time)/adj_time) %>%
  filter(pct_to_avg >= -.017) %>%
  summarise(adjusted_avg = mean(adj_time),
            last_race_adj_time = first(adj_time),
            st_dev = sd(adj_time),
            fastest = min(adj_time)) %>%
  ungroup() %>%
  mutate(st_dev = ifelse(is.na(st_dev), mean(st_dev, na.rm = TRUE), st_dev),
         st_dev = st_dev*.6,
         adjusted_avg = (adjusted_avg+last_race_adj_time)/2) %>%
  select(NAME, TEAM, season, adjusted_avg, st_dev, fastest) %>%
  arrange(adjusted_avg)

PersonAnalysisFinal3 <- mac_df %>%
  # mutate(points = cumsum(count)) %>%
  # filter(placeOnTeam <= 5) %>%
  # group_by(TEAM) %>%mac_df %>%
  group_by(TEAM) %>%
  mutate(count = 1,
         placeOnTeam = cumsum(count)) %>%
  ungroup() %>%
  filter(placeOnTeam <= 7) %>%
  mutate(countOverall = ifelse(placeOnTeam <= 7, 1, 0),
         points = cumsum(countOverall),
         points = ifelse(countOverall == 0, NA, points),
         place = cumsum(count),
         fastestTimeRank = rank(fastest))
# summarise(points = sum(points))

simulationBase <- PersonAnalysisFinal3 %>%
  rename(avg_adj_time = adjusted_avg) %>%
  select(NAME, TEAM, avg_adj_time, place, points) %>%
  mutate(simNumber = 1)

for(i in 2:1000){
  print(i)
  
  simulation <- mac_df %>%
    rowwise() %>%
    mutate(adjusted_avg = rnorm(1, adjusted_avg, st_dev)) %>%
    ungroup() %>%
    arrange(adjusted_avg) %>%
    group_by(TEAM) %>%
    mutate(count = 1,
           placeOnTeam = cumsum(count)) %>%
    ungroup() %>%
    filter(placeOnTeam <= 9) %>%
    mutate(countOverall = ifelse(placeOnTeam <= 7, 1, 0),
           points = cumsum(countOverall),
           points = ifelse(countOverall == 0, NA, points),
           place = cumsum(count),
           fastestTimeRank = rank(fastest)) %>%
    rename(avg_adj_time = adjusted_avg) %>%
    select(NAME, TEAM, avg_adj_time, place, points) %>%
    mutate(simNumber = i)
  
  simulationBase <- rbind(simulationBase, simulation)
}


simulationResults_2018 <- simulationBase %>%
  mutate(win = ifelse(place == 1, 1, 0),
         auto_qualifier = ifelse(place <= 4, 1, 0),
         all_region = ifelse(place <= 25, 1, 0)) %>%
  group_by(NAME, TEAM) %>%
  summarise(avgTime = mean(avg_adj_time),
            avgPlace = mean(place),
            maxPlace = max(place),
            minPlace = min(place),
            winPct = (sum(win)/1000)*100,
            auto_qualifierPct = (sum(auto_qualifier)/1000)*100,
            all_regionPct = (sum(all_region)/1000)*100) %>%
  mutate(season = 2018)

simulationTeamResults_2018 <- simulationBase %>%
  group_by(TEAM) %>%
  mutate(count = 1,
         total_run = sum(count)) %>%
  ungroup() %>%
  filter(total_run >= 5000) %>%
  mutate(count = 1) %>%
  group_by(TEAM, simNumber) %>%
  mutate(placeOnTeam = cumsum(count)) %>%
  filter(placeOnTeam <= 5) %>%
  group_by(TEAM, simNumber) %>%
  summarise(totalPoints = sum(points)) %>%
  arrange(simNumber, totalPoints) %>%
  group_by(simNumber) %>%
  mutate(count = 1,
         place = cumsum(count)) %>%
  ungroup() %>%
  mutate(win = ifelse(place == 1, 1, 0),
         auto_qual = ifelse(place <= 2, 1, 0)) %>%
  group_by(TEAM) %>%
  summarise(avgPlace = mean(place),
            avgPoints = mean(totalPoints),
            maxPlace = max(place),
            minPlace = min(place),
            pct_auto_qual = (sum(auto_qual)/1000)*100,
            pct_win = (sum(win)/1000)*100) %>%
  mutate(season = 2018)

#### Regionals 2019 ####
mac_df <- mens_data_final_2 %>%
  inner_join(
    mens_data_final_2 %>%
      mutate(DATE = as.Date(DATE, "%m/%d/%y")) %>%
      mutate(season = year(DATE),
             month = month(DATE),
             day = day(DATE)) %>%
      filter(MEET == "NCAA Division I Great Lakes Region Cross Country Championships",
             year(DATE) == 2019) %>%
      select(NAME, TEAM), by = c("NAME", "TEAM")
  ) %>%
  filter(season == "2019") %>%
  # filter(TEAM %in% c("Ohio", "Eastern Michigan", "Central Michigan", "Toledo", "Kent State",
  #                    "Bowling Green", "Miami (Ohio)", "Akron", "Michigan", "Notre Dame", "Wisconsin", "Michigan State",
  #                    "Ohio State", "Indiana", "Purdue", "Butler", "Indiana State", "Dayton", "IUPUI", "Marquette",
  #                    "Youngstown St.", "Oakland", "Cincinnati", "Wis.-Milwaukee", "Detroit", "Xavier (Ohio)",
  #                    "Wright State", "Wis.-Green Bay", "Western Michigan", "Ball State", "Cleveland St."),
  #        # !MEET %in% c("Mel Brodt XC Invitational", "Indiana Intercollegiate Championships", "Summit League Championships"),
  #        season == "2019") %>%
  filter(DATE <= as.Date("2019-11-07")) %>%
  na.omit() %>%
  mutate(adj_time = final_adjusted_avg_mile*4.97) %>%
  arrange(desc(DATE)) %>%
  group_by(NAME, TEAM, season) %>%
  mutate(adjusted_avg = mean(adj_time),
         pct_to_avg = (adjusted_avg-adj_time)/adj_time) %>%
  filter(pct_to_avg >= -.017) %>%
  summarise(adjusted_avg = mean(adj_time),
            last_race_adj_time = first(adj_time),
            st_dev = sd(adj_time),
            fastest = min(adj_time)) %>%
  ungroup() %>%
  mutate(st_dev = ifelse(is.na(st_dev), mean(st_dev, na.rm = TRUE), st_dev),
         st_dev = st_dev*.6,
         adjusted_avg = (adjusted_avg+last_race_adj_time)/2) %>%
  select(NAME, TEAM, season, adjusted_avg, st_dev, fastest) %>%
  arrange(adjusted_avg)

PersonAnalysisFinal3 <- mac_df %>%
  # mutate(points = cumsum(count)) %>%
  # filter(placeOnTeam <= 5) %>%
  # group_by(TEAM) %>%mac_df %>%
  group_by(TEAM) %>%
  mutate(count = 1,
         placeOnTeam = cumsum(count)) %>%
  ungroup() %>%
  filter(placeOnTeam <= 7) %>%
  mutate(countOverall = ifelse(placeOnTeam <= 7, 1, 0),
         points = cumsum(countOverall),
         points = ifelse(countOverall == 0, NA, points),
         place = cumsum(count),
         fastestTimeRank = rank(fastest))
# summarise(points = sum(points))

simulationBase <- PersonAnalysisFinal3 %>%
  rename(avg_adj_time = adjusted_avg) %>%
  select(NAME, TEAM, avg_adj_time, place, points) %>%
  mutate(simNumber = 1)

for(i in 2:1000){
  print(i)
  
  simulation <- mac_df %>%
    rowwise() %>%
    mutate(adjusted_avg = rnorm(1, adjusted_avg, st_dev)) %>%
    ungroup() %>%
    arrange(adjusted_avg) %>%
    group_by(TEAM) %>%
    mutate(count = 1,
           placeOnTeam = cumsum(count)) %>%
    ungroup() %>%
    filter(placeOnTeam <= 9) %>%
    mutate(countOverall = ifelse(placeOnTeam <= 7, 1, 0),
           points = cumsum(countOverall),
           points = ifelse(countOverall == 0, NA, points),
           place = cumsum(count),
           fastestTimeRank = rank(fastest)) %>%
    rename(avg_adj_time = adjusted_avg) %>%
    select(NAME, TEAM, avg_adj_time, place, points) %>%
    mutate(simNumber = i)
  
  simulationBase <- rbind(simulationBase, simulation)
}


simulationResults_2019 <- simulationBase %>%
  mutate(win = ifelse(place == 1, 1, 0),
         auto_qualifier = ifelse(place <= 4, 1, 0),
         all_region = ifelse(place <= 25, 1, 0)) %>%
  group_by(NAME, TEAM) %>%
  summarise(avgTime = mean(avg_adj_time),
            avgPlace = mean(place),
            maxPlace = max(place),
            minPlace = min(place),
            winPct = (sum(win)/1000)*100,
            auto_qualifierPct = (sum(auto_qualifier)/1000)*100,
            all_regionPct = (sum(all_region)/1000)*100) %>%
  mutate(season = 2019)

simulationTeamResults_2019 <- simulationBase %>%
  group_by(TEAM) %>%
  mutate(count = 1,
         total_run = sum(count)) %>%
  ungroup() %>%
  filter(total_run >= 5000) %>%
  mutate(count = 1) %>%
  group_by(TEAM, simNumber) %>%
  mutate(placeOnTeam = cumsum(count)) %>%
  filter(placeOnTeam <= 5) %>%
  group_by(TEAM, simNumber) %>%
  summarise(totalPoints = sum(points)) %>%
  arrange(simNumber, totalPoints) %>%
  group_by(simNumber) %>%
  mutate(count = 1,
         place = cumsum(count)) %>%
  ungroup() %>%
  mutate(win = ifelse(place == 1, 1, 0),
         auto_qual = ifelse(place <= 2, 1, 0)) %>%
  group_by(TEAM) %>%
  summarise(avgPlace = mean(place),
            avgPoints = mean(totalPoints),
            maxPlace = max(place),
            minPlace = min(place),
            pct_auto_qual = (sum(auto_qual)/1000)*100,
            pct_win = (sum(win)/1000)*100) %>%
  mutate(season = 2019)

#### Regionals 2021 ####
mac_df <- mens_data_final_2 %>%
  inner_join(
    mens_data_final_2 %>%
      mutate(DATE = as.Date(DATE, "%m/%d/%y")) %>%
      mutate(season = year(DATE),
             month = month(DATE),
             day = day(DATE)) %>%
      filter(MEET == "NCAA Division I Great Lakes Region Cross Country Championships",
             year(DATE) == 2021) %>%
      select(NAME, TEAM), by = c("NAME", "TEAM")
  ) %>%
  filter(season == "2021") %>%
  # filter(TEAM %in% c("Ohio", "Eastern Michigan", "Central Michigan", "Toledo", "Kent State",
  #                    "Bowling Green", "Miami (Ohio)", "Akron", "Michigan", "Notre Dame", "Wisconsin", "Michigan State",
  #                    "Ohio State", "Indiana", "Purdue", "Butler", "Indiana State", "Dayton", "IUPUI", "Marquette",
  #                    "Youngstown St.", "Oakland", "Cincinnati", "Wis.-Milwaukee", "Detroit", "Xavier (Ohio)",
  #                    "Wright State", "Wis.-Green Bay", "Western Michigan", "Ball State", "Cleveland St.", "Milwaukee", "Green Bay")) %>%
  filter(DATE <= as.Date("2021-11-07")) %>%
  na.omit() %>%
  mutate(adj_time = final_adjusted_avg_mile*4.97) %>%
  arrange(desc(DATE)) %>%
  group_by(NAME, TEAM, season) %>%
  mutate(adjusted_avg = mean(adj_time),
         pct_to_avg = (adjusted_avg-adj_time)/adj_time) %>%
  filter(!MEET == 'RMU Colonial Invitational') %>%
  filter(case_when(NAME == "Tarr, Czar" ~ pct_to_avg >= -.015,
                   NAME %in% c("Park, Josh", "Gard, Seth") ~ pct_to_avg >= -0.003,
                   TRUE ~ TRUE)) %>%
  filter(pct_to_avg >= -.017,
         pct_to_avg <= .2) %>%
  mutate(adjusted_avg = mean(adj_time),
         pct_to_avg = (adjusted_avg-adj_time)/adj_time) %>%
  filter(pct_to_avg >= -.017,
         pct_to_avg <= .2) %>%
  summarise(adjusted_avg = mean(adj_time),
            last_race_adj_time = first(adj_time),
            st_dev = sd(adj_time),
            fastest = min(adj_time)) %>%
  ungroup() %>%
  mutate(st_dev = ifelse(is.na(st_dev), mean(st_dev, na.rm = TRUE), st_dev),
         st_dev = st_dev*.6,
         adjusted_avg = (adjusted_avg+last_race_adj_time)/2) %>%
  select(NAME, TEAM, season, adjusted_avg, st_dev, fastest) %>%
  arrange(adjusted_avg) %>%
  group_by(TEAM) %>%
  mutate(rank = rank(adjusted_avg)) %>%
  ungroup() %>%
  filter(rank <= 7)

scores_test <- mac_df %>%
  group_by(TEAM) %>%
  mutate(count = 1,
         total_run = sum(count)) %>%
  ungroup() %>%
  filter(total_run >= 7) %>%
  mutate(count = 1) %>%
  group_by(TEAM) %>%
  mutate(placeOnTeam = cumsum(count)) %>%
  filter(placeOnTeam <= 5) %>%
  ungroup() %>%
  mutate(points = cumsum(count)) %>%
  group_by(TEAM) %>%
  summarise(totalPoints = sum(points))

PersonAnalysisFinal3 <- mac_df %>%
  # mutate(points = cumsum(count)) %>%
  # filter(placeOnTeam <= 5) %>%
  # group_by(TEAM) %>%mac_df %>%
  group_by(TEAM) %>%
  mutate(count = 1,
         placeOnTeam = cumsum(count)) %>%
  ungroup() %>%
  filter(placeOnTeam <= 7) %>%
  mutate(countOverall = ifelse(placeOnTeam <= 7, 1, 0),
         points = cumsum(countOverall),
         points = ifelse(countOverall == 0, NA, points),
         place = cumsum(count),
         fastestTimeRank = rank(fastest))
# summarise(points = sum(points))

simulationBase <- PersonAnalysisFinal3 %>%
  rename(avg_adj_time = adjusted_avg) %>%
  select(NAME, TEAM, avg_adj_time, place, points) %>%
  mutate(simNumber = 1)

for(i in 2:1000){
  print(i)
  
  simulation <- mac_df %>%
    rowwise() %>%
    mutate(adjusted_avg = rnorm(1, adjusted_avg, st_dev)) %>%
    ungroup() %>%
    arrange(adjusted_avg) %>%
    group_by(TEAM) %>%
    mutate(count = 1,
           placeOnTeam = cumsum(count)) %>%
    ungroup() %>%
    filter(placeOnTeam <= 9) %>%
    mutate(countOverall = ifelse(placeOnTeam <= 7, 1, 0),
           points = cumsum(countOverall),
           points = ifelse(countOverall == 0, NA, points),
           place = cumsum(count),
           fastestTimeRank = rank(fastest)) %>%
    rename(avg_adj_time = adjusted_avg) %>%
    select(NAME, TEAM, avg_adj_time, place, points) %>%
    mutate(simNumber = i)
  
  simulationBase <- rbind(simulationBase, simulation)
}


simulationResults_2021 <- simulationBase %>%
  mutate(win = ifelse(place == 1, 1, 0),
         auto_qualifier = ifelse(place <= 4, 1, 0),
         all_region = ifelse(place <= 25, 1, 0)) %>%
  group_by(NAME, TEAM) %>%
  summarise(avgTime = mean(avg_adj_time),
            avgPlace = mean(place),
            maxPlace = max(place),
            minPlace = min(place),
            winPct = (sum(win)/1000)*100,
            auto_qualifierPct = (sum(auto_qualifier)/1000)*100,
            all_regionPct = (sum(all_region)/1000)*100) %>%
  mutate(season = 2021)

simulationTeamResults_2021 <- simulationBase %>%
  group_by(TEAM) %>%
  mutate(count = 1,
         total_run = sum(count)) %>%
  ungroup() %>%
  filter(total_run >= 5000) %>%
  mutate(count = 1) %>%
  group_by(TEAM, simNumber) %>%
  mutate(placeOnTeam = cumsum(count)) %>%
  filter(placeOnTeam <= 5) %>%
  group_by(TEAM, simNumber) %>%
  summarise(totalPoints = sum(points)) %>%
  arrange(simNumber, totalPoints) %>%
  group_by(simNumber) %>%
  mutate(count = 1,
         place = cumsum(count)) %>%
  ungroup() %>%
  mutate(win = ifelse(place == 1, 1, 0),
         auto_qual = ifelse(place <= 2, 1, 0)) %>%
  group_by(TEAM) %>%
  summarise(avgPlace = mean(place),
            avgPoints = mean(totalPoints),
            maxPlace = max(place),
            minPlace = min(place),
            pct_auto_qual = (sum(auto_qual)/1000)*100,
            pct_win = (sum(win)/1000)*100) %>%
  mutate(season = 2021)

regional_simulation <- rbind(simulationResults_2016,
                             simulationResults_2017, simulationResults_2018,
                             simulationResults_2019, simulationResults_2021) %>%
  mutate(gender = "Female")

regional_team_simulation <- rbind(simulationTeamResults_2016,
                                  simulationTeamResults_2017, simulationTeamResults_2018,
                                  simulationTeamResults_2019, simulationTeamResults_2021) %>%
  mutate(gender = "Female")

men_simulation <- read_csv("regional_simulation.csv") %>%
  filter(gender == "Male")

men_simulation <- men_simulation[1:1041,] %>%
  filter(gender == "Female")

men_simulation_team <- read_csv("regional_team_simulation.csv") %>%
  filter(gender == "Male")

men_simulation_team <- men_simulation_team[1:149,] %>%
  filter(gender == "Female")

regional_simulation <- rbind(regional_simulation, men_simulation)

regional_team_simulation <- rbind(regional_team_simulation, men_simulation_team)

write_csv(regional_simulation, "regional_simulation.csv")
write_csv(regional_team_simulation, "regional_team_simulation.csv")
