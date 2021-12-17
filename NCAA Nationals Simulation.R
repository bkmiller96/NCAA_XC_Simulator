#### Nationals 2015####
#mens_data_final_2 <- read_csv("womens_data.csv")

nationals <- mens_data_final_2 %>%
  # mutate(DATE = as.Date(DATE, "%m/%d/%y")) %>%
  mutate(season = year(DATE),
         month = month(DATE),
         day = day(DATE)) %>%
  filter(MEET == "NCAA Division I Cross Country Championships",
         season == 2015) %>%
  select(NAME, TEAM)

mac_df <- mens_data_final_2 %>%
  # filter(TEAM %in% c("Ohio", "Eastern Michigan", "Central Michigan", "Toledo", "Kent State",
  #                    "Bowling Green", "Miami (Ohio)", "Akron", "Michigan", "Notre Dame", "Wisconsin", "Michigan State",
  #                    "Ohio State", "Indiana", "Purdue", "Butler", "Indiana State", "Dayton", "IUPUI", "Marquette",
  #                    "Youngstown St.", "Oakland", "Cincinnati", "Wis.-Milwaukee", "Detroit", "Xavier (Ohio)",
  #                    "Wright State", "Wis.-Green Bay"),
  #        # !MEET %in% c("Mel Brodt XC Invitational", "Indiana Intercollegiate Championships", "Summit League Championships"),
  #        season == "2019") %>%
  filter(season == "2015") %>%
  filter(DATE <= as.Date("2015-11-19"), 
         DATE >= as.Date("2015-09-25")) %>%
  inner_join(nationals, by = c("NAME", "TEAM")) %>%
  na.omit() %>%
  mutate(adj_time = final_adjusted_avg_mile*4.97) %>%
  arrange(desc(DATE)) %>%
  group_by(NAME, TEAM, season) %>%
  mutate(adjusted_avg = mean(adj_time),
         pct_to_avg = (adjusted_avg-adj_time)/adj_time) %>%
  # filter(pct_to_avg >= -.017) %>%
  filter(case_when(
    grepl("Region ", MEET) ~ pct_to_avg >= -.01,
    TRUE ~ pct_to_avg >= -.017
  )) %>%
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
         allAmerican = ifelse(place <= 40, 1, 0)) %>%
  group_by(NAME, TEAM) %>%
  summarise(avgTime = mean(avg_adj_time),
            avgPlace = mean(place),
            maxPlace = max(place),
            minPlace = min(place),
            winPct = (sum(win)/1000)*100,
            allAmericanPct = (sum(allAmerican)/1000)*100) %>%
  mutate(season = 2015,
         gender = "Female")

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
         podium = ifelse(place <= 3, 1, 0)) %>%
  group_by(TEAM) %>%
  summarise(avgPlace = mean(place),
            avgPoints = mean(totalPoints),
            maxPlace = max(place),
            minPlace = min(place),
            pctWin = (sum(win)/1000)*100,
            pctPodium = (sum(podium)/1000)*100) %>%
  mutate(season = 2015,
         gender = "Female")

#### Nationals 2016####
nationals <- mens_data_final_2 %>%
  # mutate(DATE = as.Date(DATE, "%m/%d/%y")) %>%
  mutate(season = year(DATE),
         month = month(DATE),
         day = day(DATE)) %>%
  filter(MEET == "NCAA Division I Cross Country Championships",
         season == 2016) %>%
  select(NAME, TEAM)


mac_df <- mens_data_final_2 %>%
  # filter(TEAM %in% c("Ohio", "Eastern Michigan", "Central Michigan", "Toledo", "Kent State",
  #                    "Bowling Green", "Miami (Ohio)", "Akron", "Michigan", "Notre Dame", "Wisconsin", "Michigan State",
  #                    "Ohio State", "Indiana", "Purdue", "Butler", "Indiana State", "Dayton", "IUPUI", "Marquette",
  #                    "Youngstown St.", "Oakland", "Cincinnati", "Wis.-Milwaukee", "Detroit", "Xavier (Ohio)",
  #                    "Wright State", "Wis.-Green Bay"),
  #        # !MEET %in% c("Mel Brodt XC Invitational", "Indiana Intercollegiate Championships", "Summit League Championships"),
  #        season == "2019") %>%
  filter(season == "2016") %>%
  filter(DATE >= as.Date("2016-09-30"),
         DATE <= as.Date("2016-11-18")) %>%
  inner_join(nationals, by = c("NAME", "TEAM")) %>%
  na.omit() %>%
  mutate(adj_time = final_adjusted_avg_mile*4.97) %>%
  arrange(desc(DATE)) %>%
  group_by(NAME, TEAM, season) %>%
  mutate(adjusted_avg = mean(adj_time),
         pct_to_avg = (adjusted_avg-adj_time)/adj_time) %>%
  # filter(pct_to_avg >= -.017) %>%
  filter(case_when(
    grepl("Region ", MEET) ~ pct_to_avg >= -.01,
    TRUE ~ pct_to_avg >= -.017
  )) %>%
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
         allAmerican = ifelse(place <= 40, 1, 0)) %>%
  group_by(NAME, TEAM) %>%
  summarise(avgTime = mean(avg_adj_time),
            avgPlace = mean(place),
            maxPlace = max(place),
            minPlace = min(place),
            winPct = (sum(win)/1000)*100,
            allAmericanPct = (sum(allAmerican)/1000)*100) %>%
  mutate(season = 2016,
         gender = "Female")

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
         podium = ifelse(place <= 3, 1, 0)) %>%
  group_by(TEAM) %>%
  summarise(avgPlace = mean(place),
            avgPoints = mean(totalPoints),
            maxPlace = max(place),
            minPlace = min(place),
            pctWin = (sum(win)/1000)*100,
            pctPodium = (sum(podium)/1000)*100) %>%
  mutate(season = 2016,
         gender = "Female")

#### Nationals 2017####
nationals <- mens_data_final_2 %>%
  # mutate(DATE = as.Date(DATE, "%m/%d/%y")) %>%
  mutate(season = year(DATE),
         month = month(DATE),
         day = day(DATE)) %>%
  filter(MEET == "NCAA Division I Cross Country Championships",
         season == 2017) %>%
  select(NAME, TEAM)

mac_df <- mens_data_final_2 %>%
  # filter(TEAM %in% c("Ohio", "Eastern Michigan", "Central Michigan", "Toledo", "Kent State",
  #                    "Bowling Green", "Miami (Ohio)", "Akron", "Michigan", "Notre Dame", "Wisconsin", "Michigan State",
  #                    "Ohio State", "Indiana", "Purdue", "Butler", "Indiana State", "Dayton", "IUPUI", "Marquette",
  #                    "Youngstown St.", "Oakland", "Cincinnati", "Wis.-Milwaukee", "Detroit", "Xavier (Ohio)",
  #                    "Wright State", "Wis.-Green Bay"),
  #        # !MEET %in% c("Mel Brodt XC Invitational", "Indiana Intercollegiate Championships", "Summit League Championships"),
  #        season == "2019") %>%
  filter(season == "2017") %>%
  filter(DATE >= as.Date("2017-09-22"),
         DATE <= as.Date("2017-11-17")) %>%
  inner_join(nationals, by = c("NAME", "TEAM")) %>%
  na.omit() %>%
  mutate(adj_time = final_adjusted_avg_mile*4.97) %>%
  arrange(desc(DATE)) %>%
  group_by(NAME, TEAM, season) %>%
  mutate(adjusted_avg = mean(adj_time),
         pct_to_avg = (adjusted_avg-adj_time)/adj_time) %>%
  filter(case_when(
    grepl("Region ", MEET) ~ pct_to_avg >= -.01,
    TRUE ~ pct_to_avg >= -.017
  )) %>%
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
         allAmerican = ifelse(place <= 40, 1, 0)) %>%
  group_by(NAME, TEAM) %>%
  summarise(avgTime = mean(avg_adj_time),
            avgPlace = mean(place),
            maxPlace = max(place),
            minPlace = min(place),
            winPct = (sum(win)/1000)*100,
            allAmericanPct = (sum(allAmerican)/1000)*100) %>%
  mutate(season = 2017,
         gender = "Female")

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
         podium = ifelse(place <= 3, 1, 0)) %>%
  group_by(TEAM) %>%
  summarise(avgPlace = mean(place),
            avgPoints = mean(totalPoints),
            maxPlace = max(place),
            minPlace = min(place),
            pctWin = (sum(win)/1000)*100,
            pctPodium = (sum(podium)/1000)*100) %>%
  mutate(season = 2017,
         gender = "Female")

#### Nationals 2018####
nationals <- mens_data_final_2 %>%
  # mutate(DATE = as.Date(DATE, "%m/%d/%y")) %>%
  mutate(season = year(DATE),
         month = month(DATE),
         day = day(DATE)) %>%
  filter(MEET == "NCAA DI Cross Country Championships",
         season == 2018) %>%
  select(NAME, TEAM)

mac_df <- mens_data_final_2 %>%
  # filter(TEAM %in% c("Ohio", "Eastern Michigan", "Central Michigan", "Toledo", "Kent State",
  #                    "Bowling Green", "Miami (Ohio)", "Akron", "Michigan", "Notre Dame", "Wisconsin", "Michigan State",
  #                    "Ohio State", "Indiana", "Purdue", "Butler", "Indiana State", "Dayton", "IUPUI", "Marquette",
  #                    "Youngstown St.", "Oakland", "Cincinnati", "Wis.-Milwaukee", "Detroit", "Xavier (Ohio)",
  #                    "Wright State", "Wis.-Green Bay"),
  #        # !MEET %in% c("Mel Brodt XC Invitational", "Indiana Intercollegiate Championships", "Summit League Championships"),
  #        season == "2019") %>%
  filter(season == "2018") %>%
  filter(DATE >= as.Date("2018-09-20"),
         DATE <= as.Date("2018-11-16"),
         !MEET == "NCAA Division I Mountain Region Cross Country") %>%
  inner_join(nationals, by = c("NAME", "TEAM")) %>%
  na.omit() %>%
  mutate(adj_time = final_adjusted_avg_mile*4.97) %>%
  arrange(desc(DATE)) %>%
  group_by(NAME, TEAM, season) %>%
  mutate(adjusted_avg = mean(adj_time),
         pct_to_avg = (adjusted_avg-adj_time)/adj_time) %>%
  # filter(pct_to_avg >= -.017) %>%
  filter(case_when(
    grepl("Region ", MEET) ~ pct_to_avg >= -.01,
    TRUE ~ pct_to_avg >= -.017
  )) %>%
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
         allAmerican = ifelse(place <= 40, 1, 0)) %>%
  group_by(NAME, TEAM) %>%
  summarise(avgTime = mean(avg_adj_time),
            avgPlace = mean(place),
            maxPlace = max(place),
            minPlace = min(place),
            winPct = (sum(win)/1000)*100,
            allAmericanPct = (sum(allAmerican)/1000)*100) %>%
  mutate(season = 2018,
         gender = "Female")

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
         podium = ifelse(place <= 3, 1, 0)) %>%
  group_by(TEAM) %>%
  summarise(avgPlace = mean(place),
            avgPoints = mean(totalPoints),
            maxPlace = max(place),
            minPlace = min(place),
            pctWin = (sum(win)/1000)*100,
            pctPodium = (sum(podium)/1000)*100) %>%
  mutate(season = 2018,
         gender = "Female")

#### Nationals 2019####
nationals <- mens_data_final_2 %>%
  # mutate(DATE = as.Date(DATE, "%m/%d/%y")) %>%
  mutate(season = year(DATE),
         month = month(DATE),
         day = day(DATE)) %>%
  filter(MEET == "NCAA Division I Cross Country Championships",
         season == 2019) %>%
  select(NAME, TEAM)

mac_df <- mens_data_final_2 %>%
  # filter(TEAM %in% c("Ohio", "Eastern Michigan", "Central Michigan", "Toledo", "Kent State",
  #                    "Bowling Green", "Miami (Ohio)", "Akron", "Michigan", "Notre Dame", "Wisconsin", "Michigan State",
  #                    "Ohio State", "Indiana", "Purdue", "Butler", "Indiana State", "Dayton", "IUPUI", "Marquette",
  #                    "Youngstown St.", "Oakland", "Cincinnati", "Wis.-Milwaukee", "Detroit", "Xavier (Ohio)",
  #                    "Wright State", "Wis.-Green Bay"),
  #        # !MEET %in% c("Mel Brodt XC Invitational", "Indiana Intercollegiate Championships", "Summit League Championships"),
  #        season == "2019") %>%
  filter(season == "2019") %>%
  filter(DATE >= as.Date("2019-09-22"),
         DATE <= as.Date("2019-11-17")) %>%
  inner_join(nationals, by = c("NAME", "TEAM")) %>%
  na.omit() %>%
  mutate(adj_time = final_adjusted_avg_mile*4.97) %>%
  arrange(desc(DATE)) %>%
  group_by(NAME, TEAM, season) %>%
  mutate(adjusted_avg = mean(adj_time),
         pct_to_avg = (adjusted_avg-adj_time)/adj_time) %>%
  # filter(pct_to_avg >= -.017) %>%
  filter(case_when(
    grepl("Region ", MEET) ~ pct_to_avg >= -.01,
    TRUE ~ pct_to_avg >= -.017
  )) %>%
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
         allAmerican = ifelse(place <= 40, 1, 0)) %>%
  group_by(NAME, TEAM) %>%
  summarise(avgTime = mean(avg_adj_time),
            avgPlace = mean(place),
            maxPlace = max(place),
            minPlace = min(place),
            winPct = (sum(win)/1000)*100,
            allAmericanPct = (sum(allAmerican)/1000)*100) %>%
  mutate(season = 2019,
         gender = "Female")

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
         podium = ifelse(place <= 3, 1, 0)) %>%
  group_by(TEAM) %>%
  summarise(avgPlace = mean(place),
            avgPoints = mean(totalPoints),
            maxPlace = max(place),
            minPlace = min(place),
            pctWin = (sum(win)/1000)*100,
            pctPodium = (sum(podium)/1000)*100) %>%
  mutate(season = 2019,
         gender = "Female")

#### Nationals 2021####
# nationals <- mens_data_final_2 %>%
#   # mutate(DATE = as.Date(DATE, "%m/%d/%y")) %>%
#   mutate(season = year(DATE),
#          month = month(DATE),
#          day = day(DATE)) %>%
#   filter(MEET == "NCAA Division I Cross Country Championships",
#          season == 2019) %>%
#   select(NAME, TEAM)

mac_df <- mens_data_final_2 %>%
  # filter(TEAM %in% c("Alabama", "Arkansas", "BYU", "Furman", "Georgetown", "Harvard", "Iowa State", "Northern Arizona", "Notre Dame", "Oklahoma State",
  #                    "Ole Miss", "Princeton", "Stanford", "Syracuse", "Texas", "Wake Forest", "Washington", "Wisconsin", "Air Force", "Butler", "Colorado",
  #                    "Florida State", "Gonzaga", "Michigan", "Michigan State", "Minnesota", "North Carolina", "Oregon", "Portland", "Southern Utah", "Tulsa") |
  #          NAME %in% c("Sandusky, Alec", "Jha, Arjun", "Eckstein, Curtis", "Magnusson, Baldvin", "Choge, Jacob", "Thiessen, Karl", "Yego, Kirami", 
  #                      "Bowers, Sam", "O'Callaghan, Grant", "Strintzos, Haftu", "Carey, Noah", "Jennings, Jack", "Phillips, Josh", "Mthembu, Kwanele",
  #                      "Hilton, Lexington", "Makuvire, Bradley", "Kipngeno, Dennis", "Davis, Jonathan", "Fogg, Adam", "Hatfield, Dustin", "Basten, Isaac",
  #                      "Kioko, Athanas", "Curtin, Fearghal", "Flavin, JP", "Arredondo, Paul", "Allen, Christian", "Hamilton, Duncan", "Ibrahim, Abdirizak",
  #                      "Richtman, Matthew", "De Caro, Dario", "Ado, Amir", "Ibrahim, Ahmed", "Rees, Logan", "El-Sandali, Ehab", "Goddard, Ed", "Rocha, Marcelo",
  #                      "Mackinnon, Perry", "Berg, Tyler")) %>%
  filter(TEAM %in% c("Arkansas", "Colorado", "Florida State", "Georgetown", "Harvard", "Minnesota", "NC State", "New Mexico", "North Carolina", "Notre Dame",
                     "Oklahoma State", "Ole Miss", "Rice", "Stanford", "Syracuse", "Washington", "West Virginia", "Wisconsin", "Alabama", "Butler", "BYU",
                     "Colorado St.", "Iowa State", "Michigan", "Michigan State", "Northern Arizona", "Oregon", "Providence", "Utah", "Utah State", "Villanova") |
           NAME %in% c("Engel, Addie", "Leather, Ellie", "Chirchir, Joy", "Tate, Emma", "Valby, Parker", "Seymour, Sydney", "Kimeli, Joyce", "Stallworth, Lindsey",
                       "Ragenklint, Sammi", "Max, Fiona", "Ramos, Ashlyn", "Joyce, Michelle", "Rono, Irene", "McDonald, Katy-Ann", "Ramos, Beth", "Kiyeng, Prudence",
                       "Kleshchukova, Arina", "McCardell, Rachel", "Freyhof, Erika", "Murrin, Anneka", "Latema, Lona", "Herman, Tori", "Doan, Calli", "Graham, Bethany",
                       "Eastman, Annabelle", "Bockrath, Perri", "Wolfgram, Tierney", "Pray, Allison", "Allen, Summer", "Hatch, Billie", "Smee, Ruby", "Garcia, Kristen",
                       "Camarena, Katie", "Mitchell, Kaylee", "DeLay, Kayley", "Mackay, Emily", "Anderson, Phoebe", "Murphy, Sophie")) %>%
  filter(season == "2021") %>%
  filter(DATE >= as.Date("2021-09-22"),
         DATE <= as.Date("2021-11-17")) %>%
  # inner_join(nationals, by = c("NAME", "TEAM")) %>%
  na.omit() %>%
  mutate(adj_time = final_adjusted_avg_mile*4.97) %>%
  arrange(desc(DATE)) %>%
  group_by(NAME, TEAM, season) %>%
  mutate(adjusted_avg = mean(adj_time),
         pct_to_avg = (adjusted_avg-adj_time)/adj_time) %>%
  # filter(pct_to_avg >= -.017) %>%
  filter(case_when(
    grepl("Region ", MEET) ~ pct_to_avg >= -.01,
    TRUE ~ pct_to_avg >= -.017
  )) %>%
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

test_df <- mac_df %>%
  distinct(TEAM)

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
    group_by(TEAM) %>%
    mutate(count = 1,
           placeOnTeam = cumsum(count)) %>%
    ungroup() %>%
    filter(placeOnTeam <= 7) %>%
    rowwise() %>%
    mutate(adjusted_avg = rnorm(1, adjusted_avg, st_dev)) %>%
    ungroup() %>%
    arrange(adjusted_avg) %>%
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
         allAmerican = ifelse(place <= 40, 1, 0)) %>%
  group_by(NAME, TEAM) %>%
  summarise(avgTime = mean(avg_adj_time),
            avgPlace = mean(place),
            maxPlace = max(place),
            minPlace = min(place),
            winPct = (sum(win)/1000)*100,
            allAmericanPct = (sum(allAmerican)/1000)*100) %>%
  mutate(season = 2021,
         gender = "Female")

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
         podium = ifelse(place <= 3, 1, 0)) %>%
  group_by(TEAM) %>%
  summarise(avgPlace = mean(place),
            avgPoints = mean(totalPoints),
            maxPlace = max(place),
            minPlace = min(place),
            pctWin = (sum(win)/1000)*100,
            pctPodium = (sum(podium)/1000)*100) %>%
  mutate(season = 2021,
         gender = "Female")

nationals_simulation <- rbind(simulationResults_2016,
                        simulationResults_2017, simulationResults_2018,
                        simulationResults_2019, simulationResults_2021)

nationals_simulation_team <- rbind(simulationTeamResults_2016,
                             simulationTeamResults_2017, simulationTeamResults_2018,
                             simulationTeamResults_2019, simulationTeamResults_2021)

mens_simulation <- read_csv("nationals_simulation.csv") %>%
  mutate(gender = "Male")

mens_simulation_team <- read_csv("nationals_simulation_team.csv") %>%
  mutate(gender = "Male")

nationals_simulation <- rbind(nationals_simulation, mens_simulation)

nationals_simulation_team <- rbind(nationals_simulation_team, mens_simulation_team)

write_csv(nationals_simulation, "nationals_simulation.csv")
write_csv(nationals_simulation_team, "nationals_simulation_team.csv")
