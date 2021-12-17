library(rvest)
library(tidyverse)
library(RSelenium)
library(lubridate)

meetResults <- read_csv("meet_results_2012-2019.csv")
meetInfo <- read_csv("meet_info_2012-2019.csv")

mens_data <- meetResults %>%
  mutate(gender = ifelse(grepl("Men", race), "Men",
                         ifelse(grepl("Women", race), "Women",
                                ifelse(grepl("M ", race), "Men",
                                       ifelse(grepl("W ", race), "Women", NA)))),
         race_length = ifelse(grepl("5k", race), "5k", ifelse(grepl("5M", race), "5 mile",
                              ifelse(grepl("6k", race), "6k",
                                     ifelse(grepl("8k", race), "8k",
                                            ifelse(grepl("10k", race), "10k",
                                                   ifelse(grepl("5 mile", race), "5 mile",
                                                          ifelse(grepl("8.369k", race), "8.369k",
                                                                 ifelse(grepl("5.2 Mile", race), "5.2 Mile",
                                                                        ifelse(grepl("8.4k", race), "8.4k",
                                                                               ifelse(grepl("5 Mile", race),
                                                                                      "5 mile", NA))))))))))) %>%
         # race_length = case_when(
         #   grepl("5k", race) ~ "5k",
         #   grepl("6k", race) ~ "6k",
         #   grepl("8k", race) ~ "8k",
         #   grepl("10k", race) ~ "10k",
         #   TRUE ~ NA
         # )) %>%
  filter(gender == "Women")

df <- mens_data %>%
  filter(NAME == "Park, Josh")
  
d1Teams <- read_html("https://www.tfrrs.org/leagues/49.html")
d1Teams <- d1Teams %>%
  html_nodes("td:nth-child(2) a") %>%
  html_text()
# d1Teams <- d1Teams %>%
#   html_nodes("td:nth-child(2) a") %>%
#   html_text()

d1Teams <- data.frame("TEAM" = d1Teams)

mens_data_final <- mens_data %>%
  inner_join(meetInfo, by = "link") %>%
  mutate(DATE = as.Date(DATE, "%m/%d/%y")) %>%
  mutate(season = year(DATE),
         month = month(DATE),
         day = day(DATE)) %>%
  filter(month %in% c(9, 10 , 11, 12),
         !grepl(" v ", MEET),
         !grepl(" vs ", MEET),
         !grepl(" vs. ", MEET),
         season >= 2016,
         # race_length %in% c("8k", "5 mile", "8.369k", "10k", "5.2 Mile", "8.4k", "5 Mile")) %>%
         race_length %in% c("5k", "6k")) %>%
  # rowwise() %>%
  # filter(if (month==9) day>12 else TRUE) %>%
  # ungroup() %>%
  group_by(NAME, YEAR, TEAM, location, gender, race_length, raceNumber, DATE, MEET, season) %>%
  summarise(PL = min(PL, na.rm = TRUE),
            `Avg. Mile` = mean(`Avg. Mile`, na.rm = TRUE),
            TIME = mean(TIME, na.rm = TRUE),
            distance = mean(distance, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(race_title = paste0(MEET, "-", raceNumber, "-", season),
         TEAM = ifelse(TEAM == "Ohio U.", "Ohio", TEAM),
         YEAR = case_when(
           grepl("JR", YEAR) ~ "JR",
           grepl("FR", YEAR) ~ "FR",
           grepl("SO", YEAR) ~ "SO",
           grepl("SR", YEAR) ~ "SR"
         )) %>%
  filter(!is.na(YEAR)) %>%
  filter(!NAME == ",",
         !NAME %in% c("*Unknown*, Jerry", "#1174, Runner", '"James, Leaquan'),
         !substr(NAME, 1, 1) == ",") %>%
  group_by(NAME, TEAM) %>%
  mutate(career_average_mt = mean(`Avg. Mile`, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(location = str_replace_all(location, "[[:punct:]]", ""),
         location = tolower(location),
         location = str_replace_all(location, " ", "_"),
         location = str_replace_all(location, "__", "_")) %>%
  mutate(location = substr(location, 1, 14))

modeling_data <- mens_data_final %>%
  inner_join(d1Teams, by = "TEAM") %>%
  na.omit() %>%
  # filter(season >= 2016) %>%
  # select(`Avg. Mile`, career_average_mt, location, race_title, YEAR) %>%
  mutate(location = paste0(location, "-", race_length),
         race_title = as_factor(race_title),
         YEAR = as_factor(YEAR),
         race_length = as_factor(race_length),
         location = as_factor(location)) %>%
  group_by() %>%
  distinct() %>%
  ungroup() %>%
  mutate(difference = (`Avg. Mile`-career_average_mt)/`Avg. Mile`) %>%
  mutate(month = month(DATE),
         day = day(DATE),
         conference = ifelse((month == 10 & day >= 25) | (month == 11 & day <= 5), 1, 0),
         regionals = ifelse(month == 11 & grepl("Region", MEET), 1, 0)) %>%
  select(-month, -day)

testing <- boxplot(modeling_data_base$difference)

modeling_data_base <- modeling_data %>%
  filter(abs(difference) <= .07)

model <- glm(`Avg. Mile`~career_average_mt+YEAR+conference+race_length+location+race_title, family = "gaussian",modeling_data_base)
summary(model)

df <- model[["coefficients"]] %>%
  as.data.frame() %>%
  rownames_to_column(., var = "type")

options(scipen = 999)

# ### mens ####

length_adjustments <- df[7,] %>%
  mutate(type = str_remove(type, "race_length"))

colnames(length_adjustments) = c("race_length", "length_adj")

location_adjustments <- df[8:618,] %>%
  mutate(type = str_remove(type, "location"))

colnames(location_adjustments) = c("location", "location_adj")



race_adjustments <- df[619:1980,] %>%
  mutate(type = str_remove(type, "race_title"))

colnames(race_adjustments) = c("race_title", "race_adj")

# race_adjustments <- race_adjustments %>%
#   mutate(race_adj = ifelse(race_title == 'FSU Invite/Pre-Nats-2-2021', -7, race_adj))

modeling_data <- modeling_data %>%
  left_join(race_adjustments, by = "race_title") %>%
  left_join(location_adjustments, by = "location") %>%
  left_join(length_adjustments, by = "race_length") %>%
  replace_na(list("race_adj" = 0, "location_adj" = 0, "length_adj" = 0))

### womens ####
length_adjustments <- df[6,] %>%
  mutate(type = str_remove(type, "race_length"))

colnames(length_adjustments) = c("race_length", "length_adj")

location_adjustments <- df[7:531,] %>%
  mutate(type = str_remove(type, "location"))

colnames(location_adjustments) = c("location", "location_adj")


race_adjustments <- df[532:1850,] %>%
  mutate(type = str_remove(type, "race_title"))

colnames(race_adjustments) = c("race_title", "race_adj")

modeling_data <- modeling_data %>%
  left_join(race_adjustments, by = "race_title") %>%
  left_join(location_adjustments, by = "location") %>%
  left_join(length_adjustments, by = "race_length") %>%
  replace_na(list("race_adj" = 0, "location_adj" = 0, "length_adj" = 0))

mens_data_final_2 <- modeling_data %>%
  mutate(regionals_adj = 10*regionals,
         final_adjusted_avg_mile = `Avg. Mile`-race_adj-location_adj-length_adj-regionals_adj,
         final_adjusted_time = final_adjusted_avg_mile*distance,
         final_adjusted_time_min = final_adjusted_time/60,
         final_adjusted_time_min = as.numeric(substr(as.character(final_adjusted_time_min), 1, 2)),
         final_adjusted_time_sec = as.character(round(final_adjusted_time-(60*final_adjusted_time_min), digits = 0)),
         difference = (`Avg. Mile`-career_average_mt)/`Avg. Mile`) %>%
  select(NAME, TEAM, PL, location, season, DATE, race_length, MEET, distance, `Avg. Mile`, TIME, final_adjusted_avg_mile, final_adjusted_time)

# write_csv(mens_data_final_2, "womens_data.csv")
write_csv(mens_data_final_2, "mens_data.csv")

test_df <- mac_df %>%
  filter(NAME == "Miller, Brad")
boxplot(test_df$adj_time)

mens_data_final_2 <- read_csv("mens_data.csv")

options(scipen = 9999)
#### MAC's 2015 ####
mac_df <- mens_data_final_2 %>%
  inner_join(
    mens_data_final_2 %>%
      mutate(DATE = as.Date(DATE, "%m/%d/%y")) %>%
      mutate(season = year(DATE),
             month = month(DATE),
             day = day(DATE)) %>%
      filter(MEET == "Mid American Conference Championships",
             year(DATE) == 2015) %>%
      select(NAME, TEAM), by = c("NAME", "TEAM")
  ) %>%
  filter(season == "2015") %>%
  # filter(TEAM %in% c("Ohio", "Eastern Michigan", "Central Michigan", "Toledo", "Kent State",
  #                    "Bowling Green", "Miami (Ohio)", "Akron", "Buffalo", "Western Michigan",
  #                    "Northern Illinois", "Ball State"),
  #        # TEAM %in% c("Ohio", "Eastern Michigan", "Central Michigan", "Toledo", "Kent State",
  #        #             "Bowling Green", "Miami (Ohio)", "Akron", "Buffalo"),
  #        # !MEET %in% c("Mel Brodt XC Invitational"),
  #        season == "2015") %>%
  # filter(!NAME %in% c("Zimmerman, Zach", "Wise, Adam")) %>%
  filter(DATE <= as.Date("2015-10-26")) %>%
  mutate(adj_time = final_adjusted_avg_mile*3.728) %>%
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
  filter(placeOnTeam <= 9) %>%
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
         firstTeam = ifelse(place <= 7, 1, 0),
         allMAC = ifelse(place <= 14, 1, 0)) %>%
  group_by(NAME, TEAM) %>%
  summarise(avgTime = mean(avg_adj_time),
            avgPlace = mean(place),
            medPlace = median(place),
            maxPlace = max(place),
            minPlace = min(place),
            winPct = (sum(win)/1000)*100,
            firstTeamPct = (sum(firstTeam)/1000)*100,
            allMACPct = (sum(allMAC)/1000)*100) %>%
  ungroup() %>%
  mutate(season = 2015)

simulationTeamResults_2015 <- simulationBase %>%
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
  ungroup() %>%
  mutate(season = 2015)

#### MAC's 2016 ####
mac_df <- mens_data_final_2 %>%
  inner_join(
    mens_data_final_2 %>%
      mutate(DATE = as.Date(DATE, "%m/%d/%y")) %>%
      mutate(season = year(DATE),
             month = month(DATE),
             day = day(DATE)) %>%
      filter(MEET == "Mid-American Conference Cross Country Championships",
             year(DATE) == 2016) %>%
      select(NAME, TEAM), by = c("NAME", "TEAM")
  ) %>%
  filter(season == "2016") %>%
  # filter(TEAM %in% c("Ohio", "Eastern Michigan", "Central Michigan", "Toledo", "Kent State",
  #                    "Bowling Green", "Miami (Ohio)", "Akron", "Buffalo", "Western Michigan",
  #                    "Northern Illinois", "Ball State"),
  #        # TEAM %in% c("Ohio", "Eastern Michigan", "Central Michigan", "Toledo", "Kent State",
  #        #             "Bowling Green", "Miami (Ohio)", "Akron", "Buffalo"),
  #        # !MEET %in% c("Mel Brodt XC Invitational"),
  #        season == "2016") %>%
  # filter(!NAME %in% c("Zimmerman, Zach", "Wise, Adam")) %>%
  filter(DATE <= as.Date("2016-10-26")) %>%
  mutate(adj_time = final_adjusted_avg_mile*3.728) %>%
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
  filter(placeOnTeam <= 9) %>%
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
         firstTeam = ifelse(place <= 7, 1, 0),
         allMAC = ifelse(place <= 14, 1, 0)) %>%
  group_by(NAME, TEAM) %>%
  summarise(avgTime = mean(avg_adj_time),
            avgPlace = mean(place),
            medPlace = median(place),
            maxPlace = max(place),
            minPlace = min(place),
            winPct = (sum(win)/1000)*100,
            firstTeamPct = (sum(firstTeam)/1000)*100,
            allMACPct = (sum(allMAC)/1000)*100) %>%
  ungroup() %>%
  mutate(season = 2016)

simulationTeamResults_2016 <- simulationBase %>%
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
  ungroup() %>%
  mutate(season = 2016)

#### MAC's 2017 ####
mac_df <- mens_data_final_2 %>%
  inner_join(
    mens_data_final_2 %>%
      mutate(DATE = as.Date(DATE, "%m/%d/%y")) %>%
      mutate(season = year(DATE),
             month = month(DATE),
             day = day(DATE)) %>%
      filter(MEET == "2017 Mid-American Conference XC Championships",
             year(DATE) == 2017) %>%
      select(NAME, TEAM), by = c("NAME", "TEAM")
  ) %>%
  filter(season == "2017") %>%
  # filter(TEAM %in% c("Ohio", "Eastern Michigan", "Central Michigan", "Toledo", "Kent State",
  #                    "Bowling Green", "Miami (Ohio)", "Akron", "Buffalo", "Western Michigan",
  #                    "Northern Illinois", "Ball State"),
  #        # TEAM %in% c("Ohio", "Eastern Michigan", "Central Michigan", "Toledo", "Kent State",
  #        #             "Bowling Green", "Miami (Ohio)", "Akron", "Buffalo"),
  #        # !MEET %in% c("Mel Brodt XC Invitational"),
  #        season == "2017") %>%
  # filter(!NAME %in% c("Zimmerman, Zach", "Wise, Adam")) %>%
  filter(DATE <= as.Date("2017-10-26")) %>%
  mutate(adj_time = final_adjusted_avg_mile*3.728) %>%
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
  filter(placeOnTeam <= 9) %>%
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
         firstTeam = ifelse(place <= 7, 1, 0),
         allMAC = ifelse(place <= 14, 1, 0)) %>%
  group_by(NAME, TEAM) %>%
  summarise(avgTime = mean(avg_adj_time),
            avgPlace = mean(place),
            medPlace = median(place),
            maxPlace = max(place),
            minPlace = min(place),
            winPct = (sum(win)/1000)*100,
            firstTeamPct = (sum(firstTeam)/1000)*100,
            allMACPct = (sum(allMAC)/1000)*100) %>%
  ungroup() %>%
  mutate(season = 2017)

simulationTeamResults_2017 <- simulationBase %>%
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
  ungroup() %>%
  mutate(season = 2017)

#### MAC's 2018 ####
mac_df <- mens_data_final_2 %>%
  inner_join(
    mens_data_final_2 %>%
      mutate(DATE = as.Date(DATE, "%m/%d/%y")) %>%
      mutate(season = year(DATE),
             month = month(DATE),
             day = day(DATE)) %>%
      filter(MEET == "MAC championship",
             year(DATE) == 2018) %>%
      select(NAME, TEAM), by = c("NAME", "TEAM")
  ) %>%
  filter(season == "2018") %>%
  # filter(TEAM %in% c("Ohio", "Eastern Michigan", "Central Michigan", "Toledo", "Kent State",
  #                    "Bowling Green", "Miami (Ohio)", "Akron", "Buffalo", "Western Michigan",
  #                    "Northern Illinois", "Ball State"),
  #        # TEAM %in% c("Ohio", "Eastern Michigan", "Central Michigan", "Toledo", "Kent State",
  #        #             "Bowling Green", "Miami (Ohio)", "Akron", "Buffalo"),
  #        # !MEET %in% c("Mel Brodt XC Invitational"),
  #        season == "2018") %>%
  # filter(!NAME %in% c("Zimmerman, Zach", "Wise, Adam")) %>%
  filter(DATE <= as.Date("2018-10-26")) %>%
  mutate(adj_time = final_adjusted_avg_mile*3.728) %>%
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
  filter(placeOnTeam <= 9) %>%
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
         firstTeam = ifelse(place <= 7, 1, 0),
         allMAC = ifelse(place <= 14, 1, 0)) %>%
  group_by(NAME, TEAM) %>%
  summarise(avgTime = mean(avg_adj_time),
            avgPlace = mean(place),
            medPlace = median(place),
            maxPlace = max(place),
            minPlace = min(place),
            winPct = (sum(win)/1000)*100,
            firstTeamPct = (sum(firstTeam)/1000)*100,
            allMACPct = (sum(allMAC)/1000)*100) %>%
  ungroup() %>%
  mutate(season = 2018)

simulationTeamResults_2018 <- simulationBase %>%
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
  ungroup() %>%
  mutate(season = 2018)

#### MAC's 2019 ####
mac_df <- mens_data_final_2 %>%
  inner_join(
    mens_data_final_2 %>%
      mutate(DATE = as.Date(DATE, "%m/%d/%y")) %>%
      mutate(season = year(DATE),
             month = month(DATE),
             day = day(DATE)) %>%
      filter(MEET == "Mid American Conference Meet",
             year(DATE) == 2019) %>%
      select(NAME, TEAM), by = c("NAME", "TEAM")
  ) %>%
  filter(season == "2019") %>%
  # filter(TEAM %in% c("Ohio", "Eastern Michigan", "Central Michigan", "Toledo", "Kent State",
  #                    "Bowling Green", "Miami (Ohio)", "Akron", "Buffalo", "Western Michigan",
  #                    "Northern Illinois", "Ball State"),
  #        # TEAM %in% c("Ohio", "Eastern Michigan", "Central Michigan", "Toledo", "Kent State",
  #        #             "Bowling Green", "Miami (Ohio)", "Akron", "Buffalo"),
  #        # !MEET %in% c("Mel Brodt XC Invitational"),
  #        season == "2019") %>%
  # # filter(!NAME %in% c("Zimmerman, Zach", "Wise, Adam")) %>%
  filter(DATE <= as.Date("2019-10-26")) %>%
  mutate(adj_time = final_adjusted_avg_mile*3.728) %>%
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
  filter(placeOnTeam <= 9) %>%
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
         firstTeam = ifelse(place <= 7, 1, 0),
         allMAC = ifelse(place <= 14, 1, 0)) %>%
  group_by(NAME, TEAM) %>%
  summarise(avgTime = mean(avg_adj_time),
            avgPlace = mean(place),
            medPlace = median(place),
            maxPlace = max(place),
            minPlace = min(place),
            winPct = (sum(win)/1000)*100,
            firstTeamPct = (sum(firstTeam)/1000)*100,
            allMACPct = (sum(allMAC)/1000)*100) %>%
  ungroup() %>%
  mutate(season = 2019)

simulationTeamResults_2019 <- simulationBase %>%
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
  ungroup() %>%
  mutate(season = 2019)

options(scipen =  99999)
#### MAC's 2021 ####
mac_df <- mens_data_final_2 %>%
  inner_join(
    mens_data_final_2 %>%
      # mutate(DATE = as.Date(DATE, "%m/%d/%y")) %>%
      mutate(season = year(DATE),
             month = month(DATE),
             day = day(DATE)) %>%
      filter(MEET == "MAC Cross Country Championships",
             year(DATE) == 2021) %>%
      select(NAME, TEAM), by = c("NAME", "TEAM")
  ) %>%
  filter(season == "2021",
         DATE < as.Date("2021-10-28")) %>%
  # filter(TEAM %in% c("Ohio", "Eastern Michigan", "Central Michigan", "Toledo", "Kent State",
  #                    "Bowling Green", "Miami (Ohio)", "Akron", "Buffalo", "Western Michigan",
  #                    "Northern Illinois", "Ball State"),
  #        # TEAM %in% c("Ohio", "Eastern Michigan", "Central Michigan", "Toledo", "Kent State",
  #        #             "Bowling Green", "Miami (Ohio)", "Buffalo"),
  #        # !MEET %in% c("Mel Brodt XC Invitational"),
  #        season == "2021") %>%
  # # filter(!NAME %in% c("Zimmerman, Zach", "Wise, Adam")) %>%
  # filter(DATE <= as.Date("2019-10-26")) %>%
  mutate(adj_time = final_adjusted_avg_mile*3.728) %>%
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
  arrange(adjusted_avg) %>%
  group_by(TEAM) %>%
  mutate(rank = rank(adjusted_avg)) %>%
  ungroup() %>%
  filter(rank <= 9)

PersonAnalysisFinal3 <- mac_df %>%
  # mutate(points = cumsum(count)) %>%
  # filter(placeOnTeam <= 5) %>%
  # group_by(TEAM) %>%mac_df %>%
  group_by(TEAM) %>%
  mutate(count = 1,
         placeOnTeam = cumsum(count)) %>%
  ungroup() %>%
  filter(placeOnTeam <= 9) %>%
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
         firstTeam = ifelse(place <= 7, 1, 0),
         allMAC = ifelse(place <= 14, 1, 0)) %>%
  group_by(NAME, TEAM) %>%
  summarise(avgTime = mean(avg_adj_time),
            avgPlace = mean(place),
            medPlace = median(place),
            maxPlace = max(place),
            minPlace = min(place),
            winPct = (sum(win)/1000)*100,
            firstTeamPct = (sum(firstTeam)/1000)*100,
            allMACPct = (sum(allMAC)/1000)*100) %>%
  ungroup() %>%
  mutate(season = 2021)

simulationTeamResults_2021<- simulationBase %>%
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
  ungroup() %>%
  mutate(season = 2021)

mac_simulation <- rbind(simulationResults_2016,
                        simulationResults_2017, simulationResults_2018,
                        simulationResults_2019, simulationResults_2021) %>%
  mutate(gender = "Female")

mac_simulation_team <- rbind(simulationTeamResults_2016,
                             simulationTeamResults_2017, simulationTeamResults_2018,
                             simulationTeamResults_2019, simulationTeamResults_2021) %>%
  mutate(gender = "Female")

men_simulation <- read_csv("mac_simulation.csv") %>%
  filter(gender == "Male")
men_simulation_team <- read_csv("mac_simulation_team.csv") %>%
  filter(gender == "Male")
# 
# mac_simulation <- rbind(mac_simulation, men_simulation)
# 
# mac_simulation_team <- rbind(mac_simulation_team, men_simulation_team)

write_csv(mac_simulation, "mac_simulation.csv")
write_csv(mac_simulation_team, "mac_simulation_team.csv")
