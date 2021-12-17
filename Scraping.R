library(rvest)
library(tidyverse)
library(RSelenium)

### Get Links/Meet Info ####
rD <- rsDriver(browser="firefox", port=4545L, verbose=F)
remDr <- rD[["client"]]

remDr$open()

#### 2021 ####
Sys.sleep(5)
remDr$navigate("https://www.tfrrs.org/results_search.html")
remDr$screenshot(TRUE)

webElem1 <- remDr$findElement(using = "xpath", "/html/body/div[3]/form/div/div/div[2]/div[2]/div[1]/select/option[3]")
webElem1$clickElement()

webElem2 <- remDr$findElement(using = "xpath", "/html/body/div[3]/form/div/div/div[2]/div[2]/div[4]/select/option[2]")
webElem2$clickElement()

webElem3 <- remDr$findElement(using = "xpath", "/html/body/div[3]/form/div/div/div[2]/div[3]/div/button")
webElem3$clickElement()

meetListLinkk <- remDr$getPageSource()
meetList <- read_html(meetListLinkk[[1]])

meetTableAll <- meetList %>%
  html_nodes(xpath = '//*[@id="tablesaw-8744"]') %>%
  html_table()
meetTableAll <- meetTableAll[[1]]

meetLink <- meetList %>%
  html_nodes("td:nth-child(2)") %>%
  html_node("a") %>%
  html_attr("href") %>%
  substr(3, nchar(.)-1) %>%
  paste0("https://", .)

meetTableAll$link <- meetLink

meetTable_2021 <- meetTableAll %>%
  filter(!MEET %in% c("AFXC 5K Open", "NCCAA National XC Championships", "NW regional Championship", "NJCAA Region X Championship",
                      "2019 Peach Belt Conference Cross Country Championship", "Justas XC LAI 2016"),
         !grepl("NJCAA", MEET)) %>%
  mutate(DATE = ifelse(grepl("-", DATE), substr(DATE, nchar(DATE)-7, nchar(DATE)), DATE)) %>%
  mutate(DATE = as.Date(DATE, '%m/%d/%y')) %>%
  filter(DATE >= as.Date("2021-08-01"))

#### 2019 ####
Sys.sleep(5)
remDr$navigate("https://www.tfrrs.org/results_search.html")
remDr$screenshot(TRUE)

webElem1 <- remDr$findElement(using = "xpath", "/html/body/div[3]/form/div/div/div[2]/div[2]/div[1]/select/option[3]")
webElem1$clickElement()

webElem2 <- remDr$findElement(using = "xpath", "/html/body/div[3]/form/div/div/div[2]/div[2]/div[4]/select/option[4]")
webElem2$clickElement()

webElem3 <- remDr$findElement(using = "xpath", "/html/body/div[3]/form/div/div/div[2]/div[3]/div/button")
webElem3$clickElement()

meetListLinkk <- remDr$getPageSource()
meetList <- read_html(meetListLinkk[[1]])

meetTableAll <- meetList %>%
  html_nodes(xpath = '//*[@id="tablesaw-8744"]') %>%
  html_table()
meetTableAll <- meetTableAll[[1]]

meetLink <- meetList %>%
  html_nodes("td:nth-child(2)") %>%
  html_node("a") %>%
  html_attr("href") %>%
  substr(3, nchar(.)-1) %>%
  paste0("https://", .)

meetTableAll$link <- meetLink

meetTable_2019 <- meetTableAll %>%
  filter(!MEET %in% c("AFXC 5K Open", "NCCAA National XC Championships", "NW regional Championship", "NJCAA Region X Championship",
                      "2019 Peach Belt Conference Cross Country Championship", "Justas XC LAI 2016"),
         !grepl("NJCAA", MEET)) %>%
  mutate(DATE = ifelse(grepl("-", DATE), substr(DATE, nchar(DATE)-7, nchar(DATE)), DATE)) %>%
  mutate(DATE = as.Date(DATE, '%m/%d/%y')) 

#### 2018 ####
Sys.sleep(5)
remDr$navigate("https://www.tfrrs.org/results_search.html")
remDr$screenshot(TRUE)

webElem1 <- remDr$findElement(using = "xpath", "/html/body/div[3]/form/div/div/div[2]/div[2]/div[1]/select/option[3]")
webElem1$clickElement()

webElem2 <- remDr$findElement(using = "xpath", "/html/body/div[3]/form/div/div/div[2]/div[2]/div[4]/select/option[5]")
webElem2$clickElement()

webElem3 <- remDr$findElement(using = "xpath", "/html/body/div[3]/form/div/div/div[2]/div[3]/div/button")
webElem3$clickElement()

meetListLinkk <- remDr$getPageSource()
meetList <- read_html(meetListLinkk[[1]])

meetTableAll <- meetList %>%
  html_nodes(xpath = '//*[@id="tablesaw-8744"]') %>%
  html_table()
meetTableAll <- meetTableAll[[1]]

meetLink <- meetList %>%
  html_nodes("td:nth-child(2)") %>%
  html_node("a") %>%
  html_attr("href") %>%
  substr(3, nchar(.)-1) %>%
  paste0("https://", .)

meetTableAll$link <- meetLink

meetTable_2018 <- meetTableAll %>%
  filter(!MEET %in% c("AFXC 5K Open", "NCCAA National XC Championships", "NW regional Championship", "NJCAA Region X Championship",
                      "2019 Peach Belt Conference Cross Country Championship", "Justas XC LAI 2016"),
         !grepl("NJCAA", MEET)) %>%
  mutate(DATE = ifelse(grepl("-", DATE), substr(DATE, nchar(DATE)-7, nchar(DATE)), DATE)) %>%
  mutate(DATE = as.Date(DATE, '%m/%d/%y')) 

#### 2017 ####
Sys.sleep(5)
remDr$navigate("https://www.tfrrs.org/results_search.html")
remDr$screenshot(TRUE)

webElem1 <- remDr$findElement(using = "xpath", "/html/body/div[3]/form/div/div/div[2]/div[2]/div[1]/select/option[3]")
webElem1$clickElement()

webElem2 <- remDr$findElement(using = "xpath", "/html/body/div[3]/form/div/div/div[2]/div[2]/div[4]/select/option[6]")
webElem2$clickElement()

webElem3 <- remDr$findElement(using = "xpath", "/html/body/div[3]/form/div/div/div[2]/div[3]/div/button")
webElem3$clickElement()

meetListLinkk <- remDr$getPageSource()
meetList <- read_html(meetListLinkk[[1]])

meetTableAll <- meetList %>%
  html_nodes(xpath = '//*[@id="tablesaw-8744"]') %>%
  html_table()
meetTableAll <- meetTableAll[[1]]

meetLink <- meetList %>%
  html_nodes("td:nth-child(2)") %>%
  html_node("a") %>%
  html_attr("href") %>%
  substr(3, nchar(.)-1) %>%
  paste0("https://", .)

meetTableAll$link <- meetLink

meetTable_2017 <- meetTableAll %>%
  filter(!MEET %in% c("AFXC 5K Open", "NCCAA National XC Championships", "NW regional Championship", "NJCAA Region X Championship",
                      "2019 Peach Belt Conference Cross Country Championship", "Justas XC LAI 2016"),
         !grepl("NJCAA", MEET)) %>%
  mutate(DATE = ifelse(grepl("-", DATE), substr(DATE, nchar(DATE)-7, nchar(DATE)), DATE)) %>%
  mutate(DATE = as.Date(DATE, '%m/%d/%y')) 

#### 2016 ####
Sys.sleep(5)
remDr$navigate("https://www.tfrrs.org/results_search.html")
remDr$screenshot(TRUE)

webElem1 <- remDr$findElement(using = "xpath", "/html/body/div[3]/form/div/div/div[2]/div[2]/div[1]/select/option[3]")
webElem1$clickElement()

webElem2 <- remDr$findElement(using = "xpath", "/html/body/div[3]/form/div/div/div[2]/div[2]/div[4]/select/option[7]")
webElem2$clickElement()

webElem3 <- remDr$findElement(using = "xpath", "/html/body/div[3]/form/div/div/div[2]/div[3]/div/button")
webElem3$clickElement()

meetListLinkk <- remDr$getPageSource()
meetList <- read_html(meetListLinkk[[1]])

meetTableAll <- meetList %>%
  html_nodes(xpath = '//*[@id="tablesaw-8744"]') %>%
  html_table()
meetTableAll <- meetTableAll[[1]]

meetLink <- meetList %>%
  html_nodes("td:nth-child(2)") %>%
  html_node("a") %>%
  html_attr("href") %>%
  substr(3, nchar(.)-1) %>%
  paste0("https://", .)

meetTableAll$link <- meetLink

meetTable_2016 <- meetTableAll %>%
  filter(!MEET %in% c("AFXC 5K Open", "NCCAA National XC Championships", "NW regional Championship", "NJCAA Region X Championship",
                      "2019 Peach Belt Conference Cross Country Championship", "Justas XC LAI 2016"),
         !grepl("NJCAA", MEET)) %>%
  mutate(DATE = ifelse(grepl("-", DATE), substr(DATE, nchar(DATE)-7, nchar(DATE)), DATE)) %>%
  mutate(DATE = as.Date(DATE, '%m/%d/%y')) 


#### 2015 ####
Sys.sleep(5)
remDr$navigate("https://www.tfrrs.org/results_search.html")
remDr$screenshot(TRUE)

webElem1 <- remDr$findElement(using = "xpath", "/html/body/div[3]/form/div/div/div[2]/div[2]/div[1]/select/option[3]")
webElem1$clickElement()

webElem2 <- remDr$findElement(using = "xpath", "/html/body/div[3]/form/div/div/div[2]/div[2]/div[4]/select/option[8]")
webElem2$clickElement()

webElem3 <- remDr$findElement(using = "xpath", "/html/body/div[3]/form/div/div/div[2]/div[3]/div/button")
webElem3$clickElement()

meetListLinkk <- remDr$getPageSource()
meetList <- read_html(meetListLinkk[[1]])

meetTableAll <- meetList %>%
  html_nodes(xpath = '//*[@id="tablesaw-8744"]') %>%
  html_table()
meetTableAll <- meetTableAll[[1]]

meetLink <- meetList %>%
  html_nodes("td:nth-child(2)") %>%
  html_node("a") %>%
  html_attr("href") %>%
  substr(3, nchar(.)-1) %>%
  paste0("https://", .)

meetTableAll$link <- meetLink

meetTable_2015 <- meetTableAll %>%
  filter(!MEET %in% c("AFXC 5K Open", "NCCAA National XC Championships", "NW regional Championship", "NJCAA Region X Championship",
                      "2019 Peach Belt Conference Cross Country Championship", "Justas XC LAI 2016"),
         !grepl("NJCAA", MEET)) %>%
  mutate(DATE = ifelse(grepl("-", DATE), substr(DATE, nchar(DATE)-7, nchar(DATE)), DATE)) %>%
  mutate(DATE = as.Date(DATE, '%m/%d/%y')) 

#### 2014 ####
Sys.sleep(5)
remDr$navigate("https://www.tfrrs.org/results_search.html")
remDr$screenshot(TRUE)

webElem1 <- remDr$findElement(using = "xpath", "/html/body/div[3]/form/div/div/div[2]/div[2]/div[1]/select/option[3]")
webElem1$clickElement()

webElem2 <- remDr$findElement(using = "xpath", "/html/body/div[3]/form/div/div/div[2]/div[2]/div[4]/select/option[9]")
webElem2$clickElement()

webElem3 <- remDr$findElement(using = "xpath", "/html/body/div[3]/form/div/div/div[2]/div[3]/div/button")
webElem3$clickElement()

meetListLinkk <- remDr$getPageSource()
meetList <- read_html(meetListLinkk[[1]])

meetTableAll <- meetList %>%
  html_nodes(xpath = '//*[@id="tablesaw-8744"]') %>%
  html_table()
meetTableAll <- meetTableAll[[1]]

meetLink <- meetList %>%
  html_nodes("td:nth-child(2)") %>%
  html_node("a") %>%
  html_attr("href") %>%
  substr(3, nchar(.)-1) %>%
  paste0("https://", .)

meetTableAll$link <- meetLink

meetTable_2014 <- meetTableAll %>%
  filter(!MEET %in% c("AFXC 5K Open", "NCCAA National XC Championships", "NW regional Championship", "NJCAA Region X Championship",
                      "2019 Peach Belt Conference Cross Country Championship", "Justas XC LAI 2016"),
         !grepl("NJCAA", MEET)) %>%
  mutate(DATE = ifelse(grepl("-", DATE), substr(DATE, nchar(DATE)-7, nchar(DATE)), DATE)) %>%
  mutate(DATE = as.Date(DATE, '%m/%d/%y')) 

#### 2013 ####
Sys.sleep(5)
remDr$navigate("https://www.tfrrs.org/results_search.html")
remDr$screenshot(TRUE)

webElem1 <- remDr$findElement(using = "xpath", "/html/body/div[3]/form/div/div/div[2]/div[2]/div[1]/select/option[3]")
webElem1$clickElement()

webElem2 <- remDr$findElement(using = "xpath", "/html/body/div[3]/form/div/div/div[2]/div[2]/div[4]/select/option[10]")
webElem2$clickElement()

webElem3 <- remDr$findElement(using = "xpath", "/html/body/div[3]/form/div/div/div[2]/div[3]/div/button")
webElem3$clickElement()

meetListLinkk <- remDr$getPageSource()
meetList <- read_html(meetListLinkk[[1]])

meetTableAll <- meetList %>%
  html_nodes(xpath = '//*[@id="tablesaw-8744"]') %>%
  html_table()
meetTableAll <- meetTableAll[[1]]

meetLink <- meetList %>%
  html_nodes("td:nth-child(2)") %>%
  html_node("a") %>%
  html_attr("href") %>%
  substr(3, nchar(.)-1) %>%
  paste0("https://", .)

meetTableAll$link <- meetLink

meetTable_2013 <- meetTableAll %>%
  filter(!MEET %in% c("AFXC 5K Open", "NCCAA National XC Championships", "NW regional Championship", "NJCAA Region X Championship",
                      "2019 Peach Belt Conference Cross Country Championship", "Justas XC LAI 2016"),
         !grepl("NJCAA", MEET)) %>%
  mutate(DATE = ifelse(grepl("-", DATE), substr(DATE, nchar(DATE)-7, nchar(DATE)), DATE)) %>%
  mutate(DATE = as.Date(DATE, '%m/%d/%y')) 

#### 2012 ####
Sys.sleep(5)
remDr$navigate("https://www.tfrrs.org/results_search.html")
remDr$screenshot(TRUE)

webElem1 <- remDr$findElement(using = "xpath", "/html/body/div[3]/form/div/div/div[2]/div[2]/div[1]/select/option[3]")
webElem1$clickElement()

webElem2 <- remDr$findElement(using = "xpath", "/html/body/div[3]/form/div/div/div[2]/div[2]/div[4]/select/option[11]")
webElem2$clickElement()

webElem3 <- remDr$findElement(using = "xpath", "/html/body/div[3]/form/div/div/div[2]/div[3]/div/button")
webElem3$clickElement()

meetListLinkk <- remDr$getPageSource()
meetList <- read_html(meetListLinkk[[1]])

meetTableAll <- meetList %>%
  html_nodes(xpath = '//*[@id="tablesaw-8744"]') %>%
  html_table()
meetTableAll <- meetTableAll[[1]]

meetLink <- meetList %>%
  html_nodes("td:nth-child(2)") %>%
  html_node("a") %>%
  html_attr("href") %>%
  substr(3, nchar(.)-1) %>%
  paste0("https://", .)

meetTableAll$link <- meetLink

meetTable_2012 <- meetTableAll %>%
  filter(!MEET %in% c("AFXC 5K Open", "NCCAA National XC Championships", "NW regional Championship", "NJCAA Region X Championship",
                      "2019 Peach Belt Conference Cross Country Championship", "Justas XC LAI 2016"),
         !grepl("NJCAA", MEET)) %>%
  mutate(DATE = ifelse(grepl("-", DATE), substr(DATE, nchar(DATE)-7, nchar(DATE)), DATE)) %>%
  mutate(DATE = as.Date(DATE, '%m/%d/%y')) 

meetTable <- rbind(meetTable_2012, meetTable_2013, meetTable_2014, meetTable_2015,
                   meetTable_2016, meetTable_2017, meetTable_2018, meetTable_2019, meetTable_2021)


### Fetch Results ####
meetResults <- data.frame("PL" = NA,
                          "NAME" = NA,
                          "YEAR" = NA,
                          "TEAM" = NA,
                          "Avg. Mile" = NA,
                          "TIME" = NA,
                          "raceNumber" = NA,
                          "distance" = NA,
                          "link" = NA,
                          "race" = NA,
                          "location" = NA)
meetResults <- meetResults[-1,]

meetResultsLink <- read_html(meetTable$link[1])
# meetResults <- meetResultsLink %>%
#   html_nodes("div") %>%
#   html_nodes("div") %>%
#   html_nodes("div")
# 
meetTitle <- meetResultsLink %>%
  html_nodes(".font-weight-500") %>%
  html_text()
meetTitle <- meetTitle[grepl(c("Individual Results"), meetTitle)]

meetLocation <- meetResultsLink %>%
  html_nodes(".inline-block:nth-child(4)") %>%
  html_text()

meetResultsAll <- meetResultsLink %>%
  html_nodes('table') %>%
  html_table()

a <- length(meetResultsAll)

df <- data.frame("number" = NA,
                 "contains_name" = NA)
df <- df[-1,]
for(i in 1:a){
  contains_NAME <- "NAME" %in% colnames(meetResultsAll[[i]])
  
  df_temp <- data.frame("number" = i,
                        "contains_name" = contains_NAME)
  
  df <- rbind(df, df_temp)
}

df <- df %>%
  filter(contains_name == TRUE)

meetResultsAll_New <- meetResultsAll[df$number]


# meetResults <- meetResultsAll[[2]] %>%
#   mutate(raceNumber = 1) %>%
#   select(PL, NAME, YEAR, TEAM, `Avg. Mile`, TIME, raceNumber) %>%
#   mutate(min = as.numeric(sapply(strsplit(`Avg. Mile`, ":"), `[`, 1))*60,
#          sec = as.numeric(substr(sapply(strsplit(`Avg. Mile`, ":"), `[`, 2), 1, 2)),
#          `Avg. Mile` = min+sec,
#          min = as.numeric(sapply(strsplit(TIME, ":"), `[`, 1))*60,
#          sec = as.numeric(substr(sapply(strsplit(TIME, ":"), `[`, 2), 1, 2)),
#          TIME = min+sec) %>%
#   select(PL, NAME, YEAR, TEAM, `Avg. Mile`, TIME, raceNumber) %>%
#   mutate(distance = TIME/`Avg. Mile`,
#          link = meetTable$link[1],
#          race = meetTitle[2],
#          location = meetLocation)

for(i in 1:(length(meetResultsAll_New))){
  n = i
  print(n)
  meetResultsTemp <- meetResultsAll_New[[i]] 
  
  if("NAME" %in% colnames(meetResultsTemp)){
    meetResultsTemp <- meetResultsTemp %>%
      mutate(raceNumber = n) %>%
      select(PL, NAME, YEAR, TEAM, `Avg. Mile`, TIME, raceNumber) %>%
      mutate(min = as.numeric(sapply(strsplit(`Avg. Mile`, ":"), `[`, 1))*60,
             sec = as.numeric(substr(sapply(strsplit(`Avg. Mile`, ":"), `[`, 2), 1, 2)),
             `Avg. Mile` = min+sec,
             min = as.numeric(sapply(strsplit(TIME, ":"), `[`, 1))*60,
             sec = as.numeric(substr(sapply(strsplit(TIME, ":"), `[`, 2), 1, 2)),
             TIME = min+sec) %>%
      select(PL, NAME, YEAR, TEAM, `Avg. Mile`, TIME, raceNumber) %>%
      mutate(distance = TIME/`Avg. Mile`,
             link = meetTable$link[1],
             race = meetTitle[n],
             location = meetLocation)
    meetResults <- rbind(meetResults, meetResultsTemp)
  } else {
    meetResultsTemp <- NA
  }
}


testing_non_scraped_base <- (meetResults %>%
  distinct(link))$link

meetTable_not_scraped <- meetTable %>%
  anti_join(
    meetResults %>%
      distinct(link),
    by = "link"
  ) %>%
  filter(DATE >= as.Date("2021-01-01"))

for(b in 1:nrow(meetTable_not_scraped)) {
  try({
  print(meetTable_not_scraped$MEET[b])
  print(b)
  ### Fetch Results ####
  meetResultsLink <- read_html(meetTable_not_scraped$link[b])
  # meetResults <- meetResultsLink %>%
  #   html_nodes("div") %>%
  #   html_nodes("div") %>%
  #   html_nodes("div")
  
  meetTitle <- meetResultsLink %>%
    html_nodes(".font-weight-500") %>%
    html_text()
  meetTitle <- meetTitle[grepl(c("Individual Results"), meetTitle)]
  
  meetLocation <- meetResultsLink %>%
    html_nodes(".inline-block:nth-child(4)") %>%
    html_text()
  
  meetResultsAll <- meetResultsLink %>%
    html_nodes('table') %>%
    html_table()
  
  a <- length(meetResultsAll)
  
  df <- data.frame("number" = NA,
                   "contains_name" = NA)
  df <- df[-1,]
  for(i in 1:a){
    contains_NAME <- "NAME" %in% colnames(meetResultsAll[[i]])
    
    df_temp <- data.frame("number" = i,
                          "contains_name" = contains_NAME)
    
    df <- rbind(df, df_temp)
  }
  
  df <- df %>%
    filter(contains_name == TRUE)
  
  meetResultsAll_New <- meetResultsAll[df$number]
  
  for(i in 1:(length(meetResultsAll_New))){
    try({  
    n = i
    print(n)
    meetResultsTemp <- meetResultsAll_New[[n]] %>%
      mutate(raceNumber = n) %>%
      select(PL, NAME, YEAR, TEAM, `Avg. Mile`, TIME, raceNumber) %>%
      mutate(min = as.numeric(sapply(strsplit(`Avg. Mile`, ":"), `[`, 1))*60,
             sec = as.numeric(substr(sapply(strsplit(`Avg. Mile`, ":"), `[`, 2), 1, 2)),
             `Avg. Mile` = min+sec,
             min = as.numeric(sapply(strsplit(TIME, ":"), `[`, 1))*60,
             sec = as.numeric(substr(sapply(strsplit(TIME, ":"), `[`, 2), 1, 2)),
             TIME = min+sec) %>%
      select(PL, NAME, YEAR, TEAM, `Avg. Mile`, TIME, raceNumber) %>%
      mutate(distance = TIME/`Avg. Mile`,
             link = meetTable_not_scraped$link[b],
             race = meetTitle[n],
             location = meetLocation)
    meetResults <- rbind(meetResults, meetResultsTemp)
    }, silent=TRUE)
  }
  Sys.sleep(.1)
  }, silent=TRUE)
}

# meetResultsLink <- read_html("https://www.tfrrs.org/results/xc/16602/Pre-National_Invitational")
# 
# meetResults <- read_csv("meet_results_2019.csv")
# 
# meetResultsAll <- meetResultsLink %>%
#   html_nodes('table') %>%
#   html_table()
# 
# meetResultsTemp <- meetResultsAll[[2]] %>%
#   mutate(raceNumber = 1) %>%
#   select(PL, NAME, YEAR, TEAM, `Avg. Mile`, TIME, raceNumber) %>%
#   mutate(min = as.numeric(sapply(strsplit(`Avg. Mile`, ":"), `[`, 1))*60,
#          sec = as.numeric(substr(sapply(strsplit(`Avg. Mile`, ":"), `[`, 2), 1, 2)),
#          `Avg. Mile` = min+sec,
#          min = as.numeric(sapply(strsplit(TIME, ":"), `[`, 1))*60,
#          sec = as.numeric(substr(sapply(strsplit(TIME, ":"), `[`, 2), 1, 2)),
#          TIME = min+sec) %>%
#   select(PL, NAME, YEAR, TEAM, `Avg. Mile`, TIME, raceNumber) %>%
#   mutate(distance = TIME/`Avg. Mile`,
#          link = meetTable$link[1])
# 
# for(i in 2){
#   n = i*2
#   print(n)
#   meetResultsTempA <- meetResultsAll[[n]] %>%
#     mutate(raceNumber = n/2) %>%
#     select(PL, NAME, YEAR, TEAM, `Avg. Mile`, TIME, raceNumber) %>%
#     mutate(min = as.numeric(sapply(strsplit(`Avg. Mile`, ":"), `[`, 1))*60,
#            sec = as.numeric(substr(sapply(strsplit(`Avg. Mile`, ":"), `[`, 2), 1, 2)),
#            `Avg. Mile` = min+sec,
#            min = as.numeric(sapply(strsplit(TIME, ":"), `[`, 1))*60,
#            sec = as.numeric(substr(sapply(strsplit(TIME, ":"), `[`, 2), 1, 2)),
#            TIME = min+sec) %>%
#     select(PL, NAME, YEAR, TEAM, `Avg. Mile`, TIME, raceNumber) %>%
#     mutate(distance = TIME/`Avg. Mile`,
#            link = meetTable$link[1])
#   meetResultsTemp <- rbind(meetResultsTemp, meetResultsTempA)
# }
# 
# for(i in 4:5){
#   n = (i*2)-1
#   print(n)
#   meetResultsTempA <- meetResultsAll[[n]] %>%
#     mutate(raceNumber = n/2) %>%
#     select(PL, NAME, YEAR, TEAM, `Avg. Mile`, TIME, raceNumber) %>%
#     mutate(min = as.numeric(sapply(strsplit(`Avg. Mile`, ":"), `[`, 1))*60,
#            sec = as.numeric(substr(sapply(strsplit(`Avg. Mile`, ":"), `[`, 2), 1, 2)),
#            `Avg. Mile` = min+sec,
#            min = as.numeric(sapply(strsplit(TIME, ":"), `[`, 1))*60,
#            sec = as.numeric(substr(sapply(strsplit(TIME, ":"), `[`, 2), 1, 2)),
#            TIME = min+sec) %>%
#     select(PL, NAME, YEAR, TEAM, `Avg. Mile`, TIME, raceNumber) %>%
#     mutate(distance = TIME/`Avg. Mile`,
#            link = meetTable$link[1])
#   meetResultsTemp <- rbind(meetResultsTemp, meetResultsTempA)
# }
# 
# meetResults <- meetResults %>%
#   filter(!link == "https://www.tfrrs.org/results/xc/16602/Pre-National_Invitational")
# meetResults <- rbind(meetResults, meetResultsTemp)

# meetResultsTemp <- meetResultsTemp %>%
#   mutate(link = "https://www.tfrrs.org/results/xc/16602/Pre-National_Invitational") %>%
#   select(-`link == "https://www.tfrrs.org/results/xc/16602/Pre-National_Invitational"`)



write_csv(meetResults, "meet_results_2012-2019.csv")
write_csv(meetTable, "meet_info_2012-2019.csv")  
