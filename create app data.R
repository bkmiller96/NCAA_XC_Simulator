nationals <- read_csv("nationals_simulation.csv") %>%
  select(NAME, TEAM, season)
regionals <- read_csv("regional_simulation.csv") %>%
  select(NAME, TEAM, season)
mac <- read_csv("mac_simulation.csv") %>%
  select(NAME, TEAM, season)

combined_meets <- rbind(nationals, regionals, mac)

mens <- read_csv("mens_data.csv")
womens <- read_csv("womens_data.csv")

combined <- rbind(mens, womens) %>%
  inner_join(combined_meets, by = c("NAME", "TEAM", "season")) %>%
  group_by() %>%
  distinct() %>%
  ungroup()

write_csv(combined, "app_data.csv")
