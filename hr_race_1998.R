library(tidyverse)
library(Lahman)
library(lubridate)

fields <- read_csv("data/fields.csv")
data1998 <- read_csv("data/all1998.csv",
                     col_names = pull(fields, Header))

sosa_id <- Master %>%
  filter(nameFirst == "Sammy", nameLast == "Sosa") %>%
  pull(retroID)

mac_id <- Master %>%
  filter(nameFirst == "Mark", nameLast == "McGwire") %>%
  pull(retroID)

hr_race <- data1998 %>%
  filter(BAT_ID %in% c(sosa_id, mac_id))

cum_hr <- function(d) {
  d %>%
    mutate(Date = ymd(str_sub(GAME_ID, 4, 11))) %>%
    arrange(Date) %>%
    mutate(HR = ifelse(EVENT_CD == 23, 1, 0),
           cumHR = cumsum(HR)) %>%
    select(Date, cumHR)
}

hr_ytd <- hr_race %>%
  split(pull(., BAT_ID)) %>%
  map_df(cum_hr, .id = "BAT_ID") %>%
  inner_join(Master, by = c("BAT_ID" = "retroID"))

ggplot(hr_ytd, aes(Date, cumHR, linetype = nameLast)) +
  geom_line() +
  geom_hline(yintercept = 62, color = "blue") +
  annotate("text", ymd("1998-04-15"), 65,
           label = "62", color = "blue") +
  ylab("Home Runs in the Season")

