library(tidyr)
library(feasts)
library(tsibbledata)
library(dplyr)
library(ggplot2)
library(glue)
library(tidyverse)
library(tsibble)

# Read in data
hotels <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-11/hotels.csv') %>%
                 mutate(date = glue::glue("{arrival_date_year}-{arrival_date_month}-{arrival_date_day_of_month}"),
                        date = parse_date(date, format = "%Y-%B-%d")) %>% 
                 mutate(guests=children+babies+adults) %>%
                 mutate(kids=children+babies) %>%
                 mutate(has_kids=(kids>0)) %>%
                 mutate(kids_per_adult=kids/adults) %>%
                 mutate(kids_coverage=case_when(
                                   kids == 0 ~ "0, Free",
                                   kids_per_adult > 0 & kids_per_adult <= 1 ~ "0-1, One-one",
                                   kids_per_adult > 1  ~ ">1, Zone")) %>%
                 mutate(children_per_adult=children/adults) %>%
                 mutate(babies_per_adult=babies/adults) %>%
                 mutate(assigned_reserved=(reserved_room_type==assigned_room_type))

fig_by_ratio <- hotels %>% filter(!is.na(kids_coverage)) %>% 
  group_by(date,kids_coverage) %>% 
  summarize(reservations=n()) %>% 
  tsibble::as_tsibble(key = kids_coverage) %>% 
  fill_gaps(reservations=0) %>%
  group_by_key() %>% 
  mutate(reservations_ma = slide_dbl(reservations, ~ mean(., na.rm = TRUE), .size = 14)) %>%
  autoplot(reservations_ma) + theme(legend.position = "none") +
  theme_bw() + facet_grid(rows = vars(kids_coverage),scale="free") +
  xlab("Date") + ylab("Reservations (14-day MA)") +
  guides(fill = FALSE, color = FALSE, linetype = FALSE, shape = FALSE) +
  ggtitle("Peak Kids - Trends by Kid:Parent Ratio") + 
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

ggsave(filename="hotel_20200211.jpg", width=8, height=5, plot=fig_by_ratio)
