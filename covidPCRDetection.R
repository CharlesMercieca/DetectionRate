library(tidyverse)
library(ggplot2)
library(lubridate)

#Read tests data, use both PCR and rapid antigen as 'total tests'
tests <- read_csv("https://raw.githubusercontent.com/COVID19-Malta/COVID19-Data/master/COVID-19%20Malta%20-%20COVID%20Tests.csv") %>% 
  mutate(date = lubridate::dmy(`Publication date`)) %>% 
  rename(total_tests = "NAA and rapid antigen tests in previous day")

#Read case data and join tests
cases <- read_csv("https://raw.githubusercontent.com/COVID19-Malta/COVID19-Cases/master/COVID-19%20Malta%20-%20Aggregate%20Data%20Set.csv") %>% 
  mutate(Date = lubridate::dmy(Date)) %>% 
  janitor::clean_names() %>%
  left_join(tests, by = c('date' = 'date')) %>% 
  mutate(detection_rate = new_cases/total_tests,
         last_week = if_else(date >= Sys.Date() - 7, "Last Week", ""))

#Plot
cases %>% 
  filter(!is.na(detection_rate)) %>% 
ggplot(aes(x = date, y = detection_rate))+
  geom_point(color = 'grey80')+
  geom_point(data = tail(cases, 7), aes(x = date, y = detection_rate), col = "black")+
  scale_y_continuous(labels = scales::percent)+
  geom_line(stat="smooth", alpha = 0.5, size = 1.5)+
  labs(title = "COVID19 Detection Rate as a % of Total Tests",
       subtitle = "New Cases / PCR + Antigen tests.",
       x = "Date", y = "PCR Detection Rate")+
  theme_bw()
