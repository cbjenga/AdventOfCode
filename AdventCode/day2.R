library(readr)
library(here)
library(dplyr)
library(RcppRoll)

# 2.1
input <- read_table2(here::here("data","inputDay2.txt"),col_names = FALSE) %>%
  rename(direction = X1,
         value = X2) %>%
  mutate(sign = ifelse(direction == "up",-1,1),
         value = value*sign,
         hozDep = ifelse(direction == "forward","H","D")) %>%
  group_by(hozDep) %>%
  summarize(value = sum(value))




input$value[1]*input$value[2]

# 2.2
input <- read_table2(here::here("data","inputDay2.txt"),col_names = FALSE) %>%
  rename(direction = X1,
         value = X2) %>%
  mutate(aim = 0,
         hPos = 0,
         dPos = 0) %>%
  mutate(sign = ifelse(direction == "up",-1,1),
         value = value*sign,
         aimDelta = ifelse(direction != "forward",value,0),
         aim2 = cumsum(aimDelta),
         hPosDelta = ifelse(direction == "forward",value,0),
         dPosDelta = ifelse(direction == "forward",value*aim2,0),
         hozDep = ifelse(direction == "forward","H","D")) %>%
  ungroup() %>%
  summarize(hPosDelta = sum(hPosDelta),
            dPosDelta = sum(dPosDelta)) 






input$hPosDelta*input$dPosDelta



