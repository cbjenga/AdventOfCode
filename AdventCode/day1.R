library(readr)
library(here)
library(dplyr)
library(RcppRoll)
inputDay1 <- read_csv(here::here("data","inputDary1.txt"),col_names = FALSE) 

listValues <- inputDay1 %>% .$X1 %>% c()

positives <- diff(listValues) %>% sign() > 0
sum(positives)


rollValues <- inputDay1 %>%
  mutate(roll_sum = roll_sum(X1, 3, align = "right", fill = NA)) %>%
  filter(!is.na(roll_sum)) %>% 
  .$roll_sum %>% c()

positives2 <- diff(rollValues) %>% sign() > 0
sum(positives2)
