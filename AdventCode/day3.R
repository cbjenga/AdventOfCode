library(readr)
library(here)
library(tidyverse)
library(RcppRoll)

# 3.1
input <- read_table2(here::here("data","inputDay3.txt"),col_names = FALSE) %>%
  separate(X1,paste0("d",seq(1,12,1)),sep = seq(1,12,1)) %>%
  summarize(across(d1:d12, ~ sum(as.numeric(.))))


idOut <- function(rate = "gamma",sumVal){
  return(case_when(rate=="gamma" ~ as.numeric(sumVal > 500),
                   rate=="epsilon" ~ as.numeric(sumVal < 500))
         )
}

gamma <- paste0(
  idOut("gamma",input$d1),
  idOut("gamma",input$d2),
  idOut("gamma",input$d3),
  idOut("gamma",input$d4),
  idOut("gamma",input$d5),
  idOut("gamma",input$d6),
  idOut("gamma",input$d7),
  idOut("gamma",input$d8),
  idOut("gamma",input$d9),
  idOut("gamma",input$d10),
  idOut("gamma",input$d11),
  idOut("gamma",input$d12)
)
epsilon <- paste0(
  idOut("epsilon",input$d1),
  idOut("epsilon",input$d2),
  idOut("epsilon",input$d3),
  idOut("epsilon",input$d4),
  idOut("epsilon",input$d5),
  idOut("epsilon",input$d6),
  idOut("epsilon",input$d7),
  idOut("epsilon",input$d8),
  idOut("epsilon",input$d9),
  idOut("epsilon",input$d10),
  idOut("epsilon",input$d11),
  idOut("epsilon",input$d12)
)


strtoi(gamma, base = 2) * strtoi(epsilon, base = 2)



#3.2

input <- read_table2(here::here("data","inputDay3.txt"),col_names = FALSE) %>%
  separate(X1,paste0("d",seq(1,12,1)),sep = seq(1,12,1)) 

workingData <- input



idOut2 <- function(rate = "gamma",sumVal,maxVal){
#  cat(str(sumVal),str(rate))
   return(case_when(rate=="gamma" ~ as.numeric(sumVal >= (maxVal / 2)),
                    rate=="epsilon" ~ as.numeric(sumVal < (maxVal / 2)),
                    TRUE ~ -50)
   )
}


i <- 1
for(i in seq(1,12)){
  
  newNum <- workingData %>%
    summarize(across(d1:d12, ~ sum(as.numeric(.))))
  
  s <- as.numeric(newNum[,i])
  d <- dim(workingData)[1]
  
  newG <- idOut2(rate = "gamma",sumVal = s, maxVal = d)
  
  
  keepList = workingData[,i] == as.character(newG)
  workingData <- workingData[keepList,]
  if(dim(workingData)[1] == 1) break
}
o2 <- paste0(workingData,collapse = "")

workingData <- input

for(i in seq(1,12)){
  
  newNum <- workingData %>%
    summarize(across(d1:d12, ~ sum(as.numeric(.))))
  
  s <- as.numeric(newNum[,i])
  d <- dim(workingData)[1]
  
  newG <- idOut2(rate = "epsilon",sumVal = s, maxVal = d)
  
  
  keepList = workingData[,i] == as.character(newG)
  workingData <- workingData[keepList,]
  if(dim(workingData)[1] == 1) break
}
co2 <- paste0(workingData,collapse = "")

strtoi(o2, base = 2) * strtoi(co2, base = 2)


