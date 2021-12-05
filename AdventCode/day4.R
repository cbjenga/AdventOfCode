library(readr)
library(here)
library(tidyverse)
library(RcppRoll)

# 4.1

inputDay4 <- read.csv(here::here("data","inputDay4.txt"), comment.char="#")

numbers <- gsub("X","",names(inputDay4))  %>% as.numeric()


boards <- inputDay4 %>%
  select(X17) %>%
  mutate(X17 = gsub("  "," ",X17)) %>%
  mutate(X17 = gsub("^\\s*","",X17)) %>%
  separate(X17,into = c("A","B","C","D","E"))



i <- 1
append(pased_boards,boards[i:(i+4),]  %>% unlist() %>% as.numeric()%>% matrix(nrow = 5))


pased_boards <- list()
for(i in seq(1,496,5)){
  
  pased_boards <- append(pased_boards,list(boards[i:(i+4),]  %>% unlist() %>% as.numeric()%>% matrix(nrow = 5)))
  
  
}


boardState <- board %in% numbers[1:75] %>% as.numeric() %>% matrix(nrow = 5)
winnginBoard <- any(colSums(boardState) == 5,rowSums(boardState) == 5)

isWinner <- function(drawn,board){
  boardState <- board %in% drawn %>% as.numeric() %>% matrix(nrow = 5)
  return(any(colSums(boardState) == 5,rowSums(boardState) == 5))
}

findWinNum <- function(board){
  for(i in seq(5,100,1)) if(isWinner(numbers[1:i],board)) return(i)
}

findWinNum(board)

boards <- list(board,board+1)

boardResults = lapply(pased_boards,findWinNum)

winBoard <- pased_boards[[which.min(boardResults)]]
calledNum <- numbers[29]
winBoardF <- winBoard %in% numbers[1:29] %>% matrix(nrow = 5)
sumMissing <- winBoard[!winBoardF] %>% sum()

result <- calledNum * sumMissing

result


#4.2
loseBoard <- pased_boards[[which.max(boardResults)]]
calledNum <- numbers[88]
loseBoardF <- loseBoard %in% numbers[1:88] %>% matrix(nrow = 5)
sumMissing <- loseBoard[!loseBoardF] %>% sum()

result <- calledNum * sumMissing

result


