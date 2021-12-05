library(readr)
library(here)
library(tidyverse)
library(RcppRoll)
library(purr)
# 5.1

inputDay5 <- read_csv(here::here("data","inputDay5.txt"),col_names = FALSE) %>%
  separate(X2, into = c("Y1","X2")) %>%
  rename(Y2 = X3) %>%
  mutate(across(where(is.character), ~ as.numeric(.)))

names(inputDay5) <- tolower(names(inputDay5))

horVert <- inputDay5 %>%
  filter(x1 == x2 | y1 == y2)


hits <- matrix(data=0, nrow = max(inputDay5),ncol = max(inputDay5))
vertHits <- horVert %>%
  filter(y1 == y2) %>%
  mutate(smallx = pmin(x1,x2),
         bigx = pmax(x1,x2))
i <- 1
for(y in vertHits$y2){
  newHits <- seq(vertHits$smallx[i],vertHits$bigx[i],1)
  hits[y,newHits] <- hits[y,newHits]+1
  i <- i+1
}

horHits <- horVert %>%
  filter(x1 == x2) %>%
  mutate(smally = pmin(y1,y2),
         bigy = pmax(y1,y2))
i <- 1
for(x in horHits$x2){
  newHits <- seq(horHits$smally[i],horHits$bigy[i],1)
  hits[newHits,x] <- hits[newHits,x]+1
  i <- i+1
}

sum(hits > 1)




#5.2
allHits <- inputDay5 %>%
  mutate(smallx = pmin(x1,x2),
         bigx = pmax(x1,x2),
         smally = pmin(y1,y2),
         bigy = pmax(y1,y2),
         case = case_when(x1 == x2 ~ "H",
                          y1 == y2 ~ "V",
                          TRUE ~ "D"))


hits <- matrix(data=0, nrow = max(inputDay5),ncol = max(inputDay5))


for(i in seq(1,500)){
  df <- allHits[i,]
  xRep <- rep(df$x1,abs(df$y1 - df$y2))
  xSeq <- seq(df$x1,df$x2)
  yRep <- rep(df$y1,abs(df$x1 - df$x2))
  ySeq <-seq(df$y1,df$y2)
  if(df$case == "H") {hits[xRep,ySeq] <- hits[xRep,ySeq]+1}
  if(df$case == "V") {hits[xSeq,yRep] <- hits[xSeq,yRep]+1}
  if(df$case == "D") {
    for(j in seq(1,length(xSeq))){
      hits[xSeq[j],ySeq[j]] <- hits[xSeq[j],ySeq[j]]+1
    }
  }
  
}

sum(hits > 1)
