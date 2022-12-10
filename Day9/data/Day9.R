# Day 9
# https://adventofcode.com/2022/day/9
library(tidyverse)

# Read Data
myFile <- "Day9/data/input.txt"
inst <- read_delim(myFile, delim=" ",col_names = c("dir", "num"))

#create a board sure to be large enough
size_row<-inst %>%
  filter(dir %in% c("R","L")) %>%
  summarize(n=sum(num)) %>%
  pluck(1)
size_col<-inst %>%
  filter(dir %in% c("U","D")) %>%
  summarize(n=sum(num)) %>%
  pluck(1)

visited=array(F, dim=c(size_row*2,size_col*2))
#visited=array(F, dim=c(5,6))

# initialize h and tail to center of board
start_row <- floor(size_row)
start_col <- floor(size_col)

h<-c(start_row,start_col)
t1<-c(start_row,start_col)

t2 <-t3 <- t4 <-t5 <-t6 <- t7 <- t8 <- t9 <-t1

visited[t1[1],t1[2]]<-T

# Move the head given the direction and number
moveH <- function(h, dir, num){
  if (dir=="U") h<- h + c(-1,0)
  if (dir=="D") h<- h + c(1,0)
  if (dir=="L") h<- h + c(0,-1)
  if (dir=="R") h<- h + c(0,1)
  return(h)
}

# move the tail given the head location
moveT <- function(h,t){
  dist<-h-t
  # print(dist)
  # print(paste("h", h[1], h[2], "t", t[1],t[2]))
  if(max(abs(dist))==2 ){
    
    #up/down/left/right
    if(all(dist==c(0,2))){t <- t+c(0,1)}
    if(all(dist==c(0,-2))){t <- t+c(0,-1)}
    if(all(dist==c(2,0))){t <- t+c(1,0)}
    if(all(dist==c(-2,0))){t <- t+c(-1,0)}
    
    #diagnonals
    
    if(all(dist==c(-2,-1))){t <- t+c(-1,-1)}
    if(all(dist==c(-2,1))){t <- t+c(-1,1)}
    if(all(dist==c(-1,2))){t <- t+c(-1,1)}
    if(all(dist==c(1,2))){t <- t+c(1,1)}
    if(all(dist==c(2,1))){t <- t+c(1,1)}
    if(all(dist==c(2,-1))){t <- t+c(1,-1)}
    if(all(dist==c(1,-2))){t <- t+c(1,-1)}
    if(all(dist==c(-1,-2))){t <- t+c(-1,-1)}
    
    #special diagonal
    if(all(dist==c(-2,2))){t <- t+c(-1,1)}
    if(all(dist==c(2,2))){t <- t+c(1,1)}
    if(all(dist==c(2,-2))){t <- t+c(1,-1)}
    if(all(dist==c(-2,-2))){t <- t+c(-1,-1)}
    
    
  }
  #print(paste("h", h[1], h[2], "t", t[1],t[2]))
  # mark visited
  
  return(t)
}

# Loop over all instructions
for (i in 1:nrow(inst)){
  #for (i in 1:2){
  #print(inst[i,])
  print(i)
  dir <- inst[[i,"dir"]]
  num <- inst[[i, "num"]]
  
  for(x in 1:num){
    h<-moveH(h, dir,1)
    t1<-moveT(h,t1)
    visited[t1[1],t1[2]]<-T
    
  }
  
  
}

# answer is total locations tail visited
answer_part1 <- sum(visited)
answer_part1
mbRutilities::write_table(answer_part1, "Day9/output/answer_part1.txt")

#part2\

h<-c(start_row,start_col)
t1<-c(start_row,start_col)

t2 <-t3 <- t4 <-t5 <-t6 <- t7 <- t8 <- t9 <-t1


visited=array(F, dim=c(size_row*2,size_col*2))
visited[t1[1],t1[2]]<-T


# Loop over all instructions
for (i in 1:nrow(inst)){
  #for (i in 1:8){
  print(inst[i,])
  print(i)
  dir <- inst[[i,"dir"]]
  num <- inst[[i, "num"]]
  
  for(x in 1:num){
    h<-moveH(h, dir,1)
    t1<-moveT(h,t1)
    t2<-moveT(t1,t2)
    t3<-moveT(t2,t3)
    t4<-moveT(t3,t4)
    t5<-moveT(t4,t5)
    t6<-moveT(t5,t6)
    t7<-moveT(t6,t7)
    t8<-moveT(t7,t8)
    t9<-moveT(t8,t9)
    
    visited[t9[1],t9[2]]<-T
    
    print(paste("h", h[1], h[2], "t1", t1[1],t1[2],
    "t2", t2[1],t2[2],
    "t3", t3[1],t3[2],
    "t4", t4[1],t4[2],
    "t5", t5[1],t5[2],
    "t6", t6[1],t6[2],
    "t7", t7[1],t7[2],
    "t8", t8[1],t8[2],
    "t9", t9[1],t9[2]
    ))
  }
  
  
}

# answer is total locations tail visited
answer_part2 <- sum(visited)
answer_part2
mbRutilities::write_table(answer_part2, "Day9/output/answer_part2.txt")

