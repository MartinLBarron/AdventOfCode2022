# Day 10
# https://adventofcode.com/2022/day/10
library(tidyverse)
myFile <- "Day10/data/input.txt"

# read data
inst <- read_lines(myFile) %>%
  str_split(.," ", simplify=T) %>%
  data.frame()
names(inst) <- c("inst", "num")

inst <- inst %>%
  mutate(numCycles=ifelse(inst=="addx",2,1),
         num=as.integer(num)) 

# create dataframe to hold cycles

numcycles<-sum(inst$numCycles)
cycles<-data.frame(x=rep(NA,numcycles+1),
                   inst=rep("",numcycles+1),
                   num=rep(NA,numcycles+1))
cycles[1,"x"]<-1

myCycles<-c(20, 60, 100, 140, 180, 220)

#loop over instructions
i=3
cycle<-2
for (i in 1:nrow(inst)){
  
  ins <-inst[[i,"inst"]]
  num <- inst[[i, "num"]]
  
  if (ins=="addx"){
    cycles[cycle,"x"] <- cycles[[cycle-1,"x"]]
    cycles[cycle,"inst"]<-ins
    cycles[cycle,"num"]<-num
    cycle <- cycle+1
    cycles[cycle, "x"] <- cycles[[cycle-1,"x"]] + inst[[i,"num"]]
    cycles[cycle,"inst"]<-ins
    cycles[cycle,"num"]<-num
    cycle <- cycle+1
    
  } else{
    cycles[cycle,"x"] <- cycles[[cycle-1,"x"]]
    cycles[cycle,"inst"]<-ins
    cycles[cycle,"num"]<-num
    cycle <- cycle+1
  }
}

answer_part1 <- cycles %>%
  mutate(rown =row_number(),
         score=rown*x) %>%
  filter(row_number() %in% myCycles) %>%
  summarize(n=sum(score)) %>%
  pluck(1)

answer_part1
mbRutilities::write_table(answer_part1, "Day10/output/answer_part1.txt")



#part2 
# part 2 gave me a hell of a time.  I finally borrowed from mastadon to fix
crt <- matrix(" ", 6,40)
sprite<- c(1,2,3)
k<-1
for (j in 1:6){
  for (i in 1:40){
    print(paste(j,i))
    print(sprite)
    if (i %in% sprite){
      print("$")
      crt[j,i] <- "#"
    }
    k <- k+1
    sprite <- c(cycles[k,"x"],cycles[k,"x"]+1,cycles[k,"x"]+2)
  }
}
apply(crt,1,paste, collapse=".")