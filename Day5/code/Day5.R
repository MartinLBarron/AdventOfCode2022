# Day 5
# https://adventofcode.com/2022/day/5
library(tidyverse)

#Parse the two types of data
myfile <- "Day5/data/input.txt"

#determine where blank is
x <- read_lines(myfile)
blankline <- which(x=="")

#determine number of stacks
numstacks <-as.integer(max(str_split(read_lines(myfile, skip = blankline-2, n_max = 1), " ", simplify = T)))

#read crates
cratesdf <- read_fwf(myfile, 
                     fwf_widths(rep(4,numstacks)),
                     n_max = blankline-2) 

# remove brackets
cratesdf <-cratesdf %>%
  map_df(~str_replace_all(.x, "\\[|\\]", ""))

#convert to vectors
crates <- list()
for (i in 1:numstacks){
  crates[[i]] <- rev(cratesdf[[i]][!is.na(cratesdf[[i]])])
}

# Read moves;
moves <- read_delim(myfile,
                    delim=" ",
                    skip = blankline,
                    col_names = c("x1", "numcrates", "x2", "from", "x3","to")) %>%
  select(-contains("x"))

# pop helper function
pop <- function(stack_no){
  stack <- crates[[stack_no]]
  stack <- stack[1:length(stack)-1]
}

# push helper function
push <- function(stack_no, crate){
  stack <- crates[[stack_no]]
  stack <- c(stack, crate)
}

# Main instruction loop

#for each instruction
for (j in 1:nrow(moves)){
  #for each number of crates to move
  numcrates <- moves[[j,"numcrates"]]
  from <- moves[[j, "from"]]
  to <- moves[[j, "to"]]
  for (i in 1:numcrates){
    # get stack to be moved
    crate <- crates[[from]]
    crate <- crate[[length(crate)]]

    # pop stack off original stack
    crates[[from]] <- pop(from)

    # push stack onto new stack
    crates[[to]] <- push(to, crate)
  }
}

answer_part1<-as.character()
for (i in 1:numstacks){
  temp <- crates[[i]]
  answer_part1 <-c(answer_part1, temp[[length(temp)]])
}
answer_part1<-paste(answer_part1, collapse = "")

write.table(answer_part1, "Day5/output/answer_part1.txt", row.names = F, col.names = F)


#part 2

#convert to vectors
crates <- list()
for (i in 1:numstacks){
  crates[[i]] <- rev(cratesdf[[i]][!is.na(cratesdf[[i]])])
}


# pop helper function
pop <- function(stack_no, numcrates){
  stack <- crates[[stack_no]] 
  endloc <- length(stack)-numcrates
  stack <- stack[1:endloc]
}

# push helper function
push <- function(stack_no, crate){
  stack <- crates[[stack_no]]
  stack <- c(stack, crate)
}

# Main instruction loop

#for each instruction
for (j in 1:nrow(moves)){
  print(j)
  #for each number of crates to move
  numcrates <- moves[[j,"numcrates"]]
  from <- moves[[j, "from"]]
  to <- moves[[j, "to"]]
  
  # get stack to be moved
  crate <- crates[[from]]
  if (numcrates==1) {
    crate <- crate[length(crate)]
  } else {
    startloc <-length(crate)-numcrates+1
    crate <- crate[startloc:length(crate)]
  }
  print(paste("crate:", crate))
  # pop stack off original stack
  crates[[from]] <- pop(from, numcrates)
  
  # push stack onto new stack
  crates[[to]] <- push(to, crate)
}


answer_part2<-as.character()
for (i in 1:numstacks){
  temp <- crates[[i]]
  answer_part2 <-c(answer_part2, temp[[length(temp)]])
}
answer_part2<-paste(answer_part2, collapse = "")

write.table(answer_part2, "Day5/output/answer_part2.txt", row.names = F, col.names = F)


max(moves$numcrates)



