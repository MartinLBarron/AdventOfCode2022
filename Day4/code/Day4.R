# Day 4
# https://adventofcode.com/2022/day/4
library(tidyverse)

# We first read the data and clean so that each start and end is a seperate
# variable name. We also calculate the max length of sequence for later
# comparison to union of sequences.

df <- read_csv("Day4/data/input.txt", col_names = F) %>%
  separate(X1, into = c("s1","e1")) %>%
  separate(X2, into = c("s2", "e2")) %>%
  mutate(maxl =pmax(
    as.numeric(e1)-as.numeric(s1)+1,
    as.numeric(e2)-as.numeric(s2)+1))

# Create sequences and add to dataframe
seq1 <- map2(df$s1,df$e1, seq) 
seq2 <- map2(df$s2,df$e2, seq) 


#Calculate union of two sequences
unionlength <- function(seq1, seq2) {
  check <- union(unlist(seq1),unlist(seq2))
  check2 <- length(check)
}

#compare length of union to original max length.  If union length is longer than
#can't contain
df<-df %>%
  mutate(unionl=map2_dbl(seq1, seq2, unionlength),
         contains=ifelse(unionl>maxl,0,1))

answer_part1 <- sum(df$contains)

write.table(answer_part1, "Day4/output/answer_part1.txt", row.names = F, col.names = F)

# Part 2
#Calculate union of two sequences
intersectlength <- function(seq1, seq2) {
  check <- intersect(unlist(seq1),unlist(seq2))
  check2 <- length(check)
}

#compare length of union to original max length.  If union length is longer than
#can't contain
df<-df %>%
  mutate(inters=map2_dbl(seq1, seq2, intersectlength),
         intersect=ifelse(inters>0,1,0))

answer_part2 <- sum(df$intersect)

write.table(answer_part2, "Day4/output/answer_part2.txt", row.names = F, col.names = F)

