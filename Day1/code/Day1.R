# Day 1
# https://adventofcode.com/2022/day/1
library(tidyverse)

file_str <- readLines("Day1/data/input.txt")

counter=1
counterv=as.numeric()
for (i in 1:length(file_str)){
  if (file_str[i]==""){
    counter=counter+1
  }
  counterv[i]<-counter
}
df <- tibble(calories=as.numeric(file_str), counter=counterv) %>%
  filter(calories!="") %>%
  group_by(counter) %>%
  summarize(totalcalories=sum(calories))

answer_part1 <- max(df$totalcalories)

write.table(answer_part1, file="Day1/output/answer_part1.txt", row.names = F, col.name=F)

# Part 2
answer_part2 <- df %>%
  arrange(desc(totalcalories)) %>%
  filter(row_number()<=3) %>%
  summarize(totalcalories=sum(totalcalories))

write.table(answer_part2, file="Day1/output/answer_part2.txt", row.names = F, col.names = F)
