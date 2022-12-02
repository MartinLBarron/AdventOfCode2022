# Day 2
# https://adventofcode.com/2022/day/2
library(tidyverse)

df <- read_delim("Day2/data/input.txt", col_names=F, delim=" ") %>%
  mutate(opponent=fct_recode(X1,"Rock" = "A",
                             "Paper" = "B",
                             "Scissors" = "C"),
         self = fct_recode(X2, "Rock" = "X",
                           "Paper" = "Y",
                           "Scissors" = "Z"),
         score_pick=case_when(self == "Rock" ~ 1,
                              self == "Paper" ~ 2,
                              self == "Scissors" ~3),
         score_game=case_when(opponent=="Rock" & self=="Rock" ~ 3,
                              opponent=="Rock" & self=="Paper" ~ 6,
                              opponent=="Rock" & self=="Scissors" ~ 0,
                              opponent=="Paper" & self=="Rock" ~ 0,
                              opponent=="Paper" & self=="Paper" ~ 3,
                              opponent=="Paper" & self=="Scissors" ~ 6,
                              opponent=="Scissors" & self=="Rock" ~ 6,
                              opponent=="Scissors" & self=="Paper" ~ 0,
                              opponent=="Scissors" & self=="Scissors" ~ 3),
         score_total=score_pick+score_game)

answer_part1 <- sum(df$score_total)

write.table(answer_part1, "Day2/output/answer_part1.txt", row.names = F, col.names = F)





