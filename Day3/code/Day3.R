# Day 3
# https://adventofcode.com/2022/day/3
library(tidyverse)

letterScore <- c(letters[1:26], LETTERS[1:26])

uchar <- function(x) unique(strsplit(x, "")[[1]]) 

findCommonLetters <- function(string1, string2){
  matches <- str_extract_all(string1, paste0("[", string2, "]"))
}

df <- read_delim("Day3/data/input.txt", delim=" ", col_names=F) %>%
  rename(sack_contents=X1) %>%
  mutate(sack1=str_sub(sack_contents,1, str_length(sack_contents)/2),
         sack2=str_sub(sack_contents,str_length(sack_contents)/2+1,str_length(sack_contents)),
         common_item = map_chr(findCommonLetters(sack1, sack2) ,uchar),
         common_score=match(common_item, letterScore)
  )

answer <- sum(df$common_score)

write.table(answer, "Day3/output/answer_part1.txt", row.names = F, col.names = F)

#Part 2
# Collapse groups into rows;
df <- df %>%
  select(sack_contents) %>%
  mutate(group=ceiling(row_number() / 3)) %>%
  group_by(group) %>%
  mutate(sack_no=row_number()) %>%
  ungroup() %>%
  pivot_wider(values_from=sack_contents, names_from=sack_no, names_prefix = "sack") 

df2 <- df %>%
  mutate(common_1_2=map_chr(findCommonLetters(sack1, sack2), ~str_c(.x, collapse = "")),
         common_2_3=map_chr(findCommonLetters(sack2, sack3), ~str_c(.x, collapse = "")),
         common_all=map_chr(findCommonLetters(common_1_2, common_2_3), ~str_c(.x, collapse = "")),
         common_all1=map_chr(common_all, uchar),
         common_score=match(common_all1, letterScore)
  )

answer <- sum(df2$common_score)

write.table(answer, "Day3/output/answer_part2.txt", row.names = F, col.names = F)

   



