# Day 6
# https://adventofcode.com/2022/day/6
library(tidyverse)


buffer <- read_lines("Day6/data/input.txt")

checker <- function(lengthofpacket){
  for (i in lengthofpacket:nchar(buffer))
  {
    pack_cand <- substr(buffer, (i-lengthofpacket+1), i)
    pack_cand <- as.character(str_split(pack_cand, "", simplify=T))
    if (length(pack_cand)==length(unique(pack_cand))) 
      return(i)
  }  
}
answer_part1 <- checker(4)

mbRutilities::write_table(answer_part1, "Day6/output/answer_part1.txt")

#part 2

answer_part2 <- checker(14)


mbRutilities::write_table(answer_part2, "Day6/output/answer_part2.txt")



