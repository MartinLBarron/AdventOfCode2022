# Day 7
# https://adventofcode.com/2022/day/7
library(tidyverse)
# From https://twitter.com/drob/status/1600360338498764800

input <- data.frame(v1=readLines("Day7/data/input.txt"))


# Given a path like c("/""/a/""/a/b"), and dir"c"
# return c("/"."/a"."/a/b"."/a/b/c")- that is, every folder it's in
cd <- function(path, dir) {
  if (!is.na(dir)){
    if (dir == "..") {
      return(head(path, -1))
    }
    return(c(path, paste0(tail(path, 1), "/", dir)))
  }
  return(path)
}
# Keep track of the cd lines
dir_sizes <- input %>%
  extract(v1,"cd_dir","cd (.*)",remove= FALSE)%>%
  mutate(path = (accumulate(cd_dir, cd))) %>%
  unnest(path) %>%
  filter(str_detect(v1,"\\d")) %>%
  separate(v1, c("size", "file"), sep=" ", convert=TRUE) %>%
  group_by(path) %>%
  summarize (size= sum (size))

# Part 1
answer_part1 <- dir_sizes %>%
  filter (size< 100000) %>%
  summarize(sum(size)) %>%
  as.integer()

mbRutilities::write_table(answer_part1, "Day7/output/answer_part1.txt")

answer_part2 <- dir_sizes %>%
  filter (size > (dir_sizes[[1,"size"]] + 30000000 - 70000000)) %>%
  summarize(min(size)) %>%
  as.integer()


mbRutilities::write_table(answer_part2, "Day7/output/answer_part2.txt")


