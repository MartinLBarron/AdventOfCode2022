# Day 8
# https://adventofcode.com/2022/day/8
library(tidyverse)
myFile<- "Day8/data/input.txt"
line_length <- nchar(read_lines(myFile, n_max=1))
forest <- read_fwf(myFile,
                   col_positions = fwf_widths(rep(1,line_length)))
forest<-as.matrix(forest)

visible<-array(data=F, dim=c(nrow(forest)-2, ncol(forest)-2))
for (row in 2:(nrow(forest)-1)){
  for (col in 2:(ncol(forest)-1)){
    tree <- forest[row,col]
    print(paste(row,col,":", tree))
    tree_up<-forest[1:(row-1),col]
    tree_left <- forest[row,1:(col-1)]
    tree_down <- forest[(row+1):nrow(forest), col]
    tree_right <- forest[row, (col+1):ncol(forest)]
    
    if(tree>max(tree_up)|tree>max(tree_left)|tree>max(tree_down)|tree>max(tree_right)){
      print(paste(row,col))
      visible[row-1, col-1]<-TRUE
    }
  }
}
answer_part1 <-(nrow(forest)*2+ncol(forest)*2)-4 + sum(visible)
answer_part1
mbRutilities::write_table(answer_part1, "Day8/output/answer_part1.txt")

visup <- array(data=0, dim=c(nrow(forest), ncol(forest)))
visdown <-array(data=0, dim=c(nrow(forest), ncol(forest)))
visleft <- array(data=0, dim=c(nrow(forest), ncol(forest)))
visright<-array(data=0, dim=c(nrow(forest), ncol(forest)))



for (row in 1:(nrow(forest))){
  for (col in 1:(ncol(forest))){
    # for (row in 4:4){
    #   for (col in 3:3){
    tree <- forest[row,col]
    print(paste(row,col,":", tree, "comp",visleft[2,3]))
    
    # up
    for(x in (row-1):1){
      print(paste("x",x))
      if(x>0){
        compare_tree <- forest[x,col]
        print(paste("up", compare_tree, "tree", tree, visup[row,col]))
        if (compare_tree<tree){
          visup[row,col]<-visup[row,col]+1
          print("here")
        }else if(compare_tree>=tree){
          visup[row,col]<-visup[row,col]+1
          break
        }
        else{
          break
        }
      }
    }
    
    
    #left
    for(y in (col-1):1){
      if(y>0){
        compare_tree <- forest[row,y]
        print(paste("left", compare_tree, "tree", tree))
        if (compare_tree<tree){
          visleft[row,col]<-visleft[row,col]+1
        }else if(compare_tree>=tree){
          visleft[row,col]<-visleft[row,col]+1
          break
        }
        else{
          break
        }
      }
    }
    
    
    # down
    for(x in (row+1):nrow(forest)){
      if(x<=nrow(forest)){
        compare_tree <- forest[x,col]
        if (compare_tree<tree){
          visdown[row,col]<-visdown[row,col]+1
        }else if(compare_tree>=tree){
          visdown[row,col]<-visdown[row,col]+1
          break
        }
        else{
          break
        }
      }
    }
    
    #right
    for(y in (col+1):ncol(forest)){
      print(paste("y", y))
      if(y<=ncol(forest)){
        compare_tree <- forest[row,y]
        print(paste("rigth", compare_tree, "tree", tree, visright[row,col]))
        
        if (compare_tree<tree){
          visright[row,col]<-visright[row,col]+1
        }else if(compare_tree>=tree){
          visright[row,col]<-visright[row,col]+1
          break
        }
        else{
          break
        }
      }
    }
  }
}


scenicScore <- visleft*visup*visright*visdown

answer_part2 <-max(scenicScore)
answer_part2
mbRutilities::write_table(answer_part2, "Day8/output/answer_part2.txt")
