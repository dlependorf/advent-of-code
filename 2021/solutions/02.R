########################################################################################################################
# Advent of Code 2021: Day 2                                                                                           #
#                                                                                                                      #
# Author: Dan Lependorf                                                                                                #
# Date: 2021-12-02                                                                                                     #
########################################################################################################################

library(tidyverse)

data <- read_delim(file="./2021/inputs/02.txt", delim=" ",
                   col_names=c("direction", "magnitude"))

########################################################################################################################
# Part One                                                                                                             #
########################################################################################################################

# The order doesn't actually matter here, since nothing here relies on a running total of anything. This means I can
# just sum everything up.
part_one_totals <- data %>%
    group_by(direction) %>%
    summarize(magnitude=sum(magnitude)) %>%
    deframe()

part_one_totals[["forward"]] * (part_one_totals[["down"]] - part_one_totals[["up"]])

########################################################################################################################
# Part Two                                                                                                             #
########################################################################################################################

# Hah, well okay. Looks like I'll need to loop through each instruction after all.
position <- 0
depth <- 0
aim <- 0

for (i in seq_len(nrow(data))) {
    if (data$direction[[i]]=="down") {
        aim <- aim + data$magnitude[[i]]
    }
    
    if (data$direction[[i]]=="up") {
        aim <- aim - data$magnitude[[i]]
    }
    
    if (data$direction[[i]]=="forward") {
        position <- position + data$magnitude[[i]]
        depth <- depth + (aim * data$magnitude[[i]])
    }
}

position * depth
