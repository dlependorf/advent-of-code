########################################################################################################################
# Advent of Code 2021: Day 6                                                                                           #
#                                                                                                                      #
# Author: Dan Lependorf                                                                                                #
# Date: 2021-12-06                                                                                                     #
########################################################################################################################

library(tidyverse)

data <- readLines("./2021/inputs/06.txt") %>%
    str_split(",") %>%
    unlist() %>%
    as.integer()

########################################################################################################################
# Part One                                                                                                             #
########################################################################################################################

# Easy, loop over 80 iterations. For each one, take advantage of R's vectorized subtraction, find out how many are
# negative, remove them, then add back in one 6 and one 8 for every removed fish.
i <- 1
fishes <- data
for (i in 1:80) {
    fishes <- fishes - 1
    
    num_turnover_fish <- length(fishes[fishes < 0])
    
    fishes <- c(fishes[fishes >= 0], rep.int(6, num_turnover_fish), rep.int(8, num_turnover_fish))
}

length(fishes)

########################################################################################################################
# Part Two                                                                                                             #
########################################################################################################################

# Heh, first thought: wait, all I have to do is change the loop from 1:80 to 1:256! Second thought upon trying it: ahhh.
part_two_fishes <- data %>%
    tibble(fish_value=.) %>%
    count(fish_value, name="num")

# Similar, just that now I'm not holding a vector element for every fish, but I'm updating a count of fishes with each
# value.
i <- 1
for (i in 1:256) {
    part_two_fishes <- part_two_fishes %>%
        mutate(fish_value=fish_value-1)
    
    num_turnover_fish <- part_two_fishes %>%
        filter(fish_value==-1) %>%
        pull(num) %>%
        {ifelse(length(.)==0, 0L, .)}
    
    part_two_fishes <- tibble(fish_value=c(6, 8),
                              num=num_turnover_fish) %>%
        bind_rows(part_two_fishes, .) %>%
        filter(fish_value >= 0) %>%
        group_by(fish_value) %>%
        summarize(num=sum(num))
        
}

# lol
options(scipen=999)

sum(part_two_fishes$num)
