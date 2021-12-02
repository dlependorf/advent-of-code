########################################################################################################################
# Advent of Code 2021: Day 1                                                                                           #
#                                                                                                                      #
# Author: Dan Lependorf                                                                                                #
# Date: 2021-12-01                                                                                                     #
########################################################################################################################

library(tidyverse)

data <- read_table(file="./2021/inputs/01.txt", col_names="depths")

########################################################################################################################
# Part One                                                                                                             #
########################################################################################################################

# I can get by with just a simple dplyr::lag() call, and then count the number of rows where the current value is
# greater than the last one.
data %>%
    mutate(last_value=lag(depths, n=1),
           increased=if_else(last_value > depths, 0, 1)) %>%
    pull(increased) %>%
    sum(na.rm=TRUE)

########################################################################################################################
# Part Two                                                                                                             #
########################################################################################################################

# Three lag() calls, and it's the same thing.
data %>%
    mutate(first_window=lag(depths, n=1)+lag(depths, n=2)+lag(depths, n=3),
           second_window=depths+lag(depths, n=1)+lag(depths, n=2),
           window_increased=if_else(second_window > first_window, 1, 0)) %>%
    pull(window_increased) %>%
    sum(na.rm=TRUE)
