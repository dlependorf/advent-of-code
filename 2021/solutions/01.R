########################################################################################################################
# Advent of Code 2021: Day 1                                                                                           #
#                                                                                                                      #
# Author: Dan Lependorf                                                                                                #
# Date: 2021-12-01                                                                                                     #
########################################################################################################################

library(tidyverse)
library(httr)

session <- readChar("./session.txt", file.info("./session.txt")$size)

########################################################################################################################
# Part One                                                                                                             #
########################################################################################################################

data <- "https://adventofcode.com/2021/day/1/input" %>%
    GET(set_cookies(session=session)) %>%
    content(encoding="UTF-8") %>%
    read_table(col_names="depths")

data %>%
    mutate(last_value=lag(depths, n=1),
           increased=if_else(last_value > depths, 0, 1)) %>%
    pull(increased) %>%
    sum(na.rm=TRUE)

########################################################################################################################
# Part Two                                                                                                             #
########################################################################################################################

data %>%
    mutate(first_window=lag(depths, n=1)+lag(depths, n=2)+lag(depths, n=3),
           second_window=depths+lag(depths, n=1)+lag(depths, n=2),
           window_increased=if_else(second_window > first_window, 1, 0)) %>%
    pull(window_increased) %>%
    sum(na.rm=TRUE)
