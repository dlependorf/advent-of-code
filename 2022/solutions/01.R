########################################################################################################################
# Advent of Code 2022: Day 1                                                                                           #
#                                                                                                                      #
# Author: Dan Lependorf                                                                                                #
# Date: 2022-12-01                                                                                                     #
########################################################################################################################

library(tidyverse)

data <- read_lines("./2022/inputs/01.txt")

########################################################################################################################
# Part One                                                                                                             #
########################################################################################################################

data %>%
    tibble(calories=.) %>%
    mutate(calories=as.integer(calories),
           elf_id=cumsum(is.na(calories))) %>%
    group_by(elf_id) %>%
    summarize(calories=sum(calories, na.rm=TRUE)) %>%
    arrange(desc(calories)) %>%
    slice(1) %>%
    pull(calories) %>%
    sum()

########################################################################################################################
# Part Two                                                                                                             #
########################################################################################################################

data %>%
    tibble(calories=.) %>%
    mutate(calories=as.integer(calories),
           elf_id=cumsum(is.na(calories))) %>%
    group_by(elf_id) %>%
    summarize(calories=sum(calories, na.rm=TRUE)) %>%
    arrange(desc(calories)) %>%
    slice(1:3) %>%
    pull(calories) %>%
    sum()
