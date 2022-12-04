########################################################################################################################
# Advent of Code 2022: Day 4                                                                                           #
#                                                                                                                      #
# Author: Dan Lependorf                                                                                                #
# Date: 2022-12-04                                                                                                     #
########################################################################################################################

library(tidyverse)

data <- read_lines("./2022/inputs/04.txt")

########################################################################################################################
# Part One                                                                                                             #
########################################################################################################################

# My guess is that I'll have to actually clean this dataset up later, but for this one, I actually only need to find
# pairs where one of them has the lower bound higher than the other and the upper bound lower than the other.
separated_data <- tibble(data=data) %>%
    separate(data, into=c("first_elf", "second_elf"), sep=",") %>%
    # I assume there's some sweet elegant way of condensing this into a single line, but oh well.
    separate(first_elf, into=c("first_elf_min", "first_elf_max"), sep="-") %>%
    separate(second_elf, into=c("second_elf_min", "second_elf_max"), sep="-") %>%
    # This was a frustrating debug...they were all characters...
    mutate(across(everything(), as.integer))

separated_data %>%
    filter((first_elf_min >= second_elf_min & first_elf_max <= second_elf_max) |
               first_elf_min <= second_elf_min & first_elf_max >= second_elf_max) %>%
    nrow()

########################################################################################################################
# Part Two                                                                                                             #
########################################################################################################################

# Not too bad! Just need to tweak the comparison operators a bit and make sure I negate the filter() call, since I'm
# looking for the ones where they do overlap, not where they don't.
separated_data %>%
    filter(!(first_elf_max < second_elf_min | first_elf_min > second_elf_max)) %>%
    nrow()
