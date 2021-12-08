########################################################################################################################
# Advent of Code 2021: Day 7                                                                                           #
#                                                                                                                      #
# Author: Dan Lependorf                                                                                                #
# Date: 2021-12-07                                                                                                     #
########################################################################################################################

library(tidyverse)

data <- readLines("./2021/inputs/07.txt") %>%
    str_split(",") %>%
    unlist() %>%
    as.integer()

########################################################################################################################
# Part One                                                                                                             #
########################################################################################################################

# Hold on, this is just the median, isn't it?
position <- median(data)

sum(abs(data-position))

########################################################################################################################
# Part Two                                                                                                             #
########################################################################################################################

# Okay, it's no longer the median. Time to brute force this thing. I imagine there is some elegant actual solution to
# this, which in all likelihood probably exists as a base R function, but oh well.
possible_positions <- tibble(position=seq(min(data), max(data)),
                             fuel_spent=NA_integer_)

# Basically what I'm after is the additive version of a factorial. Turns out that it actually has a closed form.
fuel_calculator <- function(steps) {
    steps * (steps+1) / 2
}

i <- 1
for (i in seq_len(nrow(possible_positions))) {
    position_to_try <- possible_positions$position[[i]]
    
    fuel_spent <- abs(data-position_to_try) %>%
        fuel_calculator() %>%
        sum()
    
    possible_positions$fuel_spent[[i]] <- fuel_spent
}

possible_positions %>%
    arrange(fuel_spent) %>%
    pull(fuel_spent) %>%
    pluck(1)
