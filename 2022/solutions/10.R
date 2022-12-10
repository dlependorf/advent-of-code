########################################################################################################################
# Advent of Code 2022: Day 10                                                                                          #
#                                                                                                                      #
# Author: Dan Lependorf                                                                                                #
# Date: 2022-12-10                                                                                                     #
########################################################################################################################

library(tidyverse)

data <- read_lines("./2022/inputs/10.txt")

########################################################################################################################
# Part One                                                                                                             #
########################################################################################################################

parsed_data <- tibble(data) %>%
    separate(data, into=c("instruction", "value"), sep=" ", fill="right") %>%
    mutate(value=as.integer(value))

values_record <- numeric()

register_value <- 1
current_cycle <- 1

for (i in seq_len(nrow(parsed_data))) {
    if (parsed_data$instruction[[i]]=="noop") {
        values_record[[current_cycle]] <- register_value
        
        current_cycle <- current_cycle + 1
    } else {
        values_record[[current_cycle]] <- register_value
        values_record[[current_cycle + 1]] <- register_value
        
        register_value <- register_value + parsed_data$value[[i]]
        
        current_cycle <- current_cycle + 2
    }
}

tibble(values=values_record) %>%
    mutate(cycle_num=row_number()) %>%
    filter((cycle_num-20) %% 40 == 0) %>%
    mutate(signal_strength=values * cycle_num) %>%
    pull(signal_strength) %>%
    sum()

########################################################################################################################
# Part Two                                                                                                             #
########################################################################################################################

# This isn't too bad. Using the results from part one, I need to see if the register value is within 1 of the pixel
# position, and then wrap in rows of 40 pixels each.
tibble(values=values_record) %>%
    mutate(cycle_num=row_number(),
           pixel_position=(row_number()-1) %% 40,
           pixel_value=if_else(abs(values-pixel_position) <= 1, "X", " "),
           row_id=ceiling(row_number()/40)) %>%
    group_by(row_id) %>%
    summarize(rows=paste(pixel_value, collapse=""))
