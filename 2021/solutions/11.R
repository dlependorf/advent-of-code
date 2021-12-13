########################################################################################################################
# Advent of Code 2021: Day 11                                                                                          #
#                                                                                                                      #
# Author: Dan Lependorf                                                                                                #
# Date: 2021-12-11                                                                                                     #
########################################################################################################################

library(tidyverse)

data <- read_table("./2021/inputs/11.txt", col_names="raw")

########################################################################################################################
# Part One                                                                                                             #
########################################################################################################################

octopi <- data %>%
    separate(raw, into=paste0("column_", 0:10), sep="") %>%
    select(-column_0) %>%
    mutate(row=row_number()) %>%
    pivot_longer(starts_with("column"), names_to="column", values_to="energy", names_prefix="column_",
                 names_transform=list(column=as.integer), values_transform=list(energy=as.integer))

total_flashes <- 0L
octopi_loop <- mutate(octopi, flashed_this_step=0L)

# This is very similar to my solution for day 9.
for (i in 1:100) {
    octopi_loop <- mutate(octopi_loop, energy=energy+1L)
    
    new_flashes <- octopi_loop %>%
        filter(energy >= 10,
               flashed_this_step==0L)
    
    num_new_flashes <- 0L
    
    # This while loop iterates over the flash propagation until the step is done and there is no more new flashes.
    while (nrow(new_flashes) != num_new_flashes) {
        num_new_flashes <- nrow(new_flashes)
        
        octopi_loop <- new_flashes %>%
            # This filter() call makes sure I'm only counting new flashes, and not octopi that have already flashed on
            # previous while loop iterations.
            filter(flashed_this_step==0L) %>%
            full_join(octopi_loop, ., by=character(), suffix=c("", "_flash")) %>%
            filter(abs(row-row_flash) <= 1,
                   abs(column-column_flash) <= 1) %>%
            group_by(row, column) %>%
            summarize(new_energy=n(), .groups="drop") %>%
            left_join(octopi_loop, ., by=c("row", "column")) %>%
            mutate(new_energy=if_else(is.na(new_energy), 0L, new_energy),
                   energy=energy + new_energy) %>%
            select(-new_energy) %>%
            # The end of this pipe chain flips the flashed_this_step flag, so I know not to count it on the next loop.
            left_join(new_flashes, by=c("row", "column"), suffix=c("", "_flash")) %>%
            mutate(flashed_this_step=if_else(!is.na(flashed_this_step_flash), 1L, 0L)) %>%
            select(row, column, energy, flashed_this_step)
        
        new_flashes <- octopi_loop %>%
            filter(energy >= 10)
    }
    
    octopi_loop <- octopi_loop %>%
        mutate(energy=if_else(energy >= 10, 0L, energy),
               flashed_this_step=0L)
    
    total_flashes <- total_flashes + num_new_flashes
}

total_flashes

########################################################################################################################
# Part Two                                                                                                             #
########################################################################################################################

# I should be able to copy the solution from above, but convert the outer for loop to a while loop, breaking when every
# grid space is 0.
step_counter <- 0
octopi_loop <- mutate(octopi, flashed_this_step=0L)

while (sum(octopi_loop$energy) != 0) {
    octopi_loop <- mutate(octopi_loop, energy=energy+1L)
    
    new_flashes <- octopi_loop %>%
        filter(energy >= 10,
               flashed_this_step==0L)
    
    num_new_flashes <- 0L
    
    while (nrow(new_flashes) != num_new_flashes) {
        num_new_flashes <- nrow(new_flashes)
        
        octopi_loop <- new_flashes %>%
            filter(flashed_this_step==0L) %>%
            full_join(octopi_loop, ., by=character(), suffix=c("", "_flash")) %>%
            filter(abs(row-row_flash) <= 1,
                   abs(column-column_flash) <= 1) %>%
            group_by(row, column) %>%
            summarize(new_energy=n(), .groups="drop") %>%
            left_join(octopi_loop, ., by=c("row", "column")) %>%
            mutate(new_energy=if_else(is.na(new_energy), 0L, new_energy),
                   energy=energy + new_energy) %>%
            select(-new_energy) %>%
            left_join(new_flashes, by=c("row", "column"), suffix=c("", "_flash")) %>%
            mutate(flashed_this_step=if_else(!is.na(flashed_this_step_flash), 1L, 0L)) %>%
            select(row, column, energy, flashed_this_step)
        
        new_flashes <- octopi_loop %>%
            filter(energy >= 10)
    }
    
    octopi_loop <- octopi_loop %>%
        mutate(energy=if_else(energy >= 10, 0L, energy),
               flashed_this_step=0L)
    
    step_counter <- step_counter + 1
}

step_counter
