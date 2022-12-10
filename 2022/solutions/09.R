########################################################################################################################
# Advent of Code 2022: Day 9                                                                                           #
#                                                                                                                      #
# Author: Dan Lependorf                                                                                                #
# Date: 2022-12-09                                                                                                     #
########################################################################################################################

library(tidyverse)

data <- read_lines("./2022/inputs/09.txt")

########################################################################################################################
# Part One                                                                                                             #
########################################################################################################################

# I'm going to parse out the directions from the step counts, unfurl the steps so that each row is one step in a
# direction, and also, I'm going to convert all of this to x and y coordinates.
parsed_data <- tibble(data) %>%
    separate(data, into=c("direction", "steps"), sep=" ") %>%
    mutate(steps=as.integer(steps),
           steps=map(steps, ~rep(1L, .x))) %>%
    unnest(steps) %>%
    mutate(x_steps=case_when(direction=="R" ~ steps,
                             direction=="L" ~ -steps,
                             TRUE ~ 0L),
           y_steps=case_when(direction=="U" ~ steps,
                             direction=="D" ~ -steps,
                             TRUE ~ 0L))

# Let's define a distance function, which will make a lot of these repetitive calculations simpler.
distance <- function(head, tail) {
    sqrt((head[[1]] - tail[[1]])^2 + (head[[2]] - tail[[2]])^2)
}

# We also need to keep track of all of the tail positions as we loop over the steps.
tail_positions <- list()

head_location <- c(0, 0)
tail_location <- c(0, 0)

for (i in seq_len(nrow(parsed_data))) {
    head_location <- head_location + c(parsed_data$x_steps[[i]], parsed_data$y_steps[[i]])
    
    rope_length <- distance(head_location, tail_location)
    
    if (rope_length >= 2) {
        # If the rope head is far enough to cause the tail to move, we need to update the tail location.
        tail_location[[1]] <- case_when(head_location[[1]] > tail_location[[1]] ~ tail_location[[1]] + 1,
                                        head_location[[1]] < tail_location[[1]] ~ tail_location[[1]] - 1,
                                        TRUE ~ tail_location[[1]])
        
        tail_location[[2]] <- case_when(head_location[[2]] > tail_location[[2]] ~ tail_location[[2]] + 1,
                                        head_location[[2]] < tail_location[[2]] ~ tail_location[[2]] - 1,
                                        TRUE ~ tail_location[[2]])
        
        tail_positions <- c(tail_positions, list(tail_location))
    }
}

tail_positions %>%
    unique() %>%
    length()

########################################################################################################################
# Part Two                                                                                                             #
########################################################################################################################

# Ugh. I can reuse most of the logic above, but I'll have to repeat each step ten times for each rope knot.
tail_positions <- list()

knot_locations <- rep(list(c(0, 0)), 10)

for (i in seq_len(nrow(parsed_data))) {
    knot_locations[[1]] <- knot_locations[[1]] + c(parsed_data$x_steps[[i]], parsed_data$y_steps[[i]])
    
    # This inner for loop computes the position of each rope knot (except the head).
    for (j in 2:10) {
        rope_length <- distance(knot_locations[[j-1]], knot_locations[[j]])
        
        if (rope_length >= 2) {
            knot_locations[[j]][[1]] <- case_when(knot_locations[[j-1]][[1]] > knot_locations[[j]][[1]]
                                                  ~ knot_locations[[j]][[1]] + 1,
                                                  knot_locations[[j-1]][[1]] < knot_locations[[j]][[1]]
                                                  ~ knot_locations[[j]][[1]] - 1,
                                                  TRUE ~ knot_locations[[j]][[1]])
            
            knot_locations[[j]][[2]] <- case_when(knot_locations[[j-1]][[2]] > knot_locations[[j]][[2]]
                                                  ~ knot_locations[[j]][[2]] + 1,
                                                  knot_locations[[j-1]][[2]] < knot_locations[[j]][[2]]
                                                  ~ knot_locations[[j]][[2]] - 1,
                                                  TRUE ~ knot_locations[[j]][[2]])
        }
    }
    
    tail_positions <- c(tail_positions, list(knot_locations[[10]]))
}

tail_positions %>%
    unique() %>%
    length()
