########################################################################################################################
# Advent of Code 2022: Day 5                                                                                           #
#                                                                                                                      #
# Author: Dan Lependorf                                                                                                #
# Date: 2022-12-05                                                                                                     #
########################################################################################################################

library(tidyverse)

data <- read_lines("./2022/inputs/05.txt")

########################################################################################################################
# Part One                                                                                                             #
########################################################################################################################

# Ugggggh, formatting this is going to be annoying. Let's deal with the crates first. The best way I can think of doing
# this is by treating the first eight lines as a flat width file.
starting_crates <- "./2022/inputs/05.txt" %>%
    read_fwf(file=., col_positions=fwf_widths(rep(4, 9), col_names=paste0("stack_", 1:9))) %>%
    slice(1:8) %>%
    # Removing the brackets is pretty simple from here.
    mutate(across(everything(), ~str_remove_all(.x, "\\[|\\]"))) %>%
    # This is a weird one, but map(pluck) actually functions sort of as a matrix transpose here, and also gets this out
    # of a data frame and into a list.
    map(pluck) %>%
    # Remove the NAs, and now the top crate in each stack is the first nested element of each list element.
    map(~discard(.x, is.na))

# And now the instructions.
instructions <- tibble(data=data) %>%
    slice(11:nrow(.)) %>%
    separate(data, into=c("num_crates", "location"), sep=" from ") %>%
    mutate(num_crates=as.integer(str_remove(num_crates, "move "))) %>%
    # Now that the number of crates are parsed out, I want to unfurl the number of crates, so, for example, instead of
    # "move 8 from 3 to 2", it's eight separate rows of "3 to 2". A rep() call inside of a map2() loop should work.
    mutate(location=map2(.x=location, .y=num_crates, ~rep(.x, .y))) %>%
    select(-num_crates) %>%
    unnest(location) %>%
    separate(location, into=c("from", "to"), sep=" to ") %>%
    mutate(across(everything(), as.integer))

# Now loop over every instruction and rearrange the stacks.
i <- 1
crates <- starting_crates

for (i in seq_len(nrow(instructions))) {
    instruction_from <- instructions$from[[i]]
    instruction_to <- instructions$to[[i]]
    
    crate_to_move <- crates[[instruction_from]][[1]]
    
    crates[[instruction_from]] <- crates[[instruction_from]][-1]
    crates[[instruction_to]] <- c(crate_to_move, crates[[instruction_to]])
}

# Finally, grab the first crate from each stack.
crates %>%
    map(~pluck(.x, 1)) %>%
    unlist() %>%
    paste0(collapse="")

########################################################################################################################
# Part Two                                                                                                             #
########################################################################################################################

# I'm going to have to re-do the instruction parsing bit, to remove the rep() in that map2() call.
instructions_part_2 <- tibble(data=data) %>%
    slice(11:nrow(.)) %>%
    separate(data, into=c("num_crates", "location"), sep=" from ") %>%
    mutate(num_crates=as.integer(str_remove(num_crates, "move "))) %>%
    separate(location, into=c("from", "to"), sep=" to ") %>%
    mutate(across(everything(), as.integer))

# This is mostly the same as before, but with a change to allow pulling multiple vector elements.
i <- 1
crates <- starting_crates

for (i in seq_len(nrow(instructions_part_2))) {
    instruction_from <- instructions_part_2$from[[i]]
    instruction_to <- instructions_part_2$to[[i]]
    instruction_num <- instructions_part_2$num_crates[[i]] 
    
    crates_to_move <- crates[[instruction_from]][1:instruction_num]
    
    crates[[instruction_from]] <- crates[[instruction_from]][-(1:instruction_num)]
    crates[[instruction_to]] <- c(crates_to_move, crates[[instruction_to]])
}

crates %>%
    map(~pluck(.x, 1)) %>%
    unlist() %>%
    paste0(collapse="")
