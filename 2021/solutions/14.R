########################################################################################################################
# Advent of Code 2021: Day 14                                                                                          #
#                                                                                                                      #
# Author: Dan Lependorf                                                                                                #
# Date: 2021-12-14                                                                                                     #
########################################################################################################################

library(tidyverse)

data <- read_lines("./2021/inputs/14.txt")

########################################################################################################################
# Part One                                                                                                             #
########################################################################################################################

template <- data[[1]]

rules <- data[str_detect(data, "->")] %>%
    as_tibble() %>%
    separate(value, into=c("pair", "insert"), sep=" -> ") %>%
    # I'm taking advantage of a couple of things here: one, the assignment version of stringr::str_sub() surrounded by
    # backticks, and two, the fact that putting an end value before the start value inserts without replacing.
    mutate(insert=`str_sub<-`(string=pair, start=2, end=1, value=insert))

polymer <- template
for (i in 1:10) {
    iteration_rules <- rules %>%
        # This lookahead regex makes sure I'm grabbing all matches, even overlapping ones.
        mutate(insertion_location=map(pair, ~str_locate_all(polymer, paste0("(?=(", .x, "))"))[[1]][,1])) %>%
        unnest(insertion_location) %>%
        arrange(insertion_location) %>%
        mutate(insertion_location=insertion_location+row_number()-1)
    
    for (j in seq_len(nrow(iteration_rules))) {
        insertion_location <- iteration_rules$insertion_location[[j]]
        insert <- iteration_rules$insert[[j]]
        
        str_sub(polymer, start=insertion_location, end=insertion_location+1) <- insert
    }
}

polymer %>%
    str_split("") %>%
    pluck(1) %>%
    table() %>%
    sort() %>%
    {tail(., 1) - head(., 1)}

########################################################################################################################
# Part Two                                                                                                             #
########################################################################################################################

# That approach above won't scale. I'll have to pull a similar trick as in day 8, where instead of storing the entire
# polymer, I'll have to store counts of each element. The tricky part here is that because I'm inserting characters into
# pairs, I'll have to store every overlapping pair of characters as separate elements to store.
options(scipen=999)

polymer_split_loop <- polymer %>%
    str_split("") %>%
    pluck(1) %>%
    tibble(char=.) %>%
    mutate(next_char=lead(char)) %>%
    filter(!is.na(next_char)) %>%
    transmute(pair=paste0(char, next_char)) %>%
    mutate(count=1)

for (i in 1:40) {
    polymer_split_loop <- polymer_split_loop %>%
        inner_join(rules, by="pair") %>%
        mutate(first_pair=str_sub(insert, 1, 2),
               second_pair=str_sub(insert, 2, 3)) %>%
        {
            first_pair <- select(., pair=first_pair, count)
            second_pair <- select(., pair=second_pair, count)
            
            bind_rows(first_pair, second_pair)
        } %>%
        group_by(pair) %>%
        summarize(count=sum(count))
}

# Now that I have all of the pairs and the number of occurrences, the correct number of total letters is a count of the
# FIRST letter of each pair, plus one count of the final letter of the original polymer. The final letter of the
# original polymer works because I can only insert letters in the middle of pairs, so the final letter of the original
# polymer also necessarily is the final letter of the final polymer.
polymer_split_loop %>%
    mutate(letter=str_sub(pair, 1, 1)) %>%
    select(-pair) %>%
    bind_rows(tibble(count=1, letter=str_sub(polymer, -1, -1))) %>%
    group_by(letter) %>%
    summarize(count=sum(count)) %>%
    pull(count) %>%
    sort() %>%
    {tail(., 1) - head(., 1)}

