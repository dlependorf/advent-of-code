########################################################################################################################
# Advent of Code 2022: Day 8                                                                                           #
#                                                                                                                      #
# Author: Dan Lependorf                                                                                                #
# Date: 2022-12-08                                                                                                     #
########################################################################################################################

library(tidyverse)

data <- read_lines("./2022/inputs/08.txt")

########################################################################################################################
# Part One                                                                                                             #
########################################################################################################################

# Like a lot of these, most of the work is wrangling this into a tidy dataframe where each tree has its own row, with
# appropriate row and column IDs.
tidy_data <- tibble(data) %>%
    mutate(row_id=row_number(),
           data=map(data, ~str_split(.x, "")[[1]])) %>%
    unnest(data) %>%
    group_by(row_id) %>%
    mutate(column_id=row_number(),
           tree_height=as.integer(data)) %>%
    ungroup() %>%
    select(-data)

# There's probably a very elegant one liner purrr map solution here, but I'm going to do this with an ugly for loop.
# It's very slow (7-ish  minutes) because it has to check all four directions for every single tree without being able
# to intelligently skip anything, but oh well.
visible_trees <- 0

for (i in seq_len(nrow(tidy_data))) {
    check_up <- tidy_data %>%
        filter(row_id < tidy_data$row_id[[i]],
               column_id == tidy_data$column_id[[i]],
               tree_height >= tidy_data$tree_height[[i]]) %>%
        nrow()
    
    check_right <- tidy_data %>%
        filter(row_id == tidy_data$row_id[[i]],
               column_id > tidy_data$column_id[[i]],
               tree_height >= tidy_data$tree_height[[i]]) %>%
        nrow()
    
    check_down <- tidy_data %>%
        filter(row_id > tidy_data$row_id[[i]],
               column_id == tidy_data$column_id[[i]],
               tree_height >= tidy_data$tree_height[[i]]) %>%
        nrow()
    
    check_left <- tidy_data %>%
        filter(row_id == tidy_data$row_id[[i]],
               column_id < tidy_data$column_id[[i]],
               tree_height >= tidy_data$tree_height[[i]]) %>%
        nrow()
    
    if (check_up==0 | check_right==0 | check_down==0 | check_left==0) {
        visible_trees <- visible_trees + 1
    }
}

visible_trees

########################################################################################################################
# Part Two                                                                                                             #
########################################################################################################################

# Let's do another ugly unoptimized for loop! This one is going to hinge around heavy use of cummax(). Again, about
# seven minutes.
highest_scenic_score <- 0

for (i in seq_len(nrow(tidy_data))) {
    check_up <- tidy_data %>%
        filter(row_id < tidy_data$row_id[[i]],
               column_id == tidy_data$column_id[[i]]) %>%
        arrange(desc(row_id)) %>%
        mutate(cum_max=cummax(tree_height),
               cum_sum=cumsum(cum_max >= tidy_data$tree_height[[i]])) %>%
        filter(cum_sum <= 1) %>%
        nrow()
    
    check_right <- tidy_data %>%
        filter(row_id == tidy_data$row_id[[i]],
               column_id > tidy_data$column_id[[i]]) %>%
        arrange(column_id) %>%
        mutate(cum_max=cummax(tree_height),
               cum_sum=cumsum(cum_max >= tidy_data$tree_height[[i]])) %>%
        filter(cum_sum <= 1) %>%
        nrow()
    
    check_down <- tidy_data %>%
        filter(row_id > tidy_data$row_id[[i]],
               column_id == tidy_data$column_id[[i]]) %>%
        arrange(row_id) %>%
        mutate(cum_max=cummax(tree_height),
               cum_sum=cumsum(cum_max >= tidy_data$tree_height[[i]])) %>%
        filter(cum_sum <= 1) %>%
        nrow()
    
    check_left <- tidy_data %>%
        filter(row_id == tidy_data$row_id[[i]],
               column_id < tidy_data$column_id[[i]]) %>%
        arrange(desc(column_id)) %>%
        mutate(cum_max=cummax(tree_height),
               cum_sum=cumsum(cum_max >= tidy_data$tree_height[[i]])) %>%
        filter(cum_sum <= 1) %>%
        nrow()
    
    highest_scenic_score <- max(highest_scenic_score, check_up * check_right * check_down * check_left)
}

highest_scenic_score
