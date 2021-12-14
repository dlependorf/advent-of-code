########################################################################################################################
# Advent of Code 2021: Day 13                                                                                          #
#                                                                                                                      #
# Author: Dan Lependorf                                                                                                #
# Date: 2021-12-13                                                                                                     #
########################################################################################################################

library(tidyverse)

data <- read_lines("./2021/inputs/13.txt")

########################################################################################################################
# Part One                                                                                                             #
########################################################################################################################

points <- data[str_detect(data, ",")] %>%
    as_tibble() %>%
    separate(value, into=c("x", "y"), sep=",", convert=TRUE)

instructions <- data[str_detect(data, "fold")] %>%
    str_remove("fold along ") %>%
    as_tibble() %>%
    separate(value, into=c("fold_axis", "fold_line"), sep="=", convert=TRUE)

fold_axis <- instructions$fold_axis[[1]]
fold_line <- instructions$fold_line[[1]]

points %>%
    # Points to the left of the vertical fold line won't change, but points to the right of it change by this much.
    mutate(x=if_else(x < fold_line, x, x-2L*(x-fold_line))) %>%
    distinct() %>%
    nrow()

########################################################################################################################
# Part Two                                                                                                             #
########################################################################################################################

# Same thing, but iterated, and able to also do vertical folds.
points_loop <- points
for (i in seq_len(nrow(instructions))) {
    fold_axis <- instructions$fold_axis[[i]]
    fold_line <- instructions$fold_line[[i]]
    
    if (fold_axis=="x") {
        points_loop <- points_loop %>%
            mutate(x=if_else(x < fold_line, x, x-2L*(x-fold_line))) %>%
            distinct()
    } else {
        points_loop <- points_loop %>%
            mutate(y=if_else(y < fold_line, y, y-2L*(y-fold_line))) %>%
            distinct()
    }
}

ggplot(points_loop) +
    geom_point(aes(x=x, y=y)) +
    scale_y_reverse()
