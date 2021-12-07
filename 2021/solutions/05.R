########################################################################################################################
# Advent of Code 2021: Day 5                                                                                           #
#                                                                                                                      #
# Author: Dan Lependorf                                                                                                #
# Date: 2021-12-05                                                                                                     #
########################################################################################################################

library(tidyverse)

data <- read_csv(file="./2021/inputs/05.txt", col_names=c("x1", "split", "y2"), show_col_types=FALSE) %>%
    separate(col=split, into=c("y1", "x2"), sep=" -> ") %>%
    mutate(across(everything(), as.integer))

########################################################################################################################
# Part One                                                                                                             #
########################################################################################################################

# Okay, I haven't seen part two yet, but there's no way that "for now, only consider horizontal and vertical lines"
# thing doesn't come back to bite me later if I don't come up with a solution that can deal diagonals now, right? And a
# quick look through the full dataset (including line segments that aren't just horizontal or vertical) shows that I
# only have to deal with diagonals where the slope is 1, not other non-zero non-infinite values, meaning I'll only have
# to deal with lines that can be defined by integer sequences that increase by 0 or 1, making this a LOT easier.
line_points <- data %>%
    # First, find the max length of each line across either axis...
    rowwise() %>%
    mutate(line_length=max(abs(x1-x2), abs(y1-y2))) %>%
    ungroup() %>%
    # ...and then generate the points in between each set of endpoints. I'm adding 1 to the length.out argument to
    # include both endpoints.
    mutate(x_seq=pmap(list(x1, x2, line_length), ~seq(from=..1, to=..2, length.out=..3+1)),
           y_seq=pmap(list(y1, y2, line_length), ~seq(from=..1, to=..2, length.out=..3+1)))

# All I have to do now is unlist x_seq and y_seq and count the number of occurrences for each pair.
line_points %>%
    # And filter out the non-diagonal lines, of course.
    filter(x1==x2 | y1==y2) %>%
    {
        x <- unlist(.$x_seq)
        y <- unlist(.$y_seq)
        
        tibble(x, y)
    } %>%
    count(x, y) %>%
    filter(n >= 2) %>%
    nrow()

########################################################################################################################
# Part Two                                                                                                             #
########################################################################################################################

# I was right! Literally all I have to do here is repeat the last pipe chain above, but remove that filter line.
line_points %>%
    {
        x <- unlist(.$x_seq)
        y <- unlist(.$y_seq)
        
        tibble(x, y)
    } %>%
    count(x, y) %>%
    filter(n >= 2) %>%
    nrow()
