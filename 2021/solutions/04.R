########################################################################################################################
# Advent of Code 2021: Day 4                                                                                           #
#                                                                                                                      #
# Author: Dan Lependorf                                                                                                #
# Date: 2021-12-04                                                                                                     #
########################################################################################################################

library(tidyverse)

raw_numbers <- readLines(con="./2021/inputs/04.txt", n=1)
raw_boards <- read_fwf(file="./2021/inputs/04.txt", skip=2, show_col_types=FALSE)

########################################################################################################################
# Part One                                                                                                             #
########################################################################################################################

# First, some processing on both of these. Split the numbers up and pivot the boards so I have a table one column for
# the actual board numbers, and three other columns for board, row, and column IDs.
numbers <- raw_numbers %>%
    str_split(",") %>%
    pluck(1) %>%
    as.integer()

boards <- raw_boards %>%
    # Get rid of the extra line breaks between the boards...
    filter(!is.na(X1)) %>%
    # ...because we already know that every group of 5 lines is one board.
    mutate(board_id=ceiling(row_number()/5)) %>%
    # The row_id is easy...
    group_by(board_id) %>%
    mutate(row_id=row_number()) %>%
    ungroup() %>%
    # ...but I'll need to pivot to get the column_id.
    pivot_longer(starts_with("X"), names_to="column_id", values_to="numbers",
                 names_prefix="X", names_transform=list(column_id=as.integer))

# There's probably a more efficient way of doing this, but hello again, while loops.
bingo <- FALSE
number_id <- 1
boards_during_game <- mutate(boards, called=0)
while (bingo==FALSE) {
    number_to_call <- numbers[[number_id]]
    
    boards_during_game <- boards_during_game %>%
        mutate(called=if_else(numbers==number_to_call, 1, called))
    
    # Check to see if any boards have a valid bingo by looking at the row and column sums of the called column.
    bingo_checker <- boards_during_game %>%
        group_by(board_id, row_id) %>%
        mutate(row_sums=sum(called)) %>%
        ungroup() %>%
        group_by(board_id, column_id) %>%
        mutate(column_sums=sum(called)) %>%
        ungroup()
    
    if (any(bingo_checker$row_sums==5) | any(bingo_checker$column_sums==5)) {
        bingo <- TRUE
    } else {
        number_id <- number_id + 1
    }
}

# Bingo!
bingo_checker %>%
    filter(row_sums==5 | column_sums==5) %>%
    select(board_id) %>%
    distinct() %>%
    semi_join(boards_during_game, ., by="board_id") %>%
    filter(called==0) %>%
    pull(numbers) %>%
    sum() %>%
    `*`(number_to_call)

########################################################################################################################
# Part Two                                                                                                             #
########################################################################################################################

# This isn't too much more, I can just copy most of my while loop logic here with a couple of alterations.
all_bingo <- FALSE
number_id <- 1
boards_during_game <- mutate(boards, called=0)
while (all_bingo==FALSE) {
    number_to_call <- numbers[[number_id]]
    
    boards_during_game <- boards_during_game %>%
        mutate(called=if_else(numbers==number_to_call, 1, called))
    
    bingo_checker <- boards_during_game %>%
        group_by(board_id, row_id) %>%
        mutate(row_sums=sum(called)) %>%
        ungroup() %>%
        group_by(board_id, column_id) %>%
        mutate(column_sums=sum(called)) %>%
        ungroup()
    
    # This filters out all boards that haven't won yet to find out how many have already won.
    all_bingo_checker <- bingo_checker %>%
        filter(row_sums==5 | column_sums==5) %>%
        pull(board_id) %>%
        unique() %>%
        length()
    
    # If bingo_checker indicates that the number of winning boards is the same number as the total number of boards,
    # stop. Otherwise, soldier on.
    if (all_bingo_checker==max(boards_during_game$board_id)) {
        all_bingo <- TRUE
    } else {
        number_id <- number_id + 1
    }
}

# Since all boards are now won, I'll have to unwind the last called number to find the one board that was last.
boards_during_game %>%
    mutate(called=if_else(numbers==number_to_call, 0, called)) %>%
    group_by(board_id, row_id) %>%
    mutate(row_sums=sum(called)) %>%
    ungroup() %>%
    group_by(board_id, column_id) %>%
    mutate(column_sums=sum(called)) %>%
    ungroup() %>%
    filter(row_sums==5 | column_sums==5) %>%
    select(board_id) %>%
    distinct() %>%
    # Same as above (with re-computed bingo_checker logic), except that this below is an anti join instead of a semi
    # join, since I want to find the only non-matching board this time.
    anti_join(boards_during_game, ., by="board_id") %>%
    # Because the anti join was to boards_during_game, I don't need to "re-call" the winning number, I can just compute
    # the final score.
    filter(called==0) %>% 
    pull(numbers) %>%
    sum() %>%
    `*`(number_to_call)
