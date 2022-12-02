########################################################################################################################
# Advent of Code 2022: Day 2                                                                                           #
#                                                                                                                      #
# Author: Dan Lependorf                                                                                                #
# Date: 2022-12-02                                                                                                     #
########################################################################################################################

library(tidyverse)

data <- read_lines("./2022/inputs/02.txt")

########################################################################################################################
# Part One                                                                                                             #
########################################################################################################################

opponent_score <- tribble(~play, ~opponent_score,
                          "A", 1,
                          "B", 2,
                          "C", 3)

my_score <- tribble(~play, ~my_score,
                    "X", 1,
                    "Y", 2,
                    "Z", 3)

data %>%
    tibble(game=.) %>%
    separate(game, into=c("opponent", "me"), sep=" ") %>%
    inner_join(opponent_score, by=c("opponent"="play")) %>%
    inner_join(my_score, by=c("me"="play")) %>%
    # There might be a better way of doing this, but this covers all possible solutions reasonably efficiently.
    mutate(result_score=case_when(opponent_score == my_score ~ 3,
                                  (opponent_score - my_score) %in% c(-1, 2) ~ 6,
                                  TRUE ~ 0)) %>%
    select(my_score, result_score) %>%
    unlist() %>%
    sum()

########################################################################################################################
# Part Two                                                                                                             #
########################################################################################################################

data %>%
    tibble(game=.) %>%
    separate(game, into=c("opponent", "result"), sep=" ") %>%
    inner_join(opponent_score, by=c("opponent"="play")) %>%
    # The "%in% c(-1, 2)" thing above is sort of a hack, but it looks like here I'll have to do it properly. Rock paper
    # scissors is all cyclical, so really what I'm after is being 2 above the opponent for a loss and 1 above for a win,
    # and then if that puts me above 3, take the modulo of 3 to get the actual correct answer.
    mutate(my_score=case_when(result=="Y" ~ opponent_score,
                              result=="X" ~ opponent_score + 2,
                              result=="Z" ~ opponent_score + 1),
           my_score=if_else(my_score > 3, my_score %% 3, my_score),
           result_score=case_when(result=="Z" ~ 6,
                                  result=="Y" ~ 3,
                                  result=="X" ~ 0)) %>%
    select(my_score, result_score) %>%
    unlist() %>%
    sum()
