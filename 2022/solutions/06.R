########################################################################################################################
# Advent of Code 2022: Day 6                                                                                           #
#                                                                                                                      #
# Author: Dan Lependorf                                                                                                #
# Date: 2022-12-06                                                                                                     #
########################################################################################################################

library(tidyverse)

data <- read_lines("./2022/inputs/06.txt")

########################################################################################################################
# Part One                                                                                                             #
########################################################################################################################

# This one's not too bad, I'll just need to loop through the string until this while loop find four unique characters.
four_different <- FALSE
characters_processed <- 4

while (four_different == FALSE) {
    four_characters <- str_sub(data, characters_processed - 3, characters_processed)
    
    unique_characters <- four_characters %>%
        str_split("") %>%
        pluck(1) %>%
        unique() %>%
        length()
    
    if (unique_characters < 4) {
        characters_processed <- characters_processed + 1
    } else {
        four_different <- TRUE
    }
}

characters_processed

########################################################################################################################
# Part Two                                                                                                             #
########################################################################################################################

# Same thing, with just a few tiny changes from 4 to 14.
fourteen_different <- FALSE
characters_processed <- 14

while (fourteen_different == FALSE) {
    fourteen_characters <- str_sub(data, characters_processed - 13, characters_processed)
    
    unique_characters <- fourteen_characters %>%
        str_split("") %>%
        pluck(1) %>%
        unique() %>%
        length()
    
    if (unique_characters < 14) {
        characters_processed <- characters_processed + 1
    } else {
        fourteen_different <- TRUE
    }
}

characters_processed
