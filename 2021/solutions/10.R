########################################################################################################################
# Advent of Code 2021: Day 10                                                                                          #
#                                                                                                                      #
# Author: Dan Lependorf                                                                                                #
# Date: 2021-12-10                                                                                                     #
########################################################################################################################

library(tidyverse)

data <- read_table("2021/inputs/10.txt", col_names="data")

########################################################################################################################
# Part One                                                                                                             #
########################################################################################################################

# First, I need to define all of these characters, so R knows what each character needs as a closing pair.
characters <- tribble(~opening, ~closing,
                      "(", ")",
                      "[", "]",
                      "{", "}",
                      "<", ">")

split_data <- mutate(data, split_data=map(data, ~str_split(.x, "")[[1]]))

find_corrupted_lines <- function(corrupted_line) {
    stop_char <- NA_character_

    opening_chars <- character()
    
    # This loop iterates across every function in the corrupted line. Essentially what I'm doing with the if statement
    # below is keeping a tally of all opening characters, removing them when I hit the correct closing character, and
    # breaking the loop if I hit an incorrect closing character.
    for (i in seq_along(corrupted_line)) {
        if (corrupted_line[[i]] %in% characters$opening) {
            opening_chars <- c(opening_chars, corrupted_line[[i]])

            next_closing_char <- characters %>%
                filter(opening==tail(opening_chars, 1)) %>%
                pull(closing)

            next
        } else if (corrupted_line[[i]]==next_closing_char) {
            opening_chars <- head(opening_chars, -1)
            
            next_closing_char <- characters %>%
                filter(opening==tail(opening_chars, 1)) %>%
                pull(closing)

            next
        } else {
            stop_char <- corrupted_line[[i]]

            break
        }
    }
    
    if (is.na(stop_char) & length(opening_chars)==0) {
        return("complete")
    } else if (is.na(stop_char) & length(opening_chars)!=0) {
        return("incomplete")
    } else {
        return(stop_char)
    }
}

line_results <- mutate(split_data, results=map_chr(split_data, find_corrupted_lines))

line_results %>%
    mutate(syntax_score=case_when(results==")" ~ 3,
                                  results=="]" ~ 57,
                                  results=="}" ~ 1197,
                                  results==">" ~ 25137,
                                  TRUE ~ NA_real_)) %>%
    pull(syntax_score) %>%
    sum(na.rm=TRUE)

########################################################################################################################
# Part Two                                                                                                             #
########################################################################################################################

incomplete_lines <- filter(line_results, results=="incomplete")

# Thankfully, I can borrow almost all of my approach from part one here.
autocomplete_lines <- function(incomplete_line) {
    stop_char <- NA_character_

    opening_chars <- character()

    for (i in seq_along(incomplete_line)) {
        if (incomplete_line[[i]] %in% characters$opening) {
            opening_chars <- c(opening_chars, incomplete_line[[i]])

            next_closing_char <- characters %>%
                filter(opening==tail(opening_chars, 1)) %>%
                pull(closing)

            next
        } else if (incomplete_line[[i]]==next_closing_char) {
            opening_chars <- head(opening_chars, -1)
            
            next_closing_char <- characters %>%
                filter(opening==tail(opening_chars, 1)) %>%
                pull(closing)

            next
        } else {
            stop_char <- incomplete_line[[i]]

            break
        }
    }
    
    # This is the only difference, as I now need to return the correct completed sequence. I'm doing this by reversing
    # the list of characters that have not been closed, and joining to the characters dataframe to find each one's
    # closing pair.
    tibble(opening_chars=rev(opening_chars)) %>%
        inner_join(characters, by=c("opening_chars"="opening")) %>%
        pull(closing)
}

completed_lines <- mutate(incomplete_lines, completed=map(split_data, autocomplete_lines))

# Since the score totals here are just 1, 2, 3, 4, I can take advantage of R's vector index system. Instead of storing
# the values, I can just use match() to find each one's position.
score_ref <- c(")", "]", "}", ">")

calculate_autocomplete_score <- function(autocomplete) {
    autocomplete_num <- match(autocomplete, score_ref)
    
    score <- 0
    
    for (i in seq_along(autocomplete_num)) {
        score <- 5*score + autocomplete_num[[i]]
    }
    
    score
}

completed_lines %>%
    pull(completed) %>%
    map_dbl(calculate_autocomplete_score) %>%
    median()
