########################################################################################################################
# Advent of Code 2022: Day 11                                                                                          #
#                                                                                                                      #
# Author: Dan Lependorf                                                                                                #
# Date: 2022-12-11                                                                                                     #
########################################################################################################################

library(tidyverse)

data <- read_lines("./2022/inputs/11.txt")

########################################################################################################################
# Part One                                                                                                             #
########################################################################################################################

# This is going to be a lot of formatting and tidying.
tidy_data <- tibble(data) %>%
    filter(data != "",
           str_sub(data, start=1, end=6) != "Monkey") %>%
    mutate(monkey_id=ceiling(row_number()/5)-1) %>%
    separate(data, into=c("label", "value"), sep=": ") %>%
    mutate(label=case_when(str_detect(label, "Starting") ~ "items",
                           str_detect(label, "Operation") ~ "operation",
                           str_detect(label, "Test") ~ "divisibility_test",
                           str_detect(label, "true") ~ "if_true",
                           str_detect(label, "false") ~ "if_false")) %>%
    pivot_wider(id_cols=monkey_id, names_from=label, values_from=value) %>%
    mutate(items=map(items, ~as.numeric(str_split(.x, ", ")[[1]])),
           operation=map_chr(operation, ~str_match(.x, "new = old (.*)$")[,2]),
           operand=str_sub(operation, 1, 1),
           # This is going to throw a warning and coerce all "old" arguments to NAs, but that's fine, I'll deal with it
           # later.
           argument=as.integer(str_sub(operation, 3, -1)),
           divisibility_test=map_int(divisibility_test, ~as.integer(str_extract(.x, "[0-9]+")[[1]])),
           if_true=map_int(if_true, ~as.integer(str_extract(.x, "[0-9]+")[[1]])),
           if_false=map_int(if_false, ~as.integer(str_extract(.x, "[0-9]+")[[1]]))) %>%
    select(-operation)

monkey_inspections <- vector(mode="integer", length=nrow(tidy_data))

# I hate this. For each round...
for (i in 1:20) {
    # ...and each monkey...
    for (j in seq_len(nrow(tidy_data))) {
        # ...loop across that monkey's items until there are none remaining.
        while (length(tidy_data$items[[j]]) > 0) {
            monkey_inspections[[j]] <- monkey_inspections[[j]] + 1
            
            worry_level <- tidy_data$items[[j]][[1]]
            operand <- tidy_data$operand[[j]]
            argument <- ifelse(!is.na(tidy_data$argument[[j]]), tidy_data$argument[[j]], worry_level)
            divisibility_test <- tidy_data$divisibility_test[[j]]
            
            if (operand=="+") {
                worry_level <- worry_level + argument
            } else if (operand=="*") {
                worry_level <- worry_level * argument
            }
            
            worry_level <- floor(worry_level/3)
            
            if (worry_level %% divisibility_test == 0) {
                monkey_to_throw_to <- tidy_data$if_true[[j]]
            } else {
                monkey_to_throw_to <- tidy_data$if_false[[j]]
            }
            
            tidy_data$items[[j]] <- tail(tidy_data$items[[j]], length(tidy_data$items[[j]])-1)
            tidy_data$items[[monkey_to_throw_to+1]] <- c(tidy_data$items[[monkey_to_throw_to+1]], worry_level)
        }
    }
}

monkey_inspections %>%
    sort(decreasing=TRUE) %>%
    head(2) %>%
    prod()

########################################################################################################################
# Part Two                                                                                                             #
########################################################################################################################

# Hmm, I wonder why part two seems easier than part one, let me just run this for 10,000 iterations without that
# dividing step, and...oh. Thankfully, I only need to check for divisibility, I don't actually need to keep track of the
# true worry levels, so if I just keep taking the modulus of the highest number that matters for divisibility, I can
# keep those worry numbers down.
monkey_inspections <- vector(mode="integer", length=nrow(tidy_data))

for (i in 1:10000) {
    for (j in seq_len(nrow(tidy_data))) {
        while (length(tidy_data$items[[j]]) > 0) {
            monkey_inspections[[j]] <- monkey_inspections[[j]] + 1
            
            worry_level <- tidy_data$items[[j]][[1]]
            operand <- tidy_data$operand[[j]]
            argument <- ifelse(!is.na(tidy_data$argument[[j]]), tidy_data$argument[[j]], worry_level)
            divisibility_test <- tidy_data$divisibility_test[[j]]
            
            if (operand=="+") {
                worry_level <- worry_level + argument
            } else if (operand=="*") {
                worry_level <- worry_level * argument
            }
            
            worry_level <- ifelse(worry_level > prod(tidy_data$divisibility_test),
                                  worry_level %% prod(tidy_data$divisibility_test),
                                  worry_level)

            if (worry_level %% divisibility_test == 0) {
                monkey_to_throw_to <- tidy_data$if_true[[j]]
            } else {
                monkey_to_throw_to <- tidy_data$if_false[[j]]
            }
            
            tidy_data$items[[j]] <- tail(tidy_data$items[[j]], length(tidy_data$items[[j]])-1)
            tidy_data$items[[monkey_to_throw_to+1]] <- c(tidy_data$items[[monkey_to_throw_to+1]], worry_level)
        }
    }
}

monkey_inspections %>%
    sort(decreasing=TRUE) %>%
    head(2) %>%
    prod()
