########################################################################################################################
# Advent of Code 2021: Day 3                                                                                           #
#                                                                                                                      #
# Author: Dan Lependorf                                                                                                #
# Date: 2021-12-03                                                                                                     #
########################################################################################################################

library(tidyverse)

data <- read_table(file="./2021/inputs/03.txt", col_names="numbers")

########################################################################################################################
# Part One                                                                                                             #
########################################################################################################################

# This one is rough! I'm not going to pretend this is the most efficient way of doing this, but...
part_one <- data %>%
    # ...first, separate each number into 12 columns. This is a fairly non-standard way of using the sep argument here,
    # but I'm giving character split index placements, rather than a regex...
    separate(col=numbers, into=paste0("digit_", 1:12), sep=1:11) %>%
    # ...now that everything is separated, pivot from wide to long...
    pivot_longer(starts_with("digit"), names_to="digit_num", values_to="numbers", names_prefix="digit_",
                 names_transform=list(digit_num=as.integer), values_transform=list(numbers=as.integer)) %>%
    # ...and now group by each digit placement and summarize.
    group_by(digit_num) %>%
    summarize(ones=sum(numbers)) %>%
    mutate(most_common_digit=if_else(ones > nrow(data)/2, 1, 0),
           least_common_digit=if_else(ones < nrow(data)/2, 1, 0))

part_one_gamma_binary <- paste(part_one$most_common_digit, collapse="")
part_one_epsilon_binary <- paste(part_one$least_common_digit, collapse="")

# This function is new to me, but strtoi() will convert these binary strings directly to decimal.
strtoi(part_one_gamma_binary, base=2) * strtoi(part_one_epsilon_binary, base=2)

########################################################################################################################
# Part Two                                                                                                             #
########################################################################################################################

# Well, just like day 2, my non-loop approach isn't going to cut it anymore. This time I'll need a while loop.
part_two_prep <- data %>%
    mutate(num_id=row_number()) %>%
    separate(col=numbers, into=paste0("digit_", 1:12), sep=1:11) %>%
    pivot_longer(starts_with("digit"), names_to="digit_num", values_to="numbers", names_prefix="digit_",
                 names_transform=list(digit_num=as.integer), values_transform=list(numbers=as.integer))

# In English: start with digit 1, see what's the most common, filter out everything that doesn't share that digit in
# that place, check how many are left. If it's one, move on, if not, keep going to the next digit place.
loop_dataset <- part_two_prep
num_distinct_numbers <- length(unique(loop_dataset$num_id))
digit_to_run <- 1
while (num_distinct_numbers > 1) {
    most_common_checker <- loop_dataset %>%
        filter(digit_num==digit_to_run) %>%
        pull(numbers) %>%
        # This trick saves me the trouble of tabulating the entire vector. Since I know these are all binary zeros and
        # ones, if the sum divided by the length is greater than 0.5, I know that 1 is more common than 0.
        {sum(.)/length(.)}
    
    digit_to_keep <- if_else(most_common_checker >= 0.5, 1, 0)
    
    # This little dplyr dance is the simplest way I can think of that will figure out which numbers have the correct
    # digits in the correct places.
    loop_dataset <- loop_dataset %>%
        filter(digit_num==digit_to_run,
               numbers==digit_to_keep) %>%
        select(num_id) %>%
        semi_join(loop_dataset, ., by="num_id")
    
    digit_to_run <- digit_to_run + 1
    
    num_distinct_numbers <- length(unique(loop_dataset$num_id))
}

part_two_oxygen_rating <- paste(loop_dataset$numbers, collapse="")

# Same thing here, but we're looking for the least common digit.
loop_dataset <- part_two_prep
num_distinct_numbers <- length(unique(loop_dataset$num_id))
digit_to_run <- 1
while (num_distinct_numbers > 1) {
    least_common_checker <- loop_dataset %>%
        filter(digit_num==digit_to_run) %>%
        pull(numbers) %>%
        {sum(.)/length(.)}
    
    # This is the only difference from above, since I'm looking to see if the sum divided by the length is below 0.5.
    digit_to_keep <- if_else((1-least_common_checker) <= 0.5, 0, 1)
    
    loop_dataset <- loop_dataset %>%
        filter(digit_num==digit_to_run,
               numbers==digit_to_keep) %>%
        select(num_id) %>%
        semi_join(loop_dataset, ., by="num_id")
    
    digit_to_run <- digit_to_run + 1
    
    num_distinct_numbers <- length(unique(loop_dataset$num_id))
}

part_two_co2_rating <- paste(loop_dataset$numbers, collapse="")

strtoi(part_two_oxygen_rating, base=2) * strtoi(part_two_co2_rating, base=2)
