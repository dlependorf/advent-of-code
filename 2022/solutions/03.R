########################################################################################################################
# Advent of Code 2022: Day 3                                                                                           #
#                                                                                                                      #
# Author: Dan Lependorf                                                                                                #
# Date: 2022-12-03                                                                                                     #
########################################################################################################################

library(tidyverse)

data <- read_lines("./2022/inputs/03.txt")

########################################################################################################################
# Part One                                                                                                             #
########################################################################################################################

priorities <- tibble(letter=c(letters, LETTERS)) %>%
    mutate(priority=row_number())

# This is most of the work here, just getting the data to a tidy state where each row is one observation.
tidy_data <- tibble(data=data) %>%
    transmute(first_compartment=str_sub(data, start=1, end=nchar(data)/2),
              second_compartment=str_sub(data, start=-nchar(data)/2, end=-1)) %>%
    mutate(across(everything(), ~str_split(.x, "")),
           rucksack_id=row_number()) %>%
    pivot_longer(cols=ends_with("compartment"), names_to="compartment_id", values_to="item") %>%
    unnest(item)

first_compartment <- tidy_data %>%
    filter(compartment_id=="first_compartment") %>%
    select(-compartment_id) %>%
    distinct()

second_compartment <- tidy_data %>%
    filter(compartment_id=="second_compartment") %>%
    select(-compartment_id) %>%
    distinct()

first_compartment %>%
    inner_join(second_compartment, by=c("rucksack_id", "item")) %>%
    inner_join(priorities, by=c("item"="letter")) %>%
    pull(priority) %>%
    sum()

########################################################################################################################
# Part Two                                                                                                             #
########################################################################################################################

# I'll have to re-do the tidy data step, since this time I don't care about compartments.
tidy_data_part_two <- tibble(data=data) %>%
    transmute(elf_group_id=ceiling(row_number()/3),
              item=str_split(data, "")) %>%
    group_by(elf_group_id) %>%
    mutate(elf_id=row_number()) %>%
    ungroup() %>%
    unnest(item)

first_elf_in_group <- tidy_data_part_two %>%
    filter(elf_id==1) %>%
    select(-elf_id) %>%
    distinct()

second_elf_in_group <- tidy_data_part_two %>%
    filter(elf_id==2) %>%
    select(-elf_id) %>%
    distinct()

third_elf_in_group <- tidy_data_part_two %>%
    filter(elf_id==3) %>%
    select(-elf_id) %>%
    distinct()

first_elf_in_group %>%
    inner_join(second_elf_in_group, by=c("elf_group_id", "item")) %>%
    inner_join(third_elf_in_group, by=c("elf_group_id", "item")) %>%
    inner_join(priorities, by=c("item"="letter")) %>%
    pull(priority) %>%
    sum()
