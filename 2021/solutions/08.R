########################################################################################################################
# Advent of Code 2021: Day 8                                                                                           #
#                                                                                                                      #
# Author: Dan Lependorf                                                                                                #
# Date: 2021-12-08                                                                                                     #
########################################################################################################################

library(tidyverse)

data <- readLines("./2021/inputs/08.txt")

########################################################################################################################
# Part One                                                                                                             #
########################################################################################################################

clean_data <- data %>%
    tibble(raw_data=.) %>%
    separate(raw_data, into=c("signals", "digits"), sep=" \\| ") %>%
    separate(signals, into=paste0("signal", 1:10), sep=" ") %>%
    separate(digits, into=paste0("digit", 1:4), sep=" ") %>%
    mutate(entry_id=row_number())

digits <- clean_data %>%
    select(entry_id, starts_with("digit")) %>%
    pivot_longer(starts_with("digit"), names_to="digit_id", values_to="num",
                 names_prefix="digit", names_transform=list(digit_id=as.integer))

digits %>%
    filter(nchar(num) %in% c(2, 4, 3, 7)) %>%
    nrow()

########################################################################################################################
# Part Two                                                                                                             #
########################################################################################################################

signals <- clean_data %>%
    select(entry_id, starts_with("signal")) %>%
    pivot_longer(starts_with("signal"), names_to="signal_id", values_to="num",
                 names_prefix="signal", names_transform=list(signal_id=as.integer)) %>%
    mutate(nchar=nchar(num),
           segments=map(num, ~str_split(.x, "")[[1]]))

# Alright. I can easily figure out the identity the two segments that are C or F segments by finding what is 1. From
# there, finding A is easy, because I can identify the 7. With those three, I can separate C and F by finding 6 (which
# is the only six-segment number without A+C+F).
letter_a <- signals %>%
    filter(nchar %in% c(2, 3)) %>%
    pivot_wider(id_cols=entry_id, names_from=nchar, values_from=segments, names_prefix="nchar_") %>%
    mutate(a=map2_chr(.x=nchar_2, .y=nchar_3, ~setdiff(.y, .x))) %>%
    select(entry_id, a)

letter_c <- signals %>%
    filter(nchar==3) %>%
    select(entry_id, seven=segments) %>%
    inner_join(signals, by="entry_id") %>%
    filter(nchar==6) %>%
    mutate(six_intersection=map2(.x=seven, .y=segments, ~intersect(.x, .y))) %>%
    filter(map_int(six_intersection, length)==2) %>%
    mutate(c=map2_chr(.x=seven, .y=six_intersection, ~setdiff(.x, .y))) %>%
    select(entry_id, c)

letter_f <- signals %>%
    filter(nchar==2) %>%
    inner_join(letter_c, by="entry_id") %>%
    mutate(f=map2_chr(.x=segments, .y=c, ~setdiff(.x, .y))) %>%
    select(entry_id, f)

# Next, I can identify the two segments that are B and D by looking at 4. And since 0 is the only six-segment number
# without both B+D, I can separate the two.
letter_b <- signals %>%
    inner_join(letter_c, by="entry_id") %>%
    inner_join(letter_f, by="entry_id") %>%
    filter(nchar==4) %>%
    mutate(four_setdiff=pmap(list(segments, c, f), ~setdiff(setdiff(..1, ..2), ..3))) %>%
    select(entry_id, four_setdiff) %>%
    inner_join(signals, by="entry_id") %>%
    filter(nchar==6) %>%
    mutate(zero_intersection=map2(.x=four_setdiff, .y=segments, ~intersect(.x, .y))) %>%
    filter(map_int(zero_intersection, length)==1) %>%
    mutate(b=unlist(zero_intersection)) %>%
    select(entry_id, b)

letter_d <- signals %>%
    filter(nchar==4) %>%
    inner_join(letter_b, by="entry_id") %>%
    inner_join(letter_c, by="entry_id") %>%
    inner_join(letter_f, by="entry_id") %>%
    mutate(d=pmap_chr(list(segments, b, c, f), ~setdiff(setdiff(setdiff(..1, ..2), ..3), ..4))) %>%
    select(entry_id, d)

# Finally, I have 2, 3, and 5, the five-segment numbers, and E and G as the only segments left to identify. G is the
# only unknown segment that is present in all three of them, which makes E the only one left.
letter_g <- signals %>%
    filter(nchar==5) %>%
    group_by(entry_id) %>%
    mutate(five_num=row_number()) %>%
    ungroup() %>%
    pivot_wider(id_cols=entry_id, names_from=five_num, values_from=segments, names_prefix="five_") %>%
    mutate(five_intersection=pmap(list(five_1, five_2, five_3), ~intersect(intersect(..1, ..2), ..3))) %>%
    inner_join(letter_a, by="entry_id") %>%
    inner_join(letter_d, by="entry_id") %>%
    mutate(g=pmap_chr(list(five_intersection, a, d), ~setdiff(setdiff(..1, ..2), ..3))) %>%
    select(entry_id, g)

letter_e <- signals %>%
    filter(nchar==7) %>%
    inner_join(letter_a, by="entry_id") %>%
    inner_join(letter_b, by="entry_id") %>%
    inner_join(letter_c, by="entry_id") %>%
    inner_join(letter_d, by="entry_id") %>%
    inner_join(letter_f, by="entry_id") %>%
    inner_join(letter_g, by="entry_id") %>%
    mutate(e=pmap_chr(list(segments, a, b, c, d, f, g),
                             ~setdiff(setdiff(setdiff(setdiff(setdiff(setdiff(..1, ..2), ..3), ..4), ..5), ..6), ..7))) %>%
    select(entry_id, e)
    
# Phew.
digits %>%
    mutate(nchar=nchar(num),
           segments=map(num, ~str_split(.x, "")[[1]])) %>%
    inner_join(letter_a, by="entry_id") %>%
    inner_join(letter_b, by="entry_id") %>%
    inner_join(letter_c, by="entry_id") %>%
    inner_join(letter_d, by="entry_id") %>%
    inner_join(letter_e, by="entry_id") %>%
    inner_join(letter_f, by="entry_id") %>%
    inner_join(letter_g, by="entry_id") %>%
    rowwise() %>%
    mutate(decoded_num=case_when(nchar==6 & !(d %in% segments) ~ 0,
                                 nchar==2 ~ 1,
                                 nchar==5 & (e %in% segments) ~ 2,
                                 nchar==5 & (c %in% segments) & (f %in% segments) ~ 3,
                                 nchar==4 ~ 4,
                                 nchar==5 & (b %in% segments) ~ 5,
                                 nchar==6 & !(c %in% segments) ~ 6,
                                 nchar==3 ~ 7,
                                 nchar==7 ~ 8,
                                 TRUE ~ 9)) %>%
    ungroup() %>%
    group_by(entry_id) %>%
    summarize(decoded_num=as.integer(paste0(decoded_num, collapse=""))) %>%
    pull(decoded_num) %>%
    sum()
