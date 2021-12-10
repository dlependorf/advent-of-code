########################################################################################################################
# Advent of Code 2021: Day 9                                                                                           #
#                                                                                                                      #
# Author: Dan Lependorf                                                                                                #
# Date: 2021-12-09                                                                                                     #
########################################################################################################################

library(tidyverse)

data <- read_table("./2021/inputs/09.txt", col_names="raw")

########################################################################################################################
# Part One                                                                                                             #
########################################################################################################################

# I've done this trick before, but I want to convert this grid into a tidy long format, where each number has its own
# row, with row_id and column_id fields.
heightmap <- data %>%
    separate(raw, into=paste0("column_", 0:100), sep="") %>%
    select(-column_0) %>%
    mutate(row=row_number()) %>%
    pivot_longer(starts_with("column"), names_to="column", values_to="number", names_prefix="column_",
                 names_transform=list(column=as.integer), values_transform=list(number=as.integer))

# What I need to do here is find each numbers lag + lead numbers for both rows + columns. If all four of those are
# higher than the number itself, it's a low point.
heightmap %>%
    group_by(row) %>%
    mutate(row_lag=lag(number, n=1),
           row_lead=lead(number, n=1)) %>%
    ungroup() %>%
    group_by(column) %>%
    mutate(column_lag=lag(number, n=1),
           column_lead=lead(number, n=1)) %>%
    ungroup() %>%
    rowwise() %>%
    filter(min(row_lag, row_lead, column_lag, column_lead, na.rm=TRUE) > number) %>%
    ungroup() %>%
    mutate(risk_level=number+1) %>%
    pull(risk_level) %>%
    sum()

########################################################################################################################
# Part Two                                                                                                             #
########################################################################################################################

# Essentially what I have to do here is remove all of the 9s, run a hill climbing algorithm until I hit a blank spot (an
# edge or a removed 9), remove that entire section of numbers, and repeat again with the next available non-9 number.
# This isn't going to be all that efficient, but we're only dealing with 10,000 numbers here.
basins <- tibble(basin_id=integer(),
                 row=integer(),
                 column=integer())

data_to_loop <- filter(heightmap, number != 9)

basin_id <- 1

while (nrow(data_to_loop) > 0) {
    basin_to_check <- slice(data_to_loop, 1)
    
    current_size_of_basin <- 0

    while (current_size_of_basin != nrow(basin_to_check)) {
        current_size_of_basin <- nrow(basin_to_check)
        
        basin_to_check <- data_to_loop %>%
            # This is actually a cross join, but since that doesn't exist in dplyr (not exactly), a full join on nothing
            # is the same thing.
            full_join(basin_to_check, by=character(), suffix=c("", "_basin")) %>%
            # Only keep rows where the row or the column is the same as one in the basin...
            filter(row==row_basin | column==column_basin) %>%
            # ...but also any row where the difference in row/column is 1. This gives me all basin points next to a
            # point I know to be in the current basin, excluding diagonals.
            filter(abs(row-row_basin)==1 |abs(column-column_basin)==1) %>%
            select(row, column, number) %>%
            bind_rows(basin_to_check, .) %>%
            distinct()
    }
    
    data_to_loop <- anti_join(data_to_loop, basin_to_check, by=c("row", "column", "number"))
    
    basins <- basin_to_check %>%
        mutate(basin_id=basin_id) %>%
        select(basin_id, row, column) %>%
        bind_rows(basins, .)
    
    basin_id <- basin_id + 1
}

basins %>%
    group_by(basin_id) %>%
    summarize(basin_size=n()) %>%
    arrange(desc(basin_size)) %>%
    filter(row_number() <= 3) %>%
    pull(basin_size) %>%
    prod()
