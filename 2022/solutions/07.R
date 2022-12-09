########################################################################################################################
# Advent of Code 2022: Day 7                                                                                           #
#                                                                                                                      #
# Author: Dan Lependorf                                                                                                #
# Date: 2022-12-07                                                                                                     #
########################################################################################################################

library(tidyverse)

data <- read_lines("./2022/inputs/07.txt")

########################################################################################################################
# Part One                                                                                                             #
########################################################################################################################

# ...The only way I can think of doing this is to loop across every line and build a complicated file system parsing
# thing that builds out everything as it explores. Hoo boy.

# But first, can I make this any easier on myself? It seems worthwhile to parse this into a table where each row is each
# command, in order, and a separate list-column contains the output (if any) for that command.
parsed_data <- tibble(data=data) %>%
    mutate(command_id=cumsum(str_detect(data, "^\\$"))) %>%
    group_by(command_id) %>%
    summarize(output=list(data), .groups="drop") %>%
    # Now that I have all of the output for each command in a list-column, I can extract the first element from it,
    # since that will always be the command that generated the rest of the output.
    mutate(command=map_chr(output, ~pluck(.x, 1)),
           command=str_remove(command, "^\\$\\s"),
           # And now I can remove that first entry from the rest of the output.
           output=map(output, ~tail(.x, length(.x)-1))) %>%
    # Let's parse this further. First, split the commands from their arguments...
    separate(command, into=c("command", "argument"), sep="\\s", fill="right") %>%
    # ...and then the outputs from their file sizes. I'm also going to replace any "dir" references to 0, since it'll
    # make the integer conversion easier.
    mutate(output=map(output, ~str_replace_all(.x, "dir", "0")),
           output=map(output, ~tibble(files=.x)),
           output=map(output, ~separate(.x, files, into=c("file_size", "file_name"), sep="\\s")),
           output=map(output, ~mutate(.x, file_size=as.integer(file_size))))
    
# Alright, let's do this.
i <- 1
file_system <- list()

# The first thing is to figure out what the current command is referring to. I'll need to keep track of the name of the
# current object and the names of every object it's nested in. I'm also going to make a list that I'll fill with paths
# to all directories I find.
reference <- c()
directories <- list()

for (i in seq_len(nrow(parsed_data))) {
    command <- parsed_data$command[[i]]
    argument <- parsed_data$argument[[i]]
    output <- parsed_data$output[[i]]
    
    # Let's deal with the commands first. There are three I need to figure out how to deal with, ls, cd .. (up), and cd
    # (down).
    if (command=="ls") {
        
        for (j in seq_len(nrow(output))) {
            file_name <- output$file_name[[j]]
            file_size <- output$file_size[[j]]
            
            if (file_size==0) {
                file_system[[c(reference, output$file_name[[j]])]] <- list()
            } else {
                file_system[[c(reference, output$file_name[[j]])]] <- file_size
            }
        }

    } else if (command=="cd" & argument=="..") {
        reference <- head(reference, -1)
    } else if (command=="cd") {
        reference <- c(reference, argument)
        file_system[[reference]] <- list()
        directories <- c(directories, list(reference))
    }
}

# But we're not done yet! Now I need to find all of the directories (recursively!) that contain file sizes less than or
# equal than 100000. Thankfully, I can feed each directory I've written down as indexes to file_system to compute a sum
# of everything inside.
directory_sizes <- tibble(directories) %>%
    mutate(total_file_size=map_int(directories, ~sum(unlist(file_system[[.x]]))))

directory_sizes %>%
    filter(total_file_size <= 100000) %>%
    pull(total_file_size) %>%
    sum()

########################################################################################################################
# Part Two                                                                                                             #
########################################################################################################################

# Thank god part two is easy with what I've already mapped here. How much room do I need to make?
new_free_space_needed <- 30000000 - (70000000 - directory_sizes$total_file_size[[1]])
    
directory_sizes %>%
    filter(total_file_size >= new_free_space_needed) %>%
    arrange(total_file_size) %>%
    slice(1) %>%
    pull(total_file_size)
