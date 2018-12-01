library(tidyverse)

# Starting with a frequency of zero, what is the resulting frequency after all of the changes in frequency have been applied?
starting_value <- 0
frequency_changes <- as.numeric(readr::read_lines("data/day01_1_input.txt"))
resulting_frequency <- starting_value + sum(number_changes)

print(paste0("The resulting frequency is: ", resulting_frequency))


#frequency_changes <- c(1, -2 , 3, 1, 1, -2)

# What is the first frequency your device reaches twice? Note that your device might need to repeat its list of frequency changes many times before a duplicate frequency is found, and that duplicates might be found while in the middle of processing the list.
number_repeats <- 1000
df <- data.frame(step = seq(1:(length(frequency_changes)*number_repeats)),
                 frequency_changes = rep(frequency_changes, number_repeats)) %>%
      mutate(cum_sum = starting_value + cumsum(frequency_changes))

# Identify all first occurences
dd <- df %>%
      group_by(cum_sum) %>%
      summarise(step = first(step)) %>%
      ungroup() %>%
      mutate(first_occurence = TRUE)

# Remove all first occurences
dr <- df %>%
      left_join(dd, by = c("cum_sum", "step")) %>%
      filter(is.na(first_occurence))

print(head(dr))
  

