library(tidyverse)

box_ids <- readr::read_lines("data/day02_1_input.txt")

# What is the checksum for your list of box IDs?
#box_ids <- c("abcdef",  "bababc",  "abbcde",  "abcccd", "aabcdd", "abcdee", "ababab")
letter <- letters[1:26]

df <- crossing(box_ids, letter) %>%
      mutate(number_letter = str_count(box_ids, letter))


exactly_two_times <- df %>%
                    filter(number_letter == 2) %>%
                    distinct(box_ids)


exactly_three_times <- df %>%
                       filter(number_letter == 3) %>%
                       distinct(box_ids)

check_sum <- nrow(exactly_two_times) * nrow(exactly_three_times)

print(check_sum)


# What letters are common between the two correct box IDs? (In the example above, this is found by removing the differing character from either ID, producing fgij.)
# box_ids <- <- c("abcde", "fghij", "klmno", "pqrst", "fguij", "axcye", "wvxyz")
box_id_origin1 <- box_ids
box_id_origin2 <- box_id_origin1

# All Combinations of Box Ids without the Same-Same-Combination
df <- crossing(box_id_origin1, box_id_origin2) %>%
      filter(box_id_origin1 != box_id_origin2)

# Function for Removing the yth-Character in a String

remove_y_character <- function(x, y){
  
  # Works only for a Vector x of strings with equal number of characters
  nchar_string <- max(nchar(x))
  
  if(y == 1) {
    z <- str_sub(x, 2, nchar_string)
  } else if(y == nchar_string){
    z <- str_sub(x, 1, nchar_string - 1)
  } else{
    z <- paste0(str_sub(x, 1, y - 1) , str_sub(x, y+1, nchar_string))
  }
  
  return(z)
}

# For Loop over all characters
for(i in 1:26){
  
  dd <- df %>%
        mutate(id1 = remove_y_character(box_id_origin1, i),
               id2 = remove_y_character(box_id_origin2, i),
               removed_character = i) %>%
        mutate(equal_box_id = ifelse(id1 == id2, TRUE, FALSE)) %>%
        filter(equal_box_id == TRUE)
  
  print(paste0("Removed Character: ", i))
  if(nrow(dd) > 0){
    print(dd)  
  } else{
    print("No Match")
  }
  
}
