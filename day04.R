
library(tidyverse)

# Strategy 1: Find the guard that has the most minutes asleep. What minute does that guard spend asleep the most?
# What is the ID of the guard you chose multiplied by the minute you chose? 

guards_input <- data.frame(raw_input = readr::read_lines("data/day04_1_input.txt"), stringsAsFactors = FALSE)
#guards_input <- data.frame(raw_input = readr::read_lines("data/day04_1_example.txt"), stringsAsFactors = FALSE)


guards_df <- guards_input %>% 
             separate(raw_input, into = c('date', 'time', 'event'), sep = c(12, 19)) %>%
             mutate(date = as.Date(str_remove(str_trim(date), "\\[")),
                    time = str_remove(str_trim(time), "\\]"),
                    event = str_trim(event)) %>%
             separate(time, into = c('hour', 'minute'), sep = ':', remove = FALSE) %>%
             mutate(timestamp = lubridate::ymd_hm(paste0(date, "_", time)),
                    hour = as.numeric(hour),
                    minute = as.numeric(minute)) %>%
             arrange(timestamp)


guards_add <- guards_df %>%
              mutate(guard_id = as.integer(str_remove(str_remove(event, 'Guard #'),' begins shift'))) %>%
              mutate(state = ifelse(event == 'falls asleep', "sleeping", 
                                    ifelse(event == 'wakes up', "awake", "shift"))) %>%
              fill(guard_id)
              


date <- seq(min(guards_df$date), max(guards_df$date), by = "days")
minute <- seq(0,59)


df <- crossing(date, minute) %>%
      left_join(guards_add %>% filter(state != 'shift'),
                by = c("date", "minute")) %>%
      select(date, minute, guard_id, state) %>%
      fill(guard_id, state)

# Find the guard that has the most minutes asleep.

by_guard <- 
  df %>%
  filter(state == 'sleeping') %>%
  count(guard_id) %>%
  arrange(desc(n))

sleeping_guard <- by_guard$guard_id[1]

by_minute <- 
  df %>%
  filter(state == 'sleeping',
         guard_id == sleeping_guard) %>%
  count(minute) %>%
  arrange(desc(n))

sleeping_minute <- by_minute$minute[1]

print(paste0("Guard ", sleeping_guard, " sleeps most in minute ", sleeping_minute, ". Solution is: ", sleeping_guard * sleeping_minute))


# Strategy 2: Of all guards, which guard is most frequently asleep on the same minute?

by_guard_minute <- 
  df %>%
  filter(state == 'sleeping') %>%
  count(guard_id, minute) %>%
  arrange(desc(n))

sleeping_guard <- by_guard_minute$guard_id[1]
sleeping_minute <- by_guard_minute$minute[1]

print(paste0("Guard ", sleeping_guard, " sleeps most frequently in minute ", sleeping_minute, ". Solution is: ", sleeping_guard * sleeping_minute))


