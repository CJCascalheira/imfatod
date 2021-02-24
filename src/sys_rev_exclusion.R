# Dependencies
library(tidyverse)
library(rio)

# Import
sys_rev_links <- import("data/raw/sys_rev.xlsx") %>%
  as_tibble()
sys_rev_links

sys_rev_counts <- import("data/raw/sys_rev_1.xlsx") %>%
  as_tibble()
sys_rev_counts

# Count exclusion reasons by individual articles
sys_rev_links %>%
  count(reason)

# What does NA mean?
sys_rev_links %>%
  filter(is.na(reason))

# What does other mean?
sys_rev_links %>%
  filter(reason == "other" | reason == "check notes")

# Count exclusion reasons by summation
sys_rev_counts %>%
  group_by(reason) %>%
  summarize(
    sum = sum(link)
  )

# What does other mean?
sys_rev_counts %>%
  filter(reason == "other")
