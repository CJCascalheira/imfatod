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

# Results from Web of Science
meta_wos <- read_csv("data/raw/metareview_web-science.csv")

# Results from Forward Citation Chaining
meta_fcc <- read_csv("data/raw/metareview_fcc.csv")

# Results from PubMed
pubmed <- read_csv("data/raw/pubmed_query.csv")

# Reviews in personal database
my_reviews <- read_csv("data/raw/my_reviews.csv")

# INITIAL COUNTS ----------------------------------------------------------

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

# METAREVIEW - WEB OF SCIENCE ---------------------------------------------

# Count the reasons
meta_wos %>%
  count(reason)

# METAREVIEW - FORWARD CITATION CHAINING ----------------------------------

# Count the reasons
meta_fcc %>%
  count(reason)

# METAREVIEW - PUBMED -----------------------------------------------------

# Remove duplicates in current database
pubmed_1 <- anti_join(pubmed, my_reviews, by = "DOI")

# Remove articles mentioning interventions
pubmed_2 <- pubmed_1 %>%
  mutate(
    intervention = if_else(str_detect(Title, regex("intervention|treatment", ignore_case = TRUE)), 1, 0)
  ) %>%
  filter(intervention != 1)

# Removes articles not mentioning:
pubmed_3 <- pubmed_2 %>% 
  mutate(
    # MSM
    population = if_else(str_detect(Title, regex("women|youth|adolesce|transgend", ignore_case = TRUE)), 1, 0),
    # USA
    locale = if_else(str_detect(Title, regex("Middle East|low income|brazil|latin america|pacific countr|china|united kingdom", ignore_case = TRUE)), 1, 0),
    # Substance use primarily
    std_etc = if_else(str_detect(Title, regex("HIV|hepatitis|cancer|HHV-8|tuberculo|cardiovascu|pregnancy|blood-borne", ignore_case = TRUE)), 1, 0)
  ) 
pubmed_3

# Calculation of exclusion reasons
nrow(pubmed) - nrow(pubmed_1)
nrow(pubmed_1) - nrow(pubmed_2)
pubmed_3 %>% filter(population == 1) %>% nrow()
pubmed_3 %>% filter(population != 1, locale == 1) %>% nrow()
pubmed_3 %>% filter(population != 1, locale != 1, std_etc == 1) %>% nrow()

# Export to search titles manually
pubmed_3 %>%
  filter(
    population != 1,
    locale != 1,
    std_etc != 1
  ) %>% 
  write_csv(file = "data/results/pubmed_cleaned.csv")
