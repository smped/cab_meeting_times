#' Analysis of results from the Google Form to determine the best meeting
#' times for the CAB
#'

library(tidyverse)
library(timechange)
library(googlesheets4)
library(ggtext)

## Setup attendances for weighted voting
attendances <- here::here("data", "attendance_summary_2025.rds") %>%
  read_rds()
## Load the proposals
proposals <- c("ny_fixed", "favouring_two", "western", "empty_space")
schedules <- proposals %>%
  paste0(".rds") %>%
  lapply(\(x) here::here("data", x)) %>%
  lapply(read_rds) %>%
  lapply(pluck, "data") %>%
  setNames(nm = proposals)
## Load the results & restructure
form <- "hhttps://docs.google.com/spreadsheets/d/1YKcn5mUJnACEr5cdddQetKc0GoUtAkR5tkBitZw2z6s/edit?usp=sharing"
results <- read_sheet(form)
form_tbl <- results %>%
  dplyr::select(-Timestamp, -Comments) %>%
  dplyr::rename(
    favouring_two = `Favouring Two Time Zones: Americas/Europe then Europ/Asia`,
    ny_fixed = `Fixing Times at 12pm UTC`,
    western = `Shifting Every 4 Months (As previously)`,
    empty_space = `Placing Middle Of The Night Meetings In the Largest Geographical Gaps`
  ) %>%
  mutate(
    member = str_extract(`Full Name`, "^[^ ]+") %>%
      str_replace_all("Oluwatobilola", "Tobi"),
  ) %>%
  left_join(attendances) %>%
  dplyr::select(member, prop_attended, all_of(proposals)) %>%
  pivot_longer(
    cols = all_of(proposals), names_to = "proposal", values_to = "est"
  )
## Do the analysis & get the results
schedules %>%
  lapply(
    \(x) {
      ## Weight votes by how unpleasant they are for the member
      x %>%
        mutate(
          member = str_remove_all(member, "\\*"),
          w = fct_rev(status) %>% as.integer()
        ) %>%
        summarise(
          w = sum(w), .by = member
        ) %>%
        mutate(w = w / sum(w)) # Sum to one
    }
  ) %>%
  bind_rows(.id = "proposal") %>%
  inner_join(form_tbl) %>%
  mutate(weighted_attendance = w * prop_attended * est) %>%
  summarise(
    n = sum(weighted_attendance), .by = proposal
  ) %>%
  arrange(desc(n))
# # A tibble: 4 × 2
# proposal          n
# <chr>         <dbl>
# 1 empty_space    3.82
# 2 ny_fixed       3.47
# 3 western        3.23
# 4 favouring_two  2.97

## Look at just these responses
form_tbl %>% dplyr::filter(proposal == "empty_space") %>% arrange(est)
## Check the schedule/plot
here::here("data", "empty_space.rds") %>%
  read_rds()
