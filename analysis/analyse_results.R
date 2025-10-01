#' Analysis of results from the Google Form to determine the best meeting
#' times for the CAB
#'

library(tidyverse)
library(timechange)
library(googlesheets4)

## Setup attendances for weighted voting
attendances <- here::here("data", "attendance_summary_2024.rds") %>%
  read_rds()
## Load the proposals
proposals <- c("current", "ny_fixed", "western", "empty_space")
schedules <- proposals %>%
  paste0(".rds") %>%
  lapply(\(x) here::here("data", x)) %>%
  lapply(read_rds) %>%
  lapply(pluck, "data") %>%
  setNames(nm = proposals)
## Load the results & restructure
form <- "https://docs.google.com/spreadsheets/d/1DKpROdmDwrxjOFwq3Yg7UUNeHoCEXduPO-LEJxTEqdw/edit?usp=sharing"
results <- read_sheet(form)
form_tbl <- results %>%
  dplyr::select(-Timestamp, -Comments) %>%
  dplyr::rename(
    current = `The Current Meeting Schedule`,
    ny_fixed = `Fixing Start Times at 9am for US/Eastern`,
    western = `Starting at 6am for the Western-most Members`,
    empty_space = `Placing 1–5am In The 'Empty' Time Zones`
  ) %>%
  mutate(
    ny_fixed = suppressWarnings(vapply(ny_fixed, as.integer, integer(1))),
    ny_fixed = ifelse(is.na(ny_fixed), 0L, ny_fixed),
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
      x %>%
        mutate(
          member = str_remove_all(member, "\\*"),
          w = fct_rev(status) %>% as.integer()
        ) %>%
        summarise(
          w = sum(w), .by = member
        ) %>%
        mutate(w = w / sum(w))
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
# proposal        n
# <chr>       <dbl>
# 1 empty_space  3.52
# 2 western      3.21
# 3 ny_fixed     3.13
# 4 current      3.07

## Look at just these repeonses
form_tbl %>% dplyr::filter(proposal == "empty_space") %>% arrange(est)
## Check the schedule/plot
here::here("data", "empty_space.rds") %>% read_rds()
