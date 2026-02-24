library(tidyverse)

## Stretch back 12 months from Feb 2026
eligible <- c(
  Aedin = 12, Eliana = 5, Enis = 12, Fabricio = 5, Izabela = 5, Johannes = 12,
  Kevin = 12, Kozo = 12, Laurent = 5, Lori = 12, Maria = 12, Martha = 5,
  Mengbo = 12, Stevie = 12, Umar = 12, Lluís = 6, Tobi = 12, Tuomas = 5,
  Zahraa = 12, Zuguang = 5
)

attendances <- list(
    Mar = "Zahraa, Tobi, Lori, Janani, Stevie, Jasmine, Mengbo, Lluís, Leo, Kevin",
    Apr = "Lori, Kevin, Laurent, Lluís, Johannes, Stevie, Helena, Enis, Vince, Zahraa, Kozo, Xueyi, Dario, Erdal, Tim, Ludwig, Leo, Jacques, Wolfgang, Charlotte, Kasper, Sehyun, Aedin, Levi, Stephanie, Toby, Js, Alex, Andres",
    May = "Kozo, Maria, Lori, Johannes, Lluís, Enis, Stevie, Zahraa, Janani, Hedia, Tobi",
    Jun = "Lori, Stevie, Kozo, Kevin, Zahraa, Johannes, Janani, Maria, Aedin, Xueyi, Leo",
    Jul = "Kozo, Kevin, Lori, Maria, Stevie, Aedin, Tobi, Zahraa",
    Aug = "Stevie, Kozo, Janani, Maria",
    Sep = "Lori, Enis, Stevie, Tobi, Maria, Johannes, Aedin, Janani, Zahraa, Kevin, Hedia, Leo",
    ## New CAB starts
    Oct = "Lori, Eliana, Laurent, Stevie, Kozo, Zuguang, Maria, Izabela, Johannes, Kevin, Martha, Aedin, Leo, Tuomas",
    Nov = "Lori, Laurent, Kevin, Izabela, Zuguang, Tuomas, Eliana, Umar, Enis, Stevie, Maria, Zahraa, Aedin, Kozo, Martha",
    Dec = "Lori, Laurent, Kevin, Izabela, Johannes, Fabricio, Zuguang, Tobi, Kozo, Maria, Zahraa, Aedin",
    Jan = "Laurent, Eliana, Tuomas, Kevin, Stevie, Kozo, Izabela, Nicholas, Enis, Maria, Tobi, Lori, Lluís",
    Feb = "Kozo, Lluís, Lori, Izabela, Kevin, Laurent, Johannes, Tuomas, Stevie, Eliana, Martha, Zahraa, Fabricio"
)
attendance_summary <- attendances %>%
  lapply(str_split, ",") %>%
  lapply(unlist) %>%
  lapply(\(x) tibble(member = x)) %>%
  bind_rows(.id = "Month") %>%
  mutate(
    member = str_trim(member) %>% factor(levels = names(eligible)),
  ) %>%
  summarise(n_2025 = dplyr::n(), .by = member) %>%
  complete(member = names(eligible), fill = list(n_2025 = 0)) %>%
  dplyr::filter(member %in% names(which(eligible != 9))) %>%
  mutate(
    eligible_2025 = eligible[as.character(member)],
    prop_attended = pmin(n_2025 / eligible_2025, 1) # Laurent attended the TAB/CAB
  )
attendance_summary %>%
  # dplyr::filter(prop_attended <= 1/12)
  arrange(prop_attended) %>%
  print(n = nrow(.))
write_rds(attendance_summary, here::here("data", "attendance_summary_2025.rds"), compress = "gz")
