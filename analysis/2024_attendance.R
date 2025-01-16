library(tidyverse)

eligible <- c(
  Aedin = 12, Daniela = 9, Enis = 12, Estefania = 9, Hedia = 12, Janani = 12,
  Jiefei = 12, Johannes = 12, Jordana = 9,Kevin = 12, Kozo = 12, Leo = 12,
  Lori = 12, Luyi = 12, Maria = 12, Mengbo = 12, Mike = 9, Nicole = 12,
  Stevie = 12, Umar = 12, Xueyi = 12, Jasmine = 3, Lluís = 3, Tobi = 3,
  Zahraa = 3
)

attendances <- list(
    Jan = "Kozo, Kevin, Johannes, Jiefei, Stevie, Janani, Mengbo, Aedin",
    Feb = "Mengbo, Maria, Lori, Xueyi, Leo, Kevin, Stevie, Mengbo, Aedin, Johannes",
    Mar = "Kozo, Maria, Stevie, Kevin, Jiefei, Xueyi, Johannes",
    Apr = "Kozo, Maria, Stevie, Kevin, Jiefei, Xueyi, Johannes",
    May = "Johannes, Kozo, Lori, Janani, Stevie, Maria, Leo, Xueyi, Aedin",
    Jun = "Aedin, Janani, Kevin, Leo, Lori, Maria, Mengbo, Mike, Stevie",
    Jul = "Aedin, Johannes, Kevin, Kozo, Lori, Maria, Mengbo, Mike, Stevie, Xueyi",
    Aug = "Maria, Johannes, Kevin, Xueyi, Nicole, Janani, Leo, Jiefei",
    Sep = "Lori, Enis, Mengbo, Janani, Johannes, Stevie, Leo",
    Oct = "Lori, Leo, Mengbo, Jasmine, Lluís, Tobi, Maria, Janani, Hedia, Kozo, Umar, Zahraa, Johannes, Stevie, Kevin",
    Nov = "Kozo, Lori, Zahraa, Johannes, Aedin, Tobi, Janani, Lluís, Stevie, Leo, Kevin",
    Dec = "Lori, Maria, Kozo, Xueyi, Lluís, Hedia"
)
attendance_summary <- attendances %>%
  lapply(str_split, ",") %>%
  lapply(unlist) %>%
  lapply(\(x) tibble(member = x)) %>%
  bind_rows(.id = "Month") %>%
  mutate(
    member = str_trim(member) %>% factor(levels = names(eligible)),
  ) %>%
  summarise(n_2024 = dplyr::n(), .by = member) %>%
  complete(member = names(eligible), fill = list(n_2024 = 0)) %>%
  dplyr::filter(member %in% names(which(eligible != 9))) %>%
  mutate(
    eligible_2024 = eligible[as.character(member)],
    prop_attended = n_2024 / eligible_2024
  )
attendance_summary %>%
  # dplyr::filter(prop_attended <= 1/12)
  arrange(prop_attended) %>%
  print(n = nrow(.))
write_rds(attendance_summary, here::here("data", "attendance_summary_2024.rds"), compress = "gz")
