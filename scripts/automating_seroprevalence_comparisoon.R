allSerologyStudies <- readr::read_csv("covid_underreporting/data/serology_studies.csv") %>%
  dplyr::rename(two_letter_code = Country) %>%
  dplyr::mutate(two_letter_code = dplyr::case_when(two_letter_code == "UK" ~ "GB",
                                                   two_letter_code != "UK" ~ two_letter_code))


countryCodes <- readr::read_csv("covid_underreporting/data/country_codes.csv")


tmp <- allSerologyStudies %>%
  dplyr::left_join(countryCodes)

tmp %>% View()
