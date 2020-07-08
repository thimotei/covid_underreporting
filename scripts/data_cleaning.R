#--- data cleaning and reducing
#--- the flight data files are too large to be commited to github
#--- so we process them and save the data we need for this analysis separately

here::here() %>% setwd()

source("covid_travel_restrictions/R/flight_data_cleaning_utils.R")
source("covid_travel_restrictions/R/data_helper_functions.R")
source("covid_travel_restrictions/R/plotting_helper_functions.R")

open_sky_may_2019   <- readr::read_csv("covid_travel_restrictions/data/raw_data/open_sky_may_2019.gz")
open_sky_may_2020   <- readr::read_csv("covid_travel_restrictions/data/raw_data/open_sky_may_2020.gz")


airport_lookup <- read.delim("covid_travel_restrictions/data/airports.dat", sep = ",") %>%
  dplyr::select(country = Country, three_letter_code, four_letter_code, lat, long)

flight_data_clean_may_2019   <- clean_flight_data(open_sky_may_2019)
flight_data_clean_may_2020   <- clean_flight_data(open_sky_may_2020)


total_flights_may_2019 <- flight_data_clean_may_2019 %>%
  dplyr::filter(origin_country != destination_country) %>%
  dplyr::group_by(origin_country, destination_country) %>%
  dplyr::summarise(total_flights_2019 = dplyr::n()) %>%
  dplyr::mutate(origin_country_iso_code = countrycode::countrycode(origin_country, "country.name", "iso3c"),
                destination_country_iso_code = countrycode::countrycode(destination_country, "country.name", "iso3c")) %>%
  dplyr::ungroup() %>%
  dplyr::select(origin_country_iso_code, destination_country_iso_code, total_flights_2019)

total_flights_may_2020 <- flight_data_clean_may_2020 %>%
  dplyr::filter(origin_country != destination_country) %>%
  dplyr::group_by(origin_country, destination_country) %>%
  dplyr::summarise(total_flights_2020 = dplyr::n()) %>%
  dplyr::mutate(origin_country_iso_code = countrycode::countrycode(origin_country, "country.name", "iso3c"),
                destination_country_iso_code = countrycode::countrycode(destination_country, "country.name", "iso3c")) %>%
  dplyr::ungroup() %>%
  dplyr::select(origin_country_iso_code, destination_country_iso_code, total_flights_2020)


total_flights_may_2019_2020 <- total_flights_may_2019 %>% 
  dplyr::ungroup() %>%
  dplyr::left_join(total_flights_may_2020) %>%
  dplyr::mutate(scaling_factor = total_flights_2020/total_flights_2019) %>%
  dplyr::mutate(scaling_factor = dplyr::case_when(scaling_factor > 1  ~ 1,
                                                  scaling_factor <= 1 ~ scaling_factor)) %>%
  dplyr::select(origin_country_iso_code, destination_country_iso_code, scaling_factor) %>%
  tidyr::drop_na()


oag_traveller_data <- readr::read_csv("covid_travel_restrictions/data/raw_data/Kathy flight_data_all_2019_monthly.csv")

oag_traveller_data_may_2020 <- oag_traveller_data %>%
  dplyr::filter(month == 5) %>%
  dplyr::select(origin_country_iso_code_2 = dep_country_code, destination_country_iso_code_2 = arr_country_code, bookings) %>%
  dplyr::mutate(origin_country_iso_code   = countrycode::countrycode(origin_country_iso_code_2, "iso2c", "iso3c"),
                destination_country_iso_code = countrycode::countrycode(destination_country_iso_code_2, "iso2c", "iso3c")) %>%
  dplyr::select(origin_country_iso_code, destination_country_iso_code, bookings) %>%
  dplyr::group_by(origin_country_iso_code, destination_country_iso_code) %>%
  dplyr::summarise(total_passengers = sum(bookings)) %>%
  dplyr::select(origin_country_iso_code, destination_country_iso_code, total_passengers)


readr::write_csv(total_flights_may_2019_2020, "covid_travel_restrictions/data/flight_reduction_scaling_factors.csv")
readr::write_csv(oag_traveller_data_may_2020, "covid_travel_restrictions/data/oag_data_may_2019_2020")
