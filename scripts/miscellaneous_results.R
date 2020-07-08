#--- miscellaneous results reported in the main text

here::here() %>% setwd()

library(patchwork)
source("covid_underreporting/R/data_helper_functions.R")
source("covid_underreporting/R/plotting_helper_functions.R")
source("covid_underreporting/R/plotting_functions.R")

#--- increase in new cases after adjusting for top 10 countries
#--- the top 10 changes when considering new_cases or new_cases_adjusted_mid
#--- so we find both
all_adjusted_case_data <- getAdjustedCaseDataNational()

top_10_countries <- all_adjusted_case_data %>%
  dplyr::group_by(country) %>%
  dplyr::summarise(total_cases = sum(new_cases)) %>%
  dplyr::top_n(10) %>%
  dplyr::pull(country)

top_10_countries_adjusted <- all_adjusted_case_data %>%
  dplyr::group_by(country) %>%
  dplyr::summarise(total_cases_adjusted = sum(new_cases_adjusted_mid)) %>%
  dplyr::top_n(10) %>%
  dplyr::pull(country)

top_10_new_cases_table <- all_adjusted_case_data %>%
  dplyr::filter(country %in% top_10_countries) %>%
  dplyr::group_by(country) %>%
  dplyr::filter(new_cases == max(new_cases)) %>%
  dplyr::mutate(scaling_factor = new_cases_adjusted_mid/new_cases) %>%
  dplyr::select(country, date, new_cases, new_cases_adjusted_mid, new_cases_adjusted_low, new_cases_adjusted_high, scaling_factor)

top_10_new_cases_adjusted_table <- all_adjusted_case_data %>%
  dplyr::filter(country %in% top_10_countries_adjusted) %>%
  dplyr::group_by(country) %>%
  dplyr::filter(new_cases_adjusted_mid == max(new_cases_adjusted_mid)) %>%
  dplyr::mutate(scaling_factor = new_cases_adjusted_mid/new_cases) %>%
  dplyr::select(country, date, new_cases, new_cases_adjusted_mid, new_cases_adjusted_low, new_cases_adjusted_high, scaling_factor)

top_10_new_cases_table %>%
  dplyr::full_join(top_10_new_cases_adjusted_table) %>%
  dplyr::arrange(country)

#--- running correlation between under-ascertainment and testing

testing_data <- getFigure1TestData()
under_reporting_data <- getFigure1UnderReportingData()

under_reporting_and_testing <- under_reporting_data %>%
  dplyr::left_join(testing_data) %>%
  tidyr::drop_na()

cor.test(under_reporting_and_testing$estimate, under_reporting_and_testing$testing_effort, method = "kendall")

#--- early march testing comparison

testing_data %>%
  dplyr::filter(date > "2020-03-06" & date < "2020-03-19") %>% 
  dplyr::filter(country %in% countries_in_europe_testing_march) %>%
  dplyr::summarise(testing_effort_mean = mean(testing_effort))

countries_in_europe_testing_march <- c("Austria", "Belgium", "Croatia", "Czechia", "Denmark",
                                       "Estonia", "Finland", "Hungary", "Iceland", "Italy",
                                       "Latvia", "Norway", "Poland", "Portugal", "Romania", 
                                       "Slovenia", "Switzerland")

#--- global and European under-ascertainment results summaries

# global estimates
world_min <- under_reporting_data %>% 
  dplyr::filter(date > "2020-03-01" & date < "2020-03-31") %>%
  dplyr::filter(estimate == min(estimate))

world_max <- under_reporting_data %>% 
  dplyr::filter(date > "2020-03-01" & date < "2020-03-31") %>%
  dplyr::filter(estimate == max(estimate))

world <- rbind(world_min, world_max)

# european estimates
europeanCountries <- c("Albania", "Andorra", "Austria", "Belarus", "Belgium",
                       "Bosnia and Herzegovina", "Bulgaria", "Croatia", "Cyprus",
                       "Czech Republic", "Denmark", "Estonia", "Finland", "France",
                       "Germany", "Greece", "Hungary","Ireland", "Isle of Man",
                       "Italy", "Kosovo", "Latvia", "Lithuania", "Luxembourg", "Moldova", 
                       "Netherlands", "Norway", "Macedonia", "Poland", "Portugal", "Romania", 
                       "Russia", "San Marino", "Slovakia", "Slovenia", "Spain", "Sweden", 
                       "Switzerland", "Turkey", "Ukraine", "UK")

europe_min_march_ascertainment <- under_reporting_data %>% 
  dplyr::filter(date > "2020-03-01" & date < "2020-03-31") %>%
  dplyr::filter(country %in% europeanCountries) %>%
  dplyr::filter(estimate == min(estimate))

europe_max_march_ascertainment <- under_reporting_data %>% 
  dplyr::filter(date > "2020-03-01" & date < "2020-03-31") %>%
  dplyr::filter(country %in% europeanCountries) %>%
  dplyr::filter(estimate == max(estimate))

europe_march_ascertainment <- rbind(europe_min_march_ascertainment, europe_max_march_ascertainment)

# now for april

europe_min_april_ascertainment <- under_reporting_data %>% 
  dplyr::filter(date > "2020-04-01" & date < "2020-04-30") %>%
  dplyr::filter(country %in% europeanCountries) %>%
  dplyr::filter(estimate == min(estimate))

europe_max_april_ascertainment <- under_reporting_data %>% 
  dplyr::filter(date > "2020-04-01" & date < "2020-04-30") %>%
  dplyr::filter(country %in% europeanCountries) %>%
  dplyr::filter(estimate == max(estimate))

europe_april_ascertainment <- rbind(europe_min_april_ascertainment, europe_max_april_ascertainment)

# now for may

europe_min_may_ascertainment <- under_reporting_data %>% 
  dplyr::filter(date > "2020-05-01" & date < "2020-05-31") %>%
  dplyr::filter(country %in% europeanCountries) %>%
  dplyr::filter(estimate == min(estimate))

europe_max_may_ascertainment <- under_reporting_data %>% 
  dplyr::filter(date > "2020-05-01" & date < "2020-05-31") %>%
  dplyr::filter(country %in% europeanCountries) %>%
  dplyr::filter(estimate == max(estimate))

europe_may_ascertainment <- rbind(europe_min_may_ascertainment, europe_max_may_ascertainment)

#--- same monthly temporally averaged but ranged over countries estimates for testing

europe_min_march_test <- testing_data %>% 
  dplyr::filter(date > "2020-03-01" & date < "2020-03-31") %>%
  dplyr::filter(country %in% europeanCountries) %>%
  dplyr::filter(testing_effort == min(testing_effort))

europe_max_march_test <- testing_data %>% 
  dplyr::filter(date > "2020-03-01" & date < "2020-03-31") %>%
  dplyr::filter(country %in% europeanCountries) %>%
  dplyr::filter(testing_effort == max(testing_effort))

europe_march_test <- rbind(europe_min_march_test, europe_max_march_test)

# now for april

europe_min_april_test <- testing_data %>% 
  dplyr::filter(date > "2020-04-01" & date < "2020-04-30") %>%
  dplyr::filter(country %in% europeanCountries) %>%
  dplyr::filter(testing_effort == min(testing_effort))

europe_max_april_test <- testing_data %>% 
  dplyr::filter(date > "2020-04-01" & date < "2020-04-30") %>%
  dplyr::filter(country %in% europeanCountries) %>%
  dplyr::filter(testing_effort == max(testing_effort))

europe_april_test <- rbind(europe_min_april_test, europe_max_april_test)

# now for may

europe_min_may_test <- testing_data %>% 
  dplyr::filter(date > "2020-05-01" & date < "2020-05-31") %>%
  dplyr::filter(country %in% europeanCountries) %>%
  dplyr::filter(testing_effort == min(testing_effort))

europe_max_may_test <- testing_data %>% 
  dplyr::filter(date > "2020-05-01" & date < "2020-05-31") %>%
  dplyr::filter(country %in% europeanCountries) %>%
  dplyr::filter(testing_effort == max(testing_effort))

europe_may_test <- rbind(europe_min_may_test, europe_max_may_test)



