# Code to fit GAMs to time-series of under-reporting estimates

# Set paths
here::here() %>% setwd()

#source data processing and plotting scripts
source("CFR_calculation/global_estimates/temporal/R/cfr_plot_theme.R")
source("CFR_calculation/global_estimates/temporal/R/get_one_timeseries.R")
source("CFR_calculation/global_estimates/temporal/R/get_plot_data.R")
source("CFR_calculation/global_estimates/temporal/R/plot_country.R")
source("CFR_calculation/global_estimates/temporal/R/scale_CFR_temporal.R")


# Set parameters
zMean <- 13
zSD <- 12.7
zMedian <- 9.1
mu <- log(zMedian)
sigma <- sqrt(2*(log(zMean) - mu))

deathUnderreporting <- 1.95

# set baseline level CFR
cCFRBaseline <- 1.4
cCFREstimateRange <- c(1.2, 1.7)

# Hospitalisation to death distribution
hospitalisation_to_death_truncated <- function(x) {
  plnorm(x + 1, mu, sigma) - plnorm(x, mu, sigma)
}


# Load data -----------------------------------------------------
httr::GET("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", httr::authenticate(":", ":", type="ntlm"), httr::write_disk(tf <- tempfile(fileext = ".csv")))
allDat <- readr::read_csv(tf)


allTogether <- allDat %>% 
  dplyr::arrange(countriesAndTerritories, dateRep) %>% 
  dplyr::mutate(dateRep = lubridate::dmy(dateRep))%>% 
  dplyr::rename(date = dateRep, new_cases = cases, new_deaths = deaths, country = countriesAndTerritories) %>%
  dplyr::select(date, country, new_cases, new_deaths) %>%
  dplyr::filter(!country %in% c("CANADA", "Cases_on_an_international_conveyance_Japan")) %>%
  dplyr::group_by(country) %>%
  padr::pad() %>%
  dplyr::mutate(new_cases = tidyr::replace_na(new_cases, 0),
                new_deaths = tidyr::replace_na(new_deaths, 0)) %>%
  #What is this doing?
  dplyr::group_by(country) %>%
  dplyr::mutate(new_deaths = round(new_deaths*deathUnderreporting)) %>%
  dplyr::mutate(cum_deaths = cumsum(new_deaths)) %>%
  dplyr::filter(cum_deaths > 0) %>%
  dplyr::filter(country == "United_Kingdom")
  
# under_reporting_timeseries <- get_plot_data(country_name = "United_Kingdom") %>%
#   dplyr::mutate(total_deaths = cumsum(deaths)) %>%
#   dplyr::mutate(cCFR_UQ = binom.test(total_deaths, cases_known)$conf.int[2],
#                 cCFR_LQ = binom.test(total_deaths, cases_known)$conf.int[1],
#                 lower = cCFREstimateRange[1] / (100 * cCFR_UQ),
#                 upper = cCFREstimateRange[2] / (100 * cCFR_LQ))

under_reporting_timeseries <- get_plot_data(country_name = "United_Kingdom")

tmpDF <- under_reporting_timeseries %>% 
  dplyr::mutate(temp = binom.test(cumsum(deaths), cases_known)$conf.int[2])


tmp1 <- under_reporting_timeseries$deaths %>% cumsum()
tmp2 <- under_reporting_timeseries$cases_known
