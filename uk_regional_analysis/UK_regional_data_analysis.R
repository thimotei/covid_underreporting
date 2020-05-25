#reportingEstimateMid <- 0.16
#reportingEstimateLow <- 0.13
#reportingEstimateHigh <- 0.19

httr::GET("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", httr::authenticate(":", ":", type="ntlm"), httr::write_disk(tf <- tempfile(fileext = ".csv")))
allDatRaw <- readr::read_csv(tf) %>%
  dplyr::rename(date = dateRep, 
                country = countriesAndTerritories,
                countryCode = countryterritoryCode) %>%
  dplyr::mutate(date = lubridate::dmy(date))

countryCodesLookUp <- allDatRaw %>%
  dplyr::select(country, 
                countryCode) %>% 
  unique()


data_path <- "~/Dropbox/bayesian_underreporting_estimates/current_estimates/"
files <- dir(path = data_path,
             pattern = "*.rds")

dataTmp <- dplyr::tibble(countryCode = files) %>% 
  dplyr::mutate(file_contents = purrr::map(countryCode, 
                                           ~ readRDS(file.path(data_path, .)))
                
  ) %>% 
  tidyr::unnest(cols = c(file_contents)) %>%
  dplyr::mutate(countryCode = stringr::str_remove(countryCode, "result_")) %>% 
  dplyr::mutate(countryCode = stringr::str_remove(countryCode, ".rds")) %>%
  dplyr::group_by(countryCode) %>%
  dplyr::mutate(date = seq(Sys.Date() - 13 - dplyr::n()  + 1, Sys.Date() - 13, 1)) %>% 
  dplyr::select(date, everything()) %>%
  dplyr::left_join(countryCodesLookUp) %>%
  dplyr::select(date, country, countryCode, everything()) %>%
  dplyr::group_by(countryCode) %>%
  dplyr::ungroup()

UKunderReportingTimeSeries <- dataTmp %>%
  dplyr::filter(country == "United_Kingdom")


asymptomaticProp <- 1/0.7

ukRegionalCases <- NCoVUtils::get_uk_regional_cases()

ukRegionalPopulations <- c(5295000,
                           1891100,
                           3139000,
                           5479615, 
                           9133625,
                           5599735,
                           6201214,
                           8908081,
                           5900757,
                           4804149,
                           7292093,
                           2657909)


underReportingTimeseries <- UKunderReportingTimeSeries %>% 
  dplyr::left_join(ukRegionalCases, by = "date") %>%
  dplyr::select(date, country = country.y, region, cases, estimate, lower, upper) %>% 
  dplyr::group_by(region) %>%
  dplyr::arrange(region, date)


regionalPopulations <- dplyr::tibble(region = ukRegionalCases$region %>% unique(),
                                     population = ukRegionalPopulations)

ukRegionalCasesScaled <- underReportingTimeseries %>%
  dplyr::mutate(scaledCasesMid  = (cases/estimate) %>% round(),
                scaledCasesLow  = (cases/upper) %>% round(),
                scaledCasesHigh = (cases/lower) %>% round()) %>% 
  dplyr::left_join(regionalPopulations) %>%
  dplyr::mutate(propInfectedMid  = (scaledCasesMid/estimate)*100,
                propInfectedLow  = (scaledCasesLow/upper)*100,
                propInfectedHigh = (scaledCasesHigh/lower)*100) %>% 
  dplyr::group_by(country, region) %>%
  dplyr::mutate(propCumInfectedMid  = (cumsum(scaledCasesMid*asymptomaticProp)/population)*100,
                propCumInfectedLow  = (cumsum(scaledCasesLow*asymptomaticProp)/population)*100,
                propCumInfectedHigh = (cumsum(scaledCasesHigh*asymptomaticProp)/population)*100) %>%
  dplyr::group_by(country, region)


p1 <- ukRegionalCasesScaled %>%
  dplyr::group_by(country) %>%
  dplyr::group_by(region) %>%
  dplyr::filter(date <= Sys.Date() - 4) %>%
  ggplot2::ggplot(ggplot2::aes(x = date, color = region, group = region)) +
  ggplot2::geom_line(ggplot2::aes(y = scaledCasesMid)) + 
  ggplot2::geom_line(ggplot2::aes(y = scaledCasesLow),  alpha = 0.4) + 
  ggplot2::geom_line(ggplot2::aes(y = scaledCasesHigh), alpha = 0.4) +
  ggplot2::facet_wrap(~region, scale = "free") +
  ggplot2::geom_vline(xintercept = Sys.Date(), linetype = 4, color = "red") +
  ggplot2::xlab("Date") + 
  ggplot2::ylab("New adjusted symptomatic cases")+
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90)) +
  ggplot2::expand_limits(x = Sys.Date())

ggplot2::ggsave(
  "covid_underreporting/figures/new_cases_scaled_regional_UK.pdf",
  p1,
  scale = 1,
  width = 12,
  height = 6.7,
  units = "in",
  dpi = 300
)

p2 <- ukRegionalCasesScaled %>%
  dplyr::group_by(country) %>%
  dplyr::group_by(region) %>%
  dplyr::filter(date <= Sys.Date() - 4) %>%
  ggplot2::ggplot(ggplot2::aes(x = date, color = region, group = region)) +
  ggplot2::geom_line(ggplot2::aes(y = propCumInfectedMid)) + 
  ggplot2::geom_line(ggplot2::aes(y = propCumInfectedLow),  alpha = 0.4) + 
  ggplot2::geom_line(ggplot2::aes(y = propCumInfectedHigh), alpha = 0.4) +
  ggplot2::facet_wrap(~region, scale = "free") +
  ggplot2::geom_vline(xintercept = Sys.Date(), linetype = 4, color = "red") +
  ggplot2::xlab("Date") + 
  ggplot2::ylab("Percentage of population infected")+
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90)) +
  ggplot2::expand_limits(x = Sys.Date())

ggplot2::ggsave(
  "covid_underreporting/figures/cumulative_scale_prop_infected_regional_UK.pdf",
  p2,
  scale = 1,
  width = 12,
  height = 6.7,
  units = "in",
  dpi = 300
)

p3 <- ukRegionalCasesScaled %>%
  dplyr::group_by(country) %>%
  dplyr::group_by(region) %>%
  ggplot2::ggplot(ggplot2::aes(x = date, color = region, group = region)) +
  ggplot2::geom_col(ggplot2::aes(y = cases)) +
  ggplot2::facet_wrap(~region, scale = "free") +
  ggplot2::geom_vline(xintercept = Sys.Date(), linetype = 4, color = "red") +
  ggplot2::xlab("Date") + 
  ggplot2::ylab("Raw symptomatic case data")+
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90)) +
  #ggplot2::expand_limits(x = Sys.Date()) + 
  ggplot2::xlim(Sys.Date() - 60, Sys.Date())

ggplot2::ggsave(
  "covid_underreporting/figures/uk_cases_regional_raw.pdf",
  p3,
  scale = 1,
  width = 12,
  height = 6.7,
  units = "in",
  dpi = 300
)

