source("covid_underreporting/scripts/bayesian_model_outputs.R")


countriesOfInterest <- c("Germany", 
                        "Iceland", 
                        "Iran",
                        "Italy",
                        "United States of America")

dateOfTestStart <- c("2020-04-01", 
                     "2020-03-13",
                     "2020-03-15", 
                     "2020-02-23", 
                     "2020-03-13")

dateOfTestEnd <- c("2020-04-13", 
                   "2020-04-01",
                   "2020-03-20", 
                   "2020-03-08", 
                   "2020-03-27")


periodOfInterest <- 
  dplyr::tibble(country   = countriesOfInterest,
                dateStart = lubridate::ymd(dateOfTestStart), 
                dateEnd   = lubridate::ymd(dateOfTestEnd))

allData <- periodOfInterest %>% 
  dplyr::left_join(dataCountriesOfInterest)

dataCountriesOfInterest <- dataCasesAndEstimates %>% 
  dplyr::filter(country %in% countriesOfInterest) %>%
  # dplyr::full_join(icelandDat, by = c("country", "date")) %>%
  dplyr::group_by(country) %>% 
  dplyr::filter(date >= dateStart & date <= dateEnd) %>% 
  dplyr::select(country, date, cases, deaths, estimate, lower, upper)

dataCountriesOfInterestFullTimePeriod <- allData %>% 
  dplyr::filter(country %in% countriesOfInterest) %>%
  # dplyr::full_join(icelandDat, by = c("country", "date")) %>%
  dplyr::group_by(country) %>% 
  #dplyr::filter(date >= dateStart & date <= dateEnd) %>% 
  dplyr::select(country, date, cases, deaths, estimate, lower, upper)

datTmp <- allData %>% 
  dplyr::filter(country == "Iran") %>%
  dplyr::filter(date <=  "2020-03-28")

datTmp <- datTmp %>% 
  dplyr::mutate(estimateCasesMid  = round(cases/(estimate*0.5)),
                estimateCasesLow  = round(cases/(upper*0.5)),
                estimateCasesHigh = round(cases/(lower*0.5)))


datTmp %>% dplyr::summarise(sum(estimateCasesMid))
datTmp %>% dplyr::summarise(sum(estimateCasesLow))
datTmp %>% dplyr::summarise(sum(estimateCasesHigh))

casesPlot <- dataCountriesOfInterest %>%
  ggplot2::ggplot(ggplot2::aes(x = date, group = country)) +
  ggplot2::xlab("Date") +
  ggplot2::ylab("New cases each day") +
  ggplot2::geom_col(ggplot2::aes(y = cases)) + 
  ggplot2::facet_wrap(~country, scales = "free", ncol = 2) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(size = 7, angle = 90),
                 axis.text.y = ggplot2::element_text(size = 7))

deathsPlot <- dataCountriesOfInterest %>%
  ggplot2::ggplot(ggplot2::aes(x = date, group = country)) +
  ggplot2::xlab("Date") +
  ggplot2::ylab("New cases each day") +
  ggplot2::geom_col(ggplot2::aes(y = deaths)) + 
  ggplot2::facet_wrap(~country, scales = "free", ncol = 2) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(size = 7, angle = 90),
                 axis.text.y = ggplot2::element_text(size = 7))



dataCountriesOfInterestFullTimePeriod %>%
  dplyr::group_by(country) %>%
  ggplot2::ggplot(ggplot2::aes(x = date)) +
  ggplot2::xlab("Date") +
  ggplot2::ylab("Percentage of symptomatic cases reported") +
  ggplot2::facet_wrap(~country, scales = "free", ncol = 2) +
  #ggplot2::geom_col(ggplot2::aes(y = deaths)) +
  ggplot2::geom_smooth(ggplot2::aes(y = estimate*100), color = "#440154FF", alpha = 0.8) +
  ggplot2::geom_ribbon(ggplot2::aes(ymin = lower*100,
                                    ymax = upper*100), fill = "#440154FF", alpha = 0.3) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(size = 7, angle = 90),
                 axis.text.y = ggplot2::element_text(size = 7),
                 legend.position = "none")
