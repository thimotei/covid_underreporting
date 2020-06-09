



ukRegionalSeroprevalenceEstimates <- dplyr::tibble(
  region = c("East of England", "London", "London", "London", "Midlands", "Midlands", "Midlands", "North East And Yorkshire", "North East And Yorkshire", "North West", "North West", "South East", "South West"),
  observation = c(1, 1, 2, 3, 1, 2, 3, 1, 2, 1, 2, 1, 1),
  dateStart = c("2020-05-04", "2020-03-23", "2020-04-06", "2020-04-27", "2020-03-30", "2020-04-20", "2020-05-11" , "2020-04-13", "2020-05-11", "2020-04-13", "2020-05-04", "2020-04-27", "2020-04-20"),
  dateEnd   = c("2020-05-10", "2020-03-29", "2020-04-12", "2020-05-03", "2020-04-05", "2020-04-26", "2020-05-17" , "2020-04-19", "2020-05-17", "2020-04-19", "2020-05-10", "2020-05-03", "2020-04-26"),
  observedEstimateMid = c(8.1, 1.7, 10.5, 14.8, 1, 6.2, 5, 3.9, 6.1, 5.7, 10.2, 4, 4),
  observedEstimateLow = c(10.4, 0, 8, 11.9, 0, 4.1, 2.4, 1.8, 4, 3.2, 7.9, 2, 1.8),
  observedEstimateHigh = c(5.8, 3.9, 13.9, 18.1, 3, 9.1, 7.8, 5.9, 8.4, 8, 13.7, 6, 6.2)) %>%
  dplyr::mutate(dateStart = lubridate::ymd(dateStart),
                dateEnd = lubridate::ymd(dateEnd),
                dateMid = dateStart + floor(dateEnd - dateStart)/2) %>%
  dplyr::select(region, observation, dateStart, dateMid, dateEnd, observedEstimateMid, observedEstimateLow, observedEstimateHigh)

dataInputDF <- getEnglandRegionalCumulativeIncidenceEstimates() %>%
  dplyr::rename(country = region) 
observedDataDF <- ukRegionalSeroprevalenceEstimates %>%
  dplyr::rename(country = region) %>%
  dplyr::mutate(observedEstimateMid = observedEstimateMid/100,
                observedEstimateLow = observedEstimateLow/100,
                observedEstimateHigh = observedEstimateHigh/100)


tmp <- figure2Fun(dataInputDF, observedDataDF)

tmp

dataInputDF %>%
  dplyr::group_by(country) %>%
  dplyr::filter(date == max(date)) %>%
  dplyr::select(country, cumulativeIncidenceMid, cumulativeIncidenceLow, cumulativeIncidenceHigh, population) %>%
  dplyr::ungroup(country) %>%
  dplyr::summarise(totalCumIncidenceMid  = weighted.mean(cumulativeIncidenceMid,  population)*100,
                   totalCumIncidenceLow  = weighted.mean(cumulativeIncidenceLow,  population)*100,
                   totalCumIncidenceHigh = weighted.mean(cumulativeIncidenceHigh, population)*100)
