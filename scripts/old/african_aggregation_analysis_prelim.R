#---------- rough African aggregated estimates -----------------#

allUnderReportingAndTestingData <- getUnderReportingAndTestingData()

africanCountriesWeHave <- c("Burkina Faso", "Cote dIvoire", "Cameroon", "Democratic Republic of the Congo",
                            "Congo", "Algeria", "Ghana", "Guinea", "Kenya", "Liberia", "Morocco", 
                            "Mali", "Mauritius", "Niger", "Nigeria", "Sudan", "Senegal", "Sierra Leone", 
                            "Somalia", "Chad", "Togo", "Tunisia", "United Republic of Tanzania", "South Africa")


allUnderReportingData %>%
  dplyr::filter(country %in% africanCountriesWeHave) %>%
  dplyr::filter(date == max(date)) %>% 
  dplyr::select(countryCode, estimate, lower, upper) %>%
  dplyr::mutate(underReportingEstimateMid = signif(estimate*100, 2),
                underReportingEstimateLow = signif(lower*100, 2),
                underReportingEstimateHigh = signif(upper*100, 2)) %>%
  dplyr::select(countryCode, underReportingEstimateMid, underReportingEstimateLow, underReportingEstimateHigh) %>%
  dplyr::summarise(mean(underReportingEstimateMid), 
                   mean(underReportingEstimateLow),
                   mean(underReportingEstimateHigh))

allUnderReportingData %>%
  dplyr::filter(country %in% africanCountriesWeHave) %>%
  dplyr::filter(date == max(date)) %>%
  dplyr::summarise(mean(lower))

allUnderReportingData %>%
  dplyr::filter(country %in% africanCountriesWeHave) %>%
  dplyr::filter(date == max(date)) %>%
  dplyr::summarise(mean(upper))

allUnderReportingData %>%
  dplyr::filter(date == max(date)) %>%
  dplyr::summarise(mean(estimate))