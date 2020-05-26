#---------- regional seroprevalence estimates -------------------#

getRegionalUnderReportingData <- function()
{
  
  asymptomaticPropMid <- 0.5
  asymptomaticPropLow <- 0.45
  asymptomaticPropHigh <- 0.55
  
  
  populationData <- dplyr::tibble(countryCode = c("GEN", "NYC", "WUH"),
                                  population  = c(499480, 19453561, 8896900))
  
  regionalCaseDeathTS <- getRegionalCaseDeathTimeSeries() %>%
    dplyr::rename(countryCode = iso_code)
  
  regionalEstimates <- regionalHPCBayesianData() %>%
    dplyr::select(-date)
  
  dateRange <- regionalCaseDeathTS %>%
    dplyr::group_by(countryCode) %>%
    dplyr::summarise(minDate = min(date),
                     maxDate = max(date)) %>%
    dplyr::left_join(populationData)
  
  
  regionalUnderreportingAndRawData %>%
    dplyr::group_by(countryCode) %>%
    dplyr::summarise(minDate = min(date),
                     maxDate = max(date)) %>%
    dplyr::left_join(populationData)
  
  
  regionalUnderreportingAndRawData <- regionalEstimates %>%
    dplyr::left_join(dateRange) %>%
    dplyr::group_by(countryCode) %>%
    dplyr::mutate(date = seq(unique(maxDate) - 13 - dplyr::n() + 1, unique(maxDate) - 13, 1)) %>%
    dplyr::select(date, countryCode, estimate, lower, upper, population) %>%
    dplyr::left_join(regionalCaseDeathTS) %>%
    dplyr::select(-country) %>%
    tidyr::drop_na() %>%
    dplyr::group_by(countryCode) %>%
    dplyr::mutate(trueCasesMid  = smooth::sma(new_cases/estimate)$fitted,
                  trueCasesLow  = smooth::sma(new_cases/upper)$fitted,
                  trueCasesHigh = smooth::sma(new_cases/lower)$fitted) %>%
    dplyr::mutate(cumulativeIncidenceMid  = cumsum(trueCasesMid)/(population*(1 - asymptomaticPropMid)),
                  cumulativeIncidenceLow  = cumsum(trueCasesLow)/(population*(1 - asymptomaticPropLow)),
                  cumulativeIncidenceHigh = cumsum(trueCasesHigh)/(population*(1 - asymptomaticPropHigh)))
 
  
  return(regionalUnderreportingAndRawData)
  
}