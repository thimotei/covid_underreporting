prevalenceEstimate <- 0.154
popNYC <- 8175133

prevalenceRawCasesEstimate <- prevalenceEstimate * popNYC

weeksBeforeStudy <- c(0, 1, 2, 3)
reportedCases <- c(122080, 92789, 63488, 31001)

reportedCases <- dplyr::tibble(weeksBeforeStudy, reportedCases)

ourReportingEstimateMid <- 0.12
ourReportingEstimateLow <- 0.11
ourReportingEstimateHigh <- 0.26

trueSymptomaticEstimates <- reportedCases %>% 
  dplyr::mutate(trueSymptomaticEstimateMid = reportedCases*(1/ourReportingEstimateMid) %>% round(),
                trueSymptomaticEstimateLow = reportedCases*(1/ourReportingEstimateHigh) %>% round(),
                trueSymptomaticEstimateHigh = reportedCases*(1/ourReportingEstimateLow) %>% round())

asymtomaticEstimates <- c(0.4, 0.5, 0.6, 0.7)


trueSymptomaticEstimates %>%
  dplyr::group_by(weeksBeforeStudy) %>%
  dplyr::mutate(trueSymptomaticEstimateMid*asymtomaticEstimates,
                trueSymptomaticEstimateLow*asymtomaticEstimates,
                trueSymptomaticEstimateHigh*asymtomaticEstimates)
  

tmp1 <- trueSymptomaticEstimates %>% 
  dplyr::filter(weeksBeforeStudy == 0)

tmp2 <- trueSymptomaticEstimates %>% 
  dplyr::filter(weeksBeforeStudy == 1)

tmp3 <- trueSymptomaticEstimates %>%
  dplyr::filter(weeksBeforeStudy == 2)

tmp4 <- trueSymptomaticEstimates %>% 
  dplyr::filter(weeksBeforeStudy == 3)


zeroWeeksBefore <- dplyr::tibble(mid = tmp1$trueSymptomaticEstimateMid * 1/(1 - asymtomaticEstimates),
                                 low = tmp1$trueSymptomaticEstimateLow * 1/(1 - asymtomaticEstimates),
                                 high = tmp1$trueSymptomaticEstimateHigh * 1/(1 - asymtomaticEstimates)) %>% round()

oneWeekBefore <- dplyr::tibble(mid = tmp2$trueSymptomaticEstimateMid * 1/(1 - asymtomaticEstimates),
                               low = tmp2$trueSymptomaticEstimateLow * 1/(1 - asymtomaticEstimates),
                               high = tmp2$trueSymptomaticEstimateHigh * 1/(1 - asymtomaticEstimates)) %>% round()

twoWeeksBefore <- dplyr::tibble(mid = tmp3$trueSymptomaticEstimateMid * 1/(1 - asymtomaticEstimates),
                                low = tmp3$trueSymptomaticEstimateLow * 1/(1 - asymtomaticEstimates),
                                high = tmp3$trueSymptomaticEstimateHigh * 1/(1 - asymtomaticEstimates)) %>% round()

threeWeeksBefore <- dplyr::tibble(mid = tmp4$trueSymptomaticEstimateMid * 1/(1 - asymtomaticEstimates),
                                  low = tmp4$trueSymptomaticEstimateLow * 1/(1 - asymtomaticEstimates),
                                  high = tmp4$trueSymptomaticEstimateHigh * 1/(1 - asymtomaticEstimates)) %>% round()

                            