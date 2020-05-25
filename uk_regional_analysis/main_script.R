here::here() %>% setwd()

NHSRegionCaseData <- NCoVUtils::get_uk_nhs_region_cases()

underReportingEstimateMid  <- 0.034
underReportingEstimateLow  <- 0.028
underReportingEstimateHigh <- 0.077


NHSRegionCaseDataAdjusted <- NHSRegionCaseData %>% 
  dplyr::mutate(casesMid  = cases/underReportingEstimateMid,
                casesLow  = cases/underReportingEstimateLow,
                casesHigh = cases/underReportingEstimateHigh) %>%
  dplyr::select(-cases) %>%
  dplyr::group_by(region) %>% 
  dplyr::summarise(casesMid = sum(casesMid),
                   casesLow = sum(casesLow),
                   casesHigh = sum(casesHigh))

CCGtoRegionLookup <- readr::read_csv("covid_underreporting/data/ccg_april_2020.csv") %>% 
  dplyr::mutate(CCGName = ccg_name) %>% 
  dplyr::select(-ccg_name)

NHSdata1 <- readr::read_csv("covid_underreporting/data/111 Online Covid-19 data_CCG mapped.csv") %>%
  dplyr::mutate(CCGName = `April20 mappedCCGName`) %>% 
  dplyr::select(journeydate, CCGName, Total)  %>% 
  dplyr::left_join(CCGtoRegionLookup) %>%
  dplyr::group_by(nhser_name) %>%
  dplyr::summarise(cases = sum(Total)) %>%
  dplyr::rename(region = nhser_name)

NHSdata2 <- readr::read_csv("covid_underreporting/data/NHS Pathways Covid-19 data CCG mapped.csv") %>%
  dplyr::mutate(CCGName = `April20 mapped CCGName`) %>% 
  dplyr::select(`Call Date`, CCGName, TriageCount)  %>% 
  dplyr::left_join(CCGtoRegionLookup) %>%
  dplyr::group_by(nhser_name) %>%
  dplyr::summarise(cases = sum(TriageCount)) %>%
  dplyr::rename(region = nhser_name)


allNHSDat <- dplyr::tibble(region = NHSdata1$region,
                        casesCallsInternet = NHSdata1$cases + NHSdata2$cases)

allDatTogether <- allNHSDat %>% dplyr::left_join(NHSRegionCaseDataAdjusted)


ggplot2::ggplot(allDatTogether, ggplot2::aes(x = casesCallsInternet, y = casesMid, colour = factor(allNHSDat$region))) +
  ggplot2::geom_point() +
  ggplot2::labs(
    x = "NHS 111 and 999 Triages",
    y = "Adjusted case data",
    colour = "Region") 
# + ggplot2::geom_text(label = rownames(allNHSDat))

