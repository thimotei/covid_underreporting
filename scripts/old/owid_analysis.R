owidRawDat <- readr::read_csv("~/Downloads/owid-covid-data.csv")

countriesOfInterest <- c("Germany", 
                         "Iceland", 
                         "Iran",
                         "Italy",
                         "United States")

cfr <- owidRawDat %>% 
  dplyr::group_by(location) %>% 
  dplyr::mutate(cfr = total_deaths/total_cases*100) %>%
  tidyr::drop_na(cfr)

testsPerCaseDF <- owidRawDat %>% 
  dplyr::mutate(testsPerCase = new_tests/new_cases) %>%
  #tidyr::drop_na(testsPerCase) %>% 
  #dplyr::filter(testsPerCase != "Inf") %>% 
  dplyr::group_by(location)

p1 <- cfr %>% 
  dplyr::filter(location %in% countriesOfInterest) %>%
  dplyr::filter(date >= "2020-02-26") %>%
  ggplot2::ggplot(ggplot2::aes(date, cfr, color = location, group = location)) +
  ggplot2::geom_line() + 
  ggplot2::xlab("Date") + 
  ggplot2::ylab("nCFR") 
  #ggplot2::facet_wrap(~location, scales = "free")

p4 <- testsPerCaseDF %>% 
  dplyr::filter(location %in% "Germany") %>%
  #dplyr::filter(date >= "2020-03-01") %>%
  ggplot2::ggplot(ggplot2::aes(date, log(testsPerCase), color = location, group = location)) +
  ggplot2::geom_line() + 
  ggplot2::xlab("Date") + 
  ggplot2::ylab("New tests per new cases") 


testsPerCaseDF %>% 
  dplyr::filter(location %in% "United Kingdom") %>%
  ggplot2::ggplot(ggplot2::aes(date, testsPerCase, color = location, group = location)) +
  ggplot2::geom_line() + 
  ggplot2::xlab("Date") + 
  ggplot2::ylab("New tests per new cases") 


















