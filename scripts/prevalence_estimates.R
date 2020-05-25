getCumulativeIncidenceEstimates <- function()
{
  here::here() %>% setwd()
  
  
  # get all test data, case data and underreporting estimates in single tibble=
  source("covid_underreporting/R/get_underreporting_and_testing_data.R")
  source("covid_underreporting/R/get_adjusted_cases_data.R")
  allUnderReportingAndTestingData <- getUnderReportingAndTestingData()
  allAdjustedCaseData <- getAdjustedCaseData()
  
  worldPopulationEstimatesRaw <- readr::read_csv("https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/CSV_FILES/WPP2019_TotalPopulationBySex.csv")
  
  worldPopulationEstimatesClean <- worldPopulationEstimatesRaw %>%
    dplyr::filter(Variant == "Medium" & Time == "2020") %>% 
    dplyr::select(country = Location, population = PopTotal) %>%
    dplyr::mutate(population = population*1000) %>%
    dplyr::mutate(country = dplyr::case_when(country == "Bolivia (Plurinational State of)" ~ "Bolivia",
                                             country != "Bolivia (Plurinational State of)" ~ country)) %>%
    dplyr::mutate(country = dplyr::case_when(country == "Iran (Islamic Republic of)" ~ "Iran",
                                             country != "Iran (Islamic Republic of)" ~ country)) %>%
    dplyr::mutate(country = dplyr::case_when(country == "Republic of Moldova" ~ "Moldova",
                                             country != "Republic of Moldova" ~ country)) %>%
    dplyr::mutate(country = dplyr::case_when(country == "Russian Federation" ~ "Russia",
                                             country != "Russian Federation" ~ country)) %>%
    dplyr::mutate(country = dplyr::case_when(country == "Sint Maarten (Dutch part)" ~ "Sint Maarten",
                                             country != "Sint Maarten (Dutch part)" ~ country)) %>%
    dplyr::mutate(country = dplyr::case_when(country == "Republic of Korea" ~ "South Korea",
                                             country != "Republic of Korea" ~ country)) %>%
    dplyr::mutate(country = dplyr::case_when(country == "Venezuela (Bolivarian Republic of)" ~ "Venezuela",
                                             country != "Venezuela (Bolivarian Republic of)" ~ country)) %>%
    rbind(c("Kosovo", 1810366))
  
  
  asymptomaticPropMid <- 0.5
  asymptomaticPropLow <- 0.23
  asymptomaticPropHigh <- 0.7
  
  
  allPrevalenceEstimates <- allAdjustedCaseData %>% 
    dplyr::left_join(worldPopulationEstimatesClean) %>% 
    dplyr::group_by(country) %>%
    dplyr::mutate(population = as.numeric(population)) %>%
    dplyr::mutate(cumulativePrevalenceMid  = cumsum(trueCasesMid)/(population*(1 - asymptomaticPropMid)),
                  cumulativePrevalenceLow  = cumsum(trueCasesLow)/(population*(1 - asymptomaticPropLow)),
                  cumulativePrevalenceHigh = cumsum(trueCasesHigh)/(population*(1 - asymptomaticPropHigh))) %>%
    dplyr::mutate(cumulativePrevalenceMid = dplyr::case_when(cumulativePrevalenceMid >= 1 ~ 1,
                                                             cumulativePrevalenceMid <= 0 ~ 0,
                                                             cumulativePrevalenceMid > 0 & cumulativePrevalenceMid < 1 ~ cumulativePrevalenceMid)) %>%
    dplyr::mutate(cumulativePrevalenceLow = dplyr::case_when(cumulativePrevalenceLow > 1 ~ 1,
                                                             cumulativePrevalenceLow < 0 ~ 0,
                                                             cumulativePrevalenceLow > 0 & cumulativePrevalenceLow < 1 ~ cumulativePrevalenceLow)) %>%
    dplyr::mutate(cumulativePrevalenceHigh = dplyr::case_when(cumulativePrevalenceHigh > 1 ~ 1,
                                                              cumulativePrevalenceHigh < 0 ~ 0,
                                                              cumulativePrevalenceHigh > 0 & cumulativePrevalenceHigh < 1 ~ cumulativePrevalenceHigh)) %>%
    dplyr::ungroup() %>%
    tidyr::drop_na()
  
  return(allPrevalenceEstimates)
  
}
  



#----------- regional data analysis, for comparison with seroprevalence results where the national estimates are a biased estimate

USRegionalData <- NCoVUtils::get_us_regional_cases()

newYorkData <- USRegionalData %>%
  dplyr::filter(state == "New York")

  
swissData <- readr::read_csv("covid_19/COVID19_Fallzahlen_CH_total_v2.csv") %>%
  dplyr::arrange(abbreviation_canton_and_fl, date) %>%
  dplyr::group_by(abbreviation_canton_and_fl) %>%
  padr::pad(by = "date")  %>%
  dplyr::arrange(abbreviation_canton_and_fl, date) 
  
#------------ making a map of the prevalence of COVID -----------------#

theme_map <- function(world = FALSE) {
  ggplot2::theme_minimal() +
    ggplot2::theme(
      text = ggplot2::element_text(family = "serif", color = "#22211d"),
      axis.line = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.title.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_line(color = "#ebebe5", size = 0),
      panel.grid.major = ggplot2::element_line(color = "#ebebe5", size = 0),
      #plot.background = ggplot2::element_rect(fill = "#f5f5f2", color = NA), 
      #panel.background = ggplot2::element_rect(fill = "#f5f5f2", color = NA), 
      #legend.background = ggplot2::element_rect(fill = "#f5f5f2", color = NA),
      panel.border = ggplot2::element_blank(),
      plot.margin = grid::unit(c(0,0,0,0), "mm"),
      legend.position = if(world == TRUE){"bottom"}
    )
}


allPrevalenceEstimatesWorldMap <- allPrevalenceEstimates %>%
  dplyr::mutate(country = stringr::str_replace_all(country, "_", " ")) %>%
  dplyr::group_by(country) %>%
  dplyr::filter(date == max(date)) %>%
  dplyr::ungroup(country) %>%
  dplyr::mutate(country = dplyr::case_when(country == "United Kingdom" ~ "UK",
                                           country != "United Kingdom" ~ country)) %>%
  dplyr::mutate(country = dplyr::case_when(country == "United States of America" ~ "USA",
                                           country != "United States of America" ~ country)) %>%
  # dplyr::mutate(country = dplyr::case_when(country == "China" ~ "China",
  #                                          country != "China" ~ country)) %>%
  dplyr::mutate(country = dplyr::case_when(country == "Congo" ~ "Republic of Congo",
                                           country != "Congo" ~ country)) %>%
  dplyr::mutate(country = dplyr::case_when(country == "Czechia" ~ "Czech Republic",
                                           country != "Czechia" ~ country)) %>%
  dplyr::mutate(country = dplyr::case_when(country == "North Macedonia" ~ "Macedonia",
                                           country != "North Macedonia" ~ country)) %>%
  dplyr::mutate(country = dplyr::case_when(country == "United Republic of Tanzania" ~ "Tanzania",
                                           country != "United Republic of Tanzania" ~ country))
  

allPrevalenceEstimatesEuropeMap1 <- allPrevalenceEstimates %>%
  dplyr::mutate(country = stringr::str_replace_all(country, "_", " ")) %>%
  dplyr::group_by(country) %>%
  dplyr::filter(date == "2020-03-30") %>%
  dplyr::ungroup(country) %>%
  dplyr::mutate(country = dplyr::case_when(country == "United Kingdom" ~ "UK",
                                           country != "United Kingdom" ~ country))

allPrevalenceEstimatesEuropeMap2 <- allPrevalenceEstimates %>%
  dplyr::mutate(country = stringr::str_replace_all(country, "_", " ")) %>%
  dplyr::group_by(country) %>%
  dplyr::filter(date == "2020-04-20") %>%
  dplyr::ungroup(country) %>%
  dplyr::mutate(country = dplyr::case_when(country == "United Kingdom" ~ "UK",
                                           country != "United Kingdom" ~ country))

allPrevalenceEstimatesEuropeMap3 <- allPrevalenceEstimates %>%
  dplyr::mutate(country = stringr::str_replace_all(country, "_", " ")) %>%
  dplyr::group_by(country) %>%
  dplyr::filter(date == max(date)) %>%
  dplyr::ungroup(country) %>%
  dplyr::mutate(country = dplyr::case_when(country == "United Kingdom" ~ "UK",
                                           country != "United Kingdom" ~ country))


europeanCountries <- c("Albania", "Andorra", "Austria", "Belarus", "Belgium",
                       "Bosnia and Herzegovina", "Bulgaria", "Croatia", "Cyprus",
                       "Czech Republic", "Denmark", "Estonia", "Finland", "France",
                       "Germany", "Greece", "Hungary","Ireland", "Isle of Man",
                       "Italy", "Kosovo", "Latvia", "Lithuania", "Luxembourg", "Moldova", 
                       "Netherlands", "Norway", "Macedonia", "Poland", "Portugal", "Romania", 
                       "Russia", "San Marino", "Slovakia", "Slovenia", "Spain", "Sweden", 
                       "Switzerland", "Turkey", "Ukraine", "UK")


worldMapData <- ggplot2::map_data("world") %>%
  dplyr::rename(country = region) %>%
  dplyr::filter(country != "Antarctica") %>%
  dplyr::left_join(allPrevalenceEstimatesWorldMap, by = "country") %>%
  #tidyr::drop_na() %>%
  dplyr::select(country, lat, long, group, country, cumulativePrevalenceMid)


EuropeMapData1 <- ggplot2::map_data("world") %>%
  dplyr::rename(country = region) %>%
  dplyr::filter(country %in% europeanCountries) %>%
  dplyr::left_join(allPrevalenceEstimatesEuropeMap1, by = "country") %>%
  #tidyr::drop_na() %>%
  dplyr::select(country, lat, long, group, country, cumulativePrevalenceMid) 


EuropeMapData2 <- ggplot2::map_data("world") %>%
  dplyr::rename(country = region) %>%
  dplyr::filter(country %in% europeanCountries) %>%
  dplyr::left_join(allPrevalenceEstimatesEuropeMap2, by = "country") %>%
  #tidyr::drop_na() %>%
  dplyr::select(country, lat, long, group, country, cumulativePrevalenceMid)


EuropeMapData3 <- ggplot2::map_data("world") %>%
  dplyr::rename(country = region) %>%
  dplyr::filter(country %in% europeanCountries) %>%
  dplyr::left_join(allPrevalenceEstimatesEuropeMap3, by = "country") %>%
  #tidyr::drop_na() %>%
  dplyr::select(country, lat, long, group, country, cumulativePrevalenceMid)


worldMapPlot <- worldMapData %>%
  ggplot2::ggplot(ggplot2::aes(x = long, y = lat, group = group)) +
  ggplot2::geom_polygon(ggplot2::aes(fill = cumulativePrevalenceMid*100)) + 
  ggplot2::geom_path(ggplot2::aes(x = long, y = lat, group = group), size = 0.3) + 
  theme_map(world = TRUE) + 
  #ggplot2::theme(legend.position = "bottom") +
  viridis::scale_fill_viridis(
    option = "magma",
    begin = 0.4, 
    end = 0.95,
    name = "Percentage of pop that have had COVID-19",
    #discrete = T,
    direction = -1,
    ) 


EuropeMapPlot1 <- EuropeMapData1 %>%
  ggplot2::ggplot(ggplot2::aes(x = long, y = lat, group = group)) +
  ggplot2::geom_polygon(ggplot2::aes(fill = cumulativePrevalenceMid*100)) + 
  ggplot2::geom_path(ggplot2::aes(x = long, y = lat, group = group), size = 0.3) + 
  ggplot2::coord_fixed(xlim = c(-9, 42.5),
                       ylim = c(36, 70.1),
                       ratio = 1.5) + 
  #ggplot2::coord_equal() + 
  ggplot2::labs(title = "20th March") + 
  theme_map(world = FALSE) + 
  viridis::scale_fill_viridis(
    option = "magma",
    begin = 0.4, 
    end = 0.95,
    name = "Percentage of pop that have had COVID-19",
    #discrete = T,
    direction = -1,
  ) +
  ggplot2::guides(fill = FALSE)


EuropeMapPlot2 <- EuropeMapData2 %>%
  ggplot2::ggplot(ggplot2::aes(x = long, y = lat, group = group)) +
  ggplot2::geom_polygon(ggplot2::aes(fill = cumulativePrevalenceMid*100)) + 
  ggplot2::geom_path(ggplot2::aes(x = long, y = lat, group = group), size = 0.3) +
  ggplot2::coord_fixed(xlim = c(-9, 42.5),
                       ylim = c(36, 70.1),
                       ratio = 1.5) + 
  ggplot2::labs(title = "20th April") + 
  theme_map(world = FALSE) + 
  viridis::scale_fill_viridis(
    option = "magma",
    begin = 0.4, 
    end = 0.95,
    name = "Percentage of pop that have had COVID-19",
    #discrete = T,
    direction = -1,
  ) +
  ggplot2::guides(fill = FALSE)



EuropeMapPlot3 <- EuropeMapData3 %>%
  ggplot2::ggplot(ggplot2::aes(x = long, y = lat, group = group)) +
  ggplot2::geom_polygon(ggplot2::aes(fill = cumulativePrevalenceMid*100)) + 
  ggplot2::geom_path(ggplot2::aes(x = long, y = lat, group = group), size = 0.3) + 
  ggplot2::coord_fixed(xlim = c(-9, 42.5),
                       ylim = c(36, 70.1),
                       ratio = 1.5) + 
  ggplot2::labs(title = "10th May") + 
  theme_map(world = FALSE) + 
  viridis::scale_fill_viridis(
    option = "magma",
    begin = 0.4, 
    end = 0.95,
    name = "Percentage of pop that have had COVID-19",
    #discrete = T,
    direction = -1,
  ) + 
  ggplot2::guides(fill = FALSE)



library(patchwork)
wholePlot <- worldMapPlot + (EuropeMapPlot1/EuropeMapPlot2/EuropeMapPlot3) +  plot_layout(widths = c(6, 1))


ggplot2::ggsave("covid_underreporting/figures/prevalenceTwoPanels.png",
                wholePlot, 
                width  = 500,
                height = 240,
                units = "mm")



