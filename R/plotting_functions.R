#---------- reading the case and death estimates        ----------#
#---------- from ECDC and the under-reporting estimates ----------#
#---------- (from the shared Dropbox) in                ----------#


#---------------------- figure 1 uses under-reporting and testing data function
figure1Fun <- function(data, countryArg, tweak1, tweak2)
{
  
  plotDat <- data %>%
    dplyr::filter(country == countryArg)

  yLimitsTestingDat <- plotDat %>%
    dplyr::summarise(yMin = min(testing_effort),
                     yMax = max(testing_effort))
  

  yLimitsTesting <- c(yLimitsTestingDat$yMin - tweak1, yLimitsTestingDat$yMax + tweak2)
  
  yLimitsUnderreportingDat <- plotDat %>%
    dplyr::summarise(yMin = min(lower),
                     yMax = max(upper))
  
  
  yLimitsUnderreporting <- c(yLimitsUnderreportingDat$yMin - tweak1, yLimitsUnderreportingDat$yMax + tweak2)
  

  plot(plotDat$date, plotDat$testing_effort,
       type = "l",
       lty = 2, 
       lwd = 1,
       xlab = "",
       ylab = "",
       main = countryArg,
       ylim = yLimitsTesting,
       axes = FALSE)
  axis(side = 2, ylim = yLimitsTesting, col = "black", las = 1)
  box()
  
  
  par(new = TRUE)

  #--------------- plotting the second data set, the under-reporting estimate over time
  plot(plotDat$date, plotDat$estimate, 
       type = "l",
       las = 1,
       xlab = "", 
       ylab = "",
       bty = "n",
       axes = FALSE,
       ylim = yLimitsUnderreporting)
  polygon(x = c(plotDat$date, rev(plotDat$date)),
          y = c(plotDat$lower, rev(plotDat$upper)),
          col =  adjustcolor("dodgerblue", alpha.f = 0.30), border = NA)


  axis(side = 4, ylim = yLimitsUnderreporting, col = "black", las = 1)
  
  # making the time axis
  tickPositions = seq(min(plotDat$date), max(plotDat$date), length.out = 4)
  axis.Date(side = 1, plotDat$date, at = tickPositions, srt = 20)

}

figure1FunNoTesting <- function(data, countryArg, tweak1, tweak2)
{
  
  plotDat <- data %>%
    dplyr::filter(country == countryArg)
  
  yLimitsUnderreportingDat <- plotDat %>%
    dplyr::summarise(yMin = min(lower),
                     yMax = max(upper))
  
  
  yLimitsUnderreporting <- c(yLimitsUnderreportingDat$yMin - tweak1, yLimitsUnderreportingDat$yMax + tweak2)

  
  #--------------- plotting the second data set, the under-reporting estimate over time
  plot(plotDat$date, plotDat$estimate, 
       type = "l",
       las = 1,
       xlab = "", 
       ylab = "",
       #bty = "n",
       xaxt='n',
       yaxt='n',
       #axes = FALSE,
       main = countryArg,
       ylim = yLimitsUnderreporting)
  polygon(x = c(plotDat$date, rev(plotDat$date)),
          y = c(plotDat$lower, rev(plotDat$upper)),
          col =  adjustcolor("dodgerblue", alpha.f = 0.30), border = NA)
  
  axis(side = 4, ylim = yLimitsUnderreporting, col = "black", las = 1)
  
  # making the time axis
  tickPositions = seq(min(plotDat$date), max(plotDat$date), length.out = 4)
  axis.Date(side = 1, plotDat$date, at = tickPositions, srt = 20)
  
}

#------------------- function for producing the serology figure

figure2Fun <- function()
{
  
  # making national plot
  allAdjustedCaseData <- getAdjustedCaseDataNational()  
  
  country_codes <- readr::read_csv("covid_underreporting/data/country_codes.csv")
  
  serology_study_results <- readr::read_csv("covid_underreporting/data/national serology estimates.csv") %>%
    dplyr::rename(two_letter_code = iso_code) %>%
    dplyr::left_join(country_codes, by = "two_letter_code") %>%
    dplyr::rowwise() %>%
    dplyr::mutate(percentage_positive_mid = percentage_positive_mid/100,
                  lower_ci = signif(binom_min(positive_samples, sample_size), 2)/100,
                  upper_ci = signif(binom_max(positive_samples, sample_size), 2)/100) %>%
    dplyr::rename(iso_code = three_letter_code)
  
  
  all_countries_with_serology <- serology_study_results %>%
    dplyr::pull(iso_code) %>%
    unique() 
  
  # regional estimates required for these countries
  # we are concentrating on USA, UK and others we can find regional data for
  countries_to_exclude <- c("AUT", "CHN", "FRA", "IRN", "SVN", "USA", "CHE", "JPN")
  
  countries_of_interest_with_serology <- setdiff(all_countries_with_serology, countries_to_exclude)

  adjusted_cases_and_serology <- allAdjustedCaseData %>%
    dplyr::left_join(serology_study_results, by = "iso_code") %>%
    dplyr::filter(iso_code %in% countries_of_interest_with_serology)
  
  plot_national <- adjusted_cases_and_serology %>%
    dplyr::group_by() %>%
    ggplot2::ggplot() +
    ggplot2::geom_ribbon(ggplot2::aes(x = date, ymin = cumulative_incidence_low, ymax = cumulative_incidence_high), alpha = 0.3, colour = NA, fill = "dodgerblue") +
    ggplot2::geom_point(ggplot2::aes(x = sample_end_date - 13, y = percentage_positive_mid)) +
    ggplot2::geom_errorbar(ggplot2::aes(x = sample_end_date - 13, ymin = lower_ci, ymax = upper_ci), 
                           width = 0.2) +
    ggplot2::geom_vline(ggplot2::aes(xintercept = sample_end_date), linetype = "dashed") +
    ggplot2::labs(title = "A",
                  x = "", y = "Cumulative incidence (%)") + 
    ggplot2::theme(
    axis.title.x = ggplot2::element_blank()) +
    ggplot2::scale_y_continuous(labels = scales::percent) +
    ggplot2::facet_wrap(~country, scales = "free_y") + 
    ggplot2::theme_minimal()
  
  regional_data <- getAdjustedRegionalCaseAndSerologyData() %>%
    dplyr::mutate(region = dplyr::case_when(region == "North East and Yorkshire" ~ "North East",
                                            region != "North East and Yorkshire" ~ region))
  
  main_plot_regions <- c("Geneva", "London", "New York")
  all_regions <- regional_data %>% 
    dplyr::pull(region) %>% unique()
  
  remaining_regions <- setdiff(all_regions, main_plot_regions)
  
  plot_regional <- regional_data %>%
    dplyr::filter(region %in% main_plot_regions) %>%
    dplyr::group_by(region) %>%
    ggplot2::ggplot() +
    ggplot2::geom_ribbon(ggplot2::aes(x = date_infection, ymin = cumulative_incidence_low, ymax = cumulative_incidence_high), alpha = 0.3, colour = NA, fill = "dodgerblue") +
    ggplot2::geom_point(ggplot2::aes(x = sample_end_date - 13, y = percentage_positive_mid)) +
    ggplot2::geom_errorbar(ggplot2::aes(x = sample_end_date - 13, ymin = lower_ci, ymax = upper_ci), 
                           width = 0.2) +
    ggplot2::geom_vline(ggplot2::aes(xintercept = sample_end_date), linetype = "dashed") +
    ggplot2::labs(title = "B",
         x ="", y = "") + 
    ggplot2::theme(
      axis.title.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank()) +
    ggplot2::scale_y_continuous(labels = scales::percent) +
    ggplot2::facet_wrap(~region, scales = "free_y") + 
    ggplot2::theme_minimal()
    
  plot_regional_supplementary <- regional_data %>%
    dplyr::filter(region %in% remaining_regions) %>%
    dplyr::group_by(region) %>%
    ggplot2::ggplot() +
    ggplot2::geom_ribbon(ggplot2::aes(x = date_infection, ymin = cumulative_incidence_low, ymax = cumulative_incidence_high), alpha = 0.3, colour = NA, fill = "dodgerblue") +
    ggplot2::geom_point(ggplot2::aes(x = sample_end_date - 13, y = percentage_positive_mid)) +
    ggplot2::geom_errorbar(ggplot2::aes(x = sample_end_date - 13, ymin = lower_ci, ymax = upper_ci), 
                           width = 0.2) +
    ggplot2::geom_vline(ggplot2::aes(xintercept = sample_end_date), linetype = "dashed") +
    ggplot2::labs(title = "C",
                  x ="", y = "") + 
    ggplot2::theme(
    axis.title.x = ggplot2::element_blank(),
    axis.title.y = ggplot2::element_blank()) +
    ggplot2::scale_y_continuous(labels = scales::percent) +
    ggplot2::facet_wrap(~region, scales = "free_y") + 
    ggplot2::theme_minimal()
  
  layout <- "
    AAAABBB
    AAAACCC
    AAAACCC
    "
  
  plot_final <- (plot_national 
                 + plot_regional
                 + plot_regional_supplementary 
                 + plot_layout(design = layout)) 
  
  return(plot_final)
  
}

#--------------------- function for producing figure 3: true and adjusted daily
#--------------------- new case curves for top n countries

figure3Fun <- function()
{
  
  allAdjustedCaseData <- getAdjustedCaseDataNational()
  
  top10Countries <- allAdjustedCaseData %>%
    dplyr::group_by(country) %>%
    dplyr::summarise(total_adjusted_cases = sum(new_cases_adjusted_mid)) %>%
    dplyr::top_n(8) %>%
    dplyr::pull(country)
  
  options(scipen = 999)
  
  p1 <- allAdjustedCaseData %>%
    dplyr::group_by(country) %>%
    dplyr::filter(country %in% top10Countries) %>%
    dplyr::filter(new_cases > 5 & new_cases_adjusted_smooth_mid > 5 & new_cases_adjusted_smooth_low > 5 & new_cases_adjusted_smooth_high > 5) %>%
    dplyr::filter(date > "2020-03-15") %>%
    ggplot2::ggplot(ggplot2::aes(x = date, y = new_cases_smoothed, color = country)) + 
    ggplot2::geom_line() + 
    ggplot2::labs(x = "", y = "") + 
    ggplot2::scale_y_log10() +
    ggthemes::theme_few() + 
    directlabels::geom_dl(ggplot2::aes(label = country), 
                          method = list(directlabels::dl.combine("last.points"),
                                        cex = 0.6)) +
    ggplot2::theme(legend.position="none") + 
    viridis::scale_color_viridis(discrete = TRUE) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
    ggplot2::expand_limits(x = as.Date("2020-06-01"))
  
  
  p2 <- allAdjustedCaseData %>%
    dplyr::group_by(country) %>%
    dplyr::filter(country %in% top10Countries) %>%
    dplyr::filter(new_cases > 5 & new_cases_adjusted_smooth_mid > 5 & new_cases_adjusted_smooth_low > 5 & new_cases_adjusted_smooth_high > 5) %>%
    dplyr::filter(date > "2020-03-15") %>%
    ggplot2::ggplot(ggplot2::aes(x = date, y = new_cases_adjusted_smooth_mid, color = country)) + 
    ggplot2::geom_line() + 
    ggplot2::labs(x = "", y = "") + 
    ggplot2::scale_y_log10() + 
    ggthemes::theme_few() + 
    directlabels::geom_dl(ggplot2::aes(label = country), 
                          method = list(directlabels::dl.combine("last.points"),
                                        cex = 0.6)) +
    ggplot2::theme(legend.position="none") + 
    viridis::scale_color_viridis(discrete = TRUE) + 
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) + 
    ggplot2::expand_limits(x = as.Date("2020-06-01"))
  
  
  figure3 <- ggpubr::ggarrange(p1, p2,
                               labels = c("A", "B"), 
                               ncol = 2, nrow = 1)
  
  fullFigure <- ggpubr::annotate_figure(figure3, 
                                        left = ggpubr::text_grob("Smoothed moving average of daily adjusted case counts", rot = 90),
                                        bottom = ggpubr::text_grob("Time (number of weeks since surpassing 30 cases)"))
}


figure4Fun <- function()
{
  library(patchwork)
  
  allAdjustedCaseData <- getAdjustedCaseDataNational()  
  cumulativeIncidenceEstimatesWorldMap   <- allAdjustedCaseData %>% dplyr::filter(date == max(date))
  cumulativeIncidenceEstimatesEuropeMap1 <- allAdjustedCaseData %>% dplyr::filter(date ==  "2020-03-22")
  cumulativeIncidenceEstimatesEuropeMap2 <- allAdjustedCaseData %>% dplyr::filter(date ==  "2020-04-22")
  cumulativeIncidenceEstimatesEuropeMap3 <- allAdjustedCaseData %>% dplyr::filter(date ==  "2020-05-22")
  
  
  # cumulativeIncidenceEstimatesWorldMap <- getIncidenceUpToDateData(dateInput = NULL)
  # cumulativeIncidenceEstimatesEuropeMap1 <- getIncidenceUpToDateData(dateInput = "2020-03-10")
  # cumulativeIncidenceEstimatesEuropeMap2 <- getIncidenceUpToDateData(dateInput = "2020-04-10")
  # cumulativeIncidenceEstimatesEuropeMap3 <- getIncidenceUpToDateData(dateInput = "2020-05-10")
  # 
  europeanCountries <- c("Albania", "Andorra", "Austria", "Belarus", "Belgium",
                         "Bosnia and Herzegovina", "Bulgaria", "Croatia", "Cyprus",
                         "Czech Republic", "Denmark", "Estonia", "Finland", "France",
                         "Germany", "Greece", "Hungary","Ireland", "Isle of Man",
                         "Italy", "Kosovo", "Latvia", "Lithuania", "Luxembourg", "Moldova", 
                         "Netherlands", "Norway", "Macedonia", "Poland", "Portugal", "Romania", 
                         "Russia", "San Marino", "Slovakia", "Slovenia", "Spain", "Sweden", 
                         "Switzerland", "Turkey", "Ukraine", "UK")
  
  worldMapData <- combineMapAndIncidenceData(cumulativeIncidenceEstimatesWorldMap)
  europeMapData1 <- combineMapAndIncidenceData(cumulativeIncidenceEstimatesEuropeMap1)
  europeMapData2 <- combineMapAndIncidenceData(cumulativeIncidenceEstimatesEuropeMap2)
  europeMapData3 <- combineMapAndIncidenceData(cumulativeIncidenceEstimatesEuropeMap3)
  
  worldMapPlot <- mapPlottingFunction(worldMapData, europe = FALSE)
  europeMapPlot1 <- mapPlottingFunction(europeMapData1, europe = TRUE)
  europeMapPlot2 <- mapPlottingFunction(europeMapData2, europe = TRUE)
  europeMapPlot3 <- mapPlottingFunction(europeMapData3, europe = TRUE)
  
  wholeMapPlot <- worldMapPlot + (europeMapPlot1/europeMapPlot2/europeMapPlot3) +  plot_layout(widths = c(6, 1))
  
  return(wholeMapPlot)
  
}

#----------- plotting supplementary figure 1

figure1Supp <- function(data, countryArg, tweak1, tweak2)
{
  
  plotDat <- data %>%
    dplyr::filter(country == countryArg)
  
  
  limitsTesting <- plotDat %>%
    dplyr::filter(country == countryArg) %>%
    dplyr::summarise(yMin = min(testsPerCaseMA),
                     yMax = max(testsPerCaseMA))
  
  limitsUnderreporting <- plotDat %>%
    dplyr::filter(country == countryArg) %>%
    dplyr::summarise(yMin = min(lower),
                     yMax = max(upper))
  
  yLimitsTesting <- c(limitsTesting$yMin - tweak1, limitsTesting$yMax + tweak2)
  
  
  # plotting the first data set, the testing effort over time
  plot(plotDat$date, plotDat$testsPerCaseMA,
       type = "l",
       lty = 2, 
       lwd = 1,
       xlab = "",
       ylab = "",
       main = countryArg,
       #ylim = yLimitsTesting,
       axes = FALSE)
  axis(side = 2, ylim = yLimitsTesting, col = "black", las = 1)
  box()
  
  
  par(new = TRUE)
  
  plot(plotDat$date, plotDat$estimate, 
       type = "l",
       las = 1,
       xlab = "", 
       ylab = "",
       bty = "n",
       #ylim = limitsDatUnderreporting,
       axes = FALSE)
  polygon(x = c(plotDat$date, rev(plotDat$date)),
          y = c(plotDat$lower, rev(plotDat$upper)),
          col =  adjustcolor("dodgerblue", alpha.f = 0.10), border = NA)
  
}

#----------- plotting correlation between under-reporting estimates and testing efforts

figure2Supp <- function()
{
  
  plotDat <-  allDataTogether %>%
    dplyr::filter(country %in% allCountries) %>%
    dplyr::group_by(country) %>%
    dplyr::filter(dplyr::n() > 20) %>%
    dplyr::select(date, country, upper, lower, estimate, testsPerCase) %>%
    dplyr::na_if(Inf) %>%
    dplyr::mutate(upper = upper*100, lower = lower*100, estimate = estimate*100)
  
  plotOutput <- plotDat %>% 
    ggplot2::ggplot(ggplot2::aes(x = estimate, y = testsPerCase)) + 
    ggplot2::geom_point(ggplot2::aes(color = country, group = country)) + 
    ggplot2::ylim(0, 200) +
    ggplot2::xlab("Under-reporting estimate for all time points") + 
    ggplot2::ylab("Testing effort for all time points") + 
    ggplot2::geom_smooth(method = 'lm', formula = y ~ x)
  
  return(plotOutput)
  
}

figure3Supp <- function()
{
  
  regional_data <- getAdjustedRegionalCaseAndSerologyData()
  
  plot_regional_supplementary <- regional_data %>%
    dplyr::group_by(region) %>%
    ggplot2::ggplot() +
    ggplot2::geom_ribbon(ggplot2::aes(x = date_infection, ymin = cumulative_incidence_low, ymax = cumulative_incidence_high), alpha = 0.3, colour = NA, fill = "dodgerblue") +
    ggplot2::geom_point(ggplot2::aes(x = sample_end_date - 13, y = percentage_positive_mid)) +
    ggplot2::geom_errorbar(ggplot2::aes(x = sample_end_date - 13, ymin = lower_ci, ymax = upper_ci), 
                           width = 0.2) +
    ggplot2::geom_vline(ggplot2::aes(xintercept = sample_end_date), linetype = "dashed") +
    ggplot2::xlab("Date") +
    ggplot2::ylab("Cumulative incidence (%)") + 
    ggplot2::scale_y_continuous(labels = scales::percent) +
    ggplot2::facet_wrap(~region, scales = "free_y")
  
  
}

