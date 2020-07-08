#---------- reading the case and death estimates        ----------#
#---------- from ECDC and the under-reporting estimates ----------#
#---------- (from the shared Dropbox) in                ----------#


#---------------------- figure 1 uses under-reporting and testing data function
figure_1_fun <- function(under_reporting_data, testing_data, iso_code_arg)
{
  
  country_names <- dplyr::tibble(country = countrycode::countrycode(iso_code_arg, "iso3c", "iso.name.en")) %>%
    dplyr::mutate(country = dplyr::case_when(country == "United States of America (the)" ~ "USA",
                                             country == "Russian Federation (the)" ~ "Russia",
                                             country == "Iran (Islamic Republic of)" ~ "Iran",
                                             country != "USA" || "Russia" || "Iran" ~ country))
  
  plot_dat_under_reporting <- under_reporting_data %>%
    dplyr::filter(iso_code == iso_code_arg) %>%
    dplyr::mutate(estimate = estimate*100,
                  lower = lower*100,
                  upper = upper*100)
  
  plot_dat_testing <- testing_data %>%
    dplyr::filter(iso_code == iso_code_arg)
  
  x_limits_both_dat <- plot_dat_under_reporting %>%
    dplyr::summarise(x_min = min(date),
                     x_max = max(date))
  
  x_limits_both <- c(x_limits_both_dat$x_min, x_limits_both_dat$x_max)

  if(iso_code_arg %in% row1)
  {
    y_limits_under_reporting <- c(y_min = 0, y_max = 100)
  }
  if(iso_code_arg %in% row2)
  {
    y_limits_under_reporting <- c(y_min = 0, y_max = 100)
  }
  if(iso_code_arg %in% row3)
  {
    y_limits_under_reporting <- c(y_min = 0, y_max = 40)
  }


  # y_limits_under_reporting <- c(y_min = 0, y_max = 100)
  
  #--- uncomment for ad-hoc country runs
  # 
  # if(!(iso_code_arg %in% row1) | !(iso_code_arg %in% row2) | !(iso_code_arg %in% row3))
  # {
  #   y_limits_under_reporting_dat <- plot_dat_under_reporting %>%
  #     dplyr::summarise(y_min = min(lower),
  #                      y_max = max(upper))
  #   y_limits_under_reporting <- c(y_limits_under_reporting_dat$y_min, y_limits_under_reporting_dat$y_max)
  # }
  # 
  y_limits_testing_dat <- plot_dat_testing %>%
    dplyr::summarise(y_min = min(testing_effort),
                     y_max = max(testing_effort))
  
  
  y_limits_testing <- c(y_limits_testing_dat$y_min, y_limits_testing_dat$y_max)
  
  #--------------- plotting the first data set, the under-reporting estimate over time
  plot(plot_dat_under_reporting$date, plot_dat_under_reporting$estimate, 
       type = "l",
       las = 1,
       xlab = "", 
       ylab = "",
       bty = "n",
       axes = FALSE,
       xlim = x_limits_both, 
       ylim = y_limits_under_reporting)
  polygon(x = c(plot_dat_under_reporting$date, rev(plot_dat_under_reporting$date)),
          y = c(plot_dat_under_reporting$lower, rev(plot_dat_under_reporting$upper)),
          col =  adjustcolor("dodgerblue", alpha.f = 0.42), border = NA)
  axis(side = 2, ylim = y_limits_under_reporting, col = "black", las = 1)
  box()
  
  par(new = TRUE)
  #--------------- plotting the second data set, the under-reporting estimate over time
  plot(plot_dat_testing$date, plot_dat_testing$testing_effort,
       type = "l",
       lty = 2, 
       lwd = 1,
       xlab = "",
       ylab = "",
       main = country_names,
       xlim = x_limits_both,
       ylim = y_limits_testing,
       axes = FALSE)
  axis(side = 4, ylim = y_limits_under_reporting, col = "black", las = 1)
  
  # making the time axis
  tick_positions = seq(min(plot_dat_under_reporting$date), max(plot_dat_under_reporting$date), length.out = 4)
  axis.Date(side = 1, lubridate::month(plot_dat_under_reporting$date, label = TRUE), at = tick_positions, srt = 20)
}

#--------------------- function for producing figure 3: true and adjusted daily
#--------------------- new case curves for top n countries

figure_2_fun <- function()
{
  
  all_adjusted_case_data <- getAdjustedCaseDataNational()
  
  options(scipen = 999)
  
  #--- making first panel
  
  top_10_confirmed_case_countries <- all_adjusted_case_data %>%
    dplyr::group_by(country) %>%
    dplyr::summarise(total_cases = sum(new_cases)) %>%
    dplyr::top_n(10) %>%
    dplyr::pull(country)
  
  plot_1_dat <- all_adjusted_case_data %>%
    dplyr::group_by(country) %>%
    dplyr::filter(country %in% top_10_confirmed_case_countries) %>%
    dplyr::filter(new_cases > 5 & new_cases_adjusted_smooth_mid > 5 & new_cases_adjusted_smooth_low > 5 & new_cases_adjusted_smooth_high > 5) %>%
    dplyr::filter(date > "2020-03-15") 
  
  min_cases <- plot_1_dat %>%
    dplyr::ungroup() %>%
    dplyr::filter(new_cases_smoothed == min(new_cases_smoothed)) %>%
    dplyr::pull(new_cases_smoothed)
  
  max_cases <- plot_1_dat %>%
    dplyr::ungroup() %>%
    dplyr::filter(new_cases_adjusted_smooth_mid == max(new_cases_adjusted_smooth_mid)) %>%
    dplyr::pull(new_cases_adjusted_smooth_mid)
  
  p1 <- plot_1_dat %>%
    ggplot2::ggplot(ggplot2::aes(x = date, y = new_cases_smoothed, color = country)) + 
    ggplot2::geom_line() + 
    ggplot2::labs(x = "", y = "") + 
    ggplot2::scale_y_log10() +
    ggthemes::theme_few() + 
    directlabels::geom_dl(ggplot2::aes(label = country), 
                          method = list(directlabels::dl.combine("last.points"),
                                        cex = 0.6)) +
    ggplot2::theme(legend.position="none",
                   axis.title  = ggplot2::element_text(size = 11),
                   plot.margin = ggplot2::unit(c(0, 0.5, -0.49, 0.5), "cm")) + 
    ggplot2::labs(tag = "A",
                  x = "", y = "Confirmed cases by date of confirmation") + 
    viridis::scale_color_viridis(discrete = TRUE, begin = 0, end = 0.82) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
    ggplot2::expand_limits(x = as.Date("2020-06-20"),
                           y = max_cases)
  
  
  p2 <- plot_1_dat %>%
    ggplot2::ggplot(ggplot2::aes(x = date, y = new_cases_adjusted_smooth_mid, color = country)) + 
    ggplot2::geom_line() + 
    ggplot2::labs(x = "", y = "") + 
    ggplot2::scale_y_log10() + 
    ggthemes::theme_few() + 
    directlabels::geom_dl(ggplot2::aes(label = country), 
                          method = list(directlabels::dl.combine("last.points"),
                                        cex = 0.6)) +
    ggplot2::theme(legend.position="none",
                   axis.title  = ggplot2::element_text(size = 11),
                   plot.margin = ggplot2::unit(c(0, 0.5, -0.49, 0.5), "cm")) + 
    ggplot2::labs(tag = "",
                  x = "", y = "New adjusted cases by date of confirmation") + 
    viridis::scale_color_viridis(discrete = TRUE, begin = 0, end = 0.82) + 
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) + 
    ggplot2::expand_limits(x = as.Date("2020-06-20"),
                           y = min_cases)
  
  #--- making 2nd panel
  
  top_10_adjusted_case_countries <- all_adjusted_case_data %>%
    dplyr::group_by(country) %>%
    dplyr::summarise(total_adjusted_cases = sum(new_cases_adjusted_mid)) %>%
    dplyr::top_n(10) %>%
    dplyr::pull(country)
  
  plot_2_dat <- all_adjusted_case_data %>%
    dplyr::group_by(country) %>%
    dplyr::filter(country %in% top_10_adjusted_case_countries) %>%
    dplyr::filter(new_cases > 5 & new_cases_adjusted_smooth_mid > 5 & new_cases_adjusted_smooth_low > 5 & new_cases_adjusted_smooth_high > 5) %>%
    dplyr::filter(date > "2020-03-15") 
  
  min_cases <- plot_2_dat %>%
    dplyr::ungroup() %>%
    dplyr::filter(new_cases_smoothed == min(new_cases_smoothed)) %>%
    dplyr::pull(new_cases_smoothed)
  
  max_cases <- plot_2_dat %>%
    dplyr::ungroup() %>%
    dplyr::filter(new_cases_adjusted_smooth_mid == max(new_cases_adjusted_smooth_mid)) %>%
    dplyr::pull(new_cases_adjusted_smooth_mid)
  
  p3 <- plot_2_dat %>%
    ggplot2::ggplot(ggplot2::aes(x = date, y = new_cases_smoothed, color = country)) + 
    ggplot2::geom_line() + 
    ggplot2::labs(x = "", y = "") + 
    ggplot2::scale_y_log10() +
    ggthemes::theme_few() + 
    directlabels::geom_dl(ggplot2::aes(label = country), 
                          method = list(directlabels::dl.combine("last.points"),
                                        cex = 0.6)) +
    ggplot2::theme(legend.position="none",
                   axis.title  = ggplot2::element_text(size = 11),
                   plot.margin = ggplot2::unit(c(-0.49, 0.5, 0.5, 0.5), "cm")) + 
    ggplot2::labs(tag = "B",
                  x = "", y = "New confirmed cases by date of confirmation") + 
    viridis::scale_color_viridis(discrete = TRUE, begin = 0, end = 0.82) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
    ggplot2::expand_limits(x = as.Date("2020-06-20"),
                           y = max_cases)
  
  
  p4 <- plot_2_dat %>%
    ggplot2::ggplot(ggplot2::aes(x = date, y = new_cases_adjusted_smooth_mid, color = country)) + 
    ggplot2::geom_line() + 
    ggplot2::labs(x = "", y = "") + 
    ggplot2::scale_y_log10() + 
    ggthemes::theme_few() + 
    directlabels::geom_dl(ggplot2::aes(label = country), 
                          method = list(directlabels::dl.combine("last.points"),
                                        cex = 0.6)) +
    ggplot2::theme(legend.position="none",
                   axis.title  = ggplot2::element_text(size = 11),
                   plot.margin = ggplot2::unit(c(-0.49, 0.5, 0.5, 0.5), "cm")) + 
    ggplot2::labs(tag = "",
                  x = "", y = "New adjusted cases by date of confirmation") + 
    viridis::scale_color_viridis(discrete = TRUE, begin = 0, end = 0.82) + 
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) + 
    ggplot2::expand_limits(x = as.Date("2020-06-20"),
                           y = min_cases)
  
  
  #--- making facet wrap of each individual country - trying out different numbers, ten might be too many
  
  top_12_confirmed_case_countries <- union(top_10_adjusted_case_countries, top_10_confirmed_case_countries)
  
  plot_3_dat <- all_adjusted_case_data %>%
    dplyr::group_by(country) %>%
    dplyr::filter(country %in% top_12_confirmed_case_countries) %>%
    dplyr::filter(new_cases > 5 & new_cases_adjusted_smooth_mid > 5 & new_cases_adjusted_smooth_low > 5 & new_cases_adjusted_smooth_high > 5)

  
  p5 <- plot_3_dat %>%
    ggplot2::ggplot() + 
    ggplot2::geom_line(ggplot2::aes(x = date, y = new_cases_smoothed)) + 
    ggplot2::geom_ribbon(ggplot2::aes(x = date,
                                      ymin = new_cases_adjusted_smooth_low, 
                                      ymax = new_cases_adjusted_smooth_high), fill = "dodgerblue", alpha = 0.6) + 
    ggplot2::labs(x = "", y = "") + 
    ggplot2::scale_y_log10() +
    ggthemes::theme_few() + 
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45)) + 
    ggplot2::theme(legend.position="none",
                   strip.text  = ggplot2::element_text(size = 13),
                   axis.title  = ggplot2::element_text(size = 11),
                   plot.margin = ggplot2::unit(c(0, 0, 0, 0), "cm")) + 
    ggplot2::labs(tag = "C",
                  x = "", y = "Cases by date of confirmation") + 
    viridis::scale_color_viridis(discrete = TRUE, begin = 0, end = 0.82) +
    ggplot2::facet_wrap(~country, scale = "free", nrow = 5)
  
  
  #--- putting it all together into single plot using patchwork
  
  library(patchwork)
  
  layout <- "
    AAABBBEEEEE
    CCCDDDEEEEE
    "
  
  plot_final <- p1 + p2 + p3 + p4 + p5 + plot_layout(design = layout)
  plot_final
  
}

#------------------- function for producing the serology figure

figure_3_fun <- function()
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
  
  regional_data <- getAdjustedRegionalCaseAndSerologyData() 
  
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
    dplyr::mutate(region = dplyr::case_when(region == "North East and Yorkshire" ~ "North East",
                                            region != "North East and Yorkshire" ~ region)) %>% 
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

#------------------- function to produce map figure

figure_4_fun <- function()
{
  
  library(patchwork)
  
  allAdjustedCaseData <- getAdjustedCaseDataNational() %>% dplyr::group_by(country)
  cumulativeIncidenceEstimatesWorldMap   <- allAdjustedCaseData %>% dplyr::filter(date == max(date))
  cumulativeIncidenceEstimatesEuropeMap1 <- allAdjustedCaseData %>% dplyr::filter(date ==  "2020-03-22")
  cumulativeIncidenceEstimatesEuropeMap2 <- allAdjustedCaseData %>% dplyr::filter(date ==  "2020-04-22")
  cumulativeIncidenceEstimatesEuropeMap3 <- allAdjustedCaseData %>% dplyr::filter(date ==  "2020-05-22")
  
  cumulativeIncidenceEstimatesWorldMap %>% 
    dplyr::ungroup() %>%
    dplyr::filter(cumulative_incidence_mid == min(cumulative_incidence_mid))

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
  
  worldMapPlot   <- mapPlottingFunction(worldMapData,  europe = FALSE, plot_label = "A")
  europeMapPlot1 <- mapPlottingFunction(europeMapData1, europe = TRUE, plot_label = "B")
  europeMapPlot2 <- mapPlottingFunction(europeMapData2, europe = TRUE, plot_label = "C")
  europeMapPlot3 <- mapPlottingFunction(europeMapData3, europe = TRUE, plot_label = "D")
  
  wholeMapPlot <- worldMapPlot + (europeMapPlot1/europeMapPlot2/europeMapPlot3) +  plot_layout(widths = c(6, 1))
  
  return(wholeMapPlot)
  
}

#----------- plotting supplementary figure 1

figure1Supp <- function(under_reporting_data, country_arg)
{
  
  
  plot_dat_under_reporting <- under_reporting_data %>%
    dplyr::filter(country == country_arg)
  
  x_limits_both_dat <- plot_dat_under_reporting %>%
    dplyr::summarise(x_min = min(date),
                     x_max = max(date))
  
  x_limits_both <- c(x_limits_both_dat$x_min, x_limits_both_dat$x_max)
  
  #--------------- plotting the first data set, the under-reporting estimate over time
  plot(plot_dat_under_reporting$date, plot_dat_under_reporting$estimate, 
       type = "l",
       las = 1,
       xlab = "", 
       ylab = "",
       bty = "n",
       axes = FALSE,
       xlim = x_limits_both, 
       ylim = y_limits_under_reporting)
  polygon(x = c(plot_dat_under_reporting$date, rev(plot_dat_under_reporting$date)),
          y = c(plot_dat_under_reporting$lower, rev(plot_dat_under_reporting$upper)),
          col =  adjustcolor("dodgerblue", alpha.f = 0.42), border = NA)
  axis(side = 4, ylim = y_limits_under_reporting, col = "black", las = 1)
  box()
  
  # making the time axis
  tick_positions = seq(min(plot_dat_under_reporting$date), max(plot_dat_under_reporting$date), length.out = 4)
  axis.Date(side = 1, plot_dat_under_reporting$date, at = tick_positions, srt = 20)
}

#----------- plotting correlation between under-reporting estimates and testing efforts

figure_2_supp <- function(plot_data)
{
  plot_output <- plot_data %>%
    dplyr::rename(Country = country) %>%
    dplyr::mutate(Country = dplyr::case_when(Country == "Bolivia (Plurinational State of)" ~ "Bolivia",
                                             Country == "Iran (Islamic Republic of)" ~ "Iran",
                                             Country == "Russian Federation (the)" ~ "Russia",
                                             Country != "Bolivia" || "Iran" || "Russia" ~ Country )) %>%
    ggplot2::ggplot() +
    ggplot2::geom_point(ggplot2::aes(x = estimate, y = testing_effort, color = Country)) + 
    ggplot2::geom_smooth(ggplot2::aes(x = estimate, y = testing_effort), method = "loess") + 
    ggplot2::theme(legend.text = ggplot2::element_text(size = 8)) + 
    viridis::scale_color_viridis(discrete = TRUE, begin = 0, end = 0.82) + 
    ggplot2::theme_minimal() + 
    ggplot2::labs(x = "Median under-ascertainment estimate (log scale)",
                  y = "Testing effort (log scale)") + 
    ggplot2::scale_y_log10() + 
    ggplot2::guides(shape = ggplot2::guide_legend(override.aes = list(size = 0.35))) + 
    ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(size = 0.35))) +
    ggplot2::theme(legend.title = ggplot2::element_text(size = 7), 
                   legend.text  = ggplot2::element_text(size = 7))
  
  return(plot_output)
  
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

