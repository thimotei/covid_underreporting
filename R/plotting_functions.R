#---------- reading the case and death estimates        ----------#
#---------- from ECDC and the under-reporting estimates ----------#
#---------- (from the shared Dropbox) in                ----------#


# figure 1 uses under-reporting and testing data function
figure1Fun <- function(data, countryArg, tweak1, tweak2)
{
  
  plotDat <- data %>%
    dplyr::filter(country == countryArg)

  yLimitsTestingDat <- plotDat %>%
    dplyr::summarise(yMin = min(testsPerCaseMA),
                     yMax = max(testsPerCaseMA))
  

  yLimitsTesting <- c(yLimitsTestingDat$yMin - tweak1, yLimitsTestingDat$yMax + tweak2)
  
  yLimitsUnderreportingDat <- plotDat %>%
    dplyr::summarise(yMin = min(lower),
                     yMax = max(upper))
  
  
  yLimitsUnderreporting <- c(yLimitsUnderreportingDat$yMin - tweak1, yLimitsUnderreportingDat$yMax + tweak2)
  

  plot(plotDat$date, plotDat$testsPerCaseMA,
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

  # plotting the second data set, the under-reporting estimate over time
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
  axis.Date(side = 1, plotDat$date)

}

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
    axis(side = 2, ylim = yLimitsTesting, col = "black",las = 1)
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

# function for producing the serology figure

figure2Fun <- function(dataInputDF, observedDataDF)
{
  
  plotPrevalence <- data %>%
    dplyr::group_by(country) %>%
    dplyr::left_join(observedDataDF, by = "country") %>%
    tidyr::drop_na() %>%
    dplyr::group_by(country) %>%
    ggplot2::ggplot(ggplot2::aes()) +
    ggplot2::scale_x_date() + 
    ggplot2::geom_ribbon(ggplot2::aes(x = date, ymin = cumulativePrevalenceLow, ymax = cumulativePrevalenceHigh), alpha = 0.3, colour = NA, fill = "dodgerblue") +
    ggplot2::geom_point(ggplot2::aes(x = dateMid - 10, y = observedEstimate)) +
    ggplot2::geom_point(ggplot2::aes(x = dateMid, y = observedEstimate)) +
    ggplot2::geom_vline(data = observedDataDF, ggplot2::aes(xintercept = dateStart), linetype = "dashed") +
    ggplot2::geom_vline(data = observedDataDF, ggplot2::aes(xintercept = dateEnd), linetype = "dashed") +
    ggplot2::xlab("Date") +
    ggplot2::ylab("Cumulative prevalence (%)") + 
    ggplot2::scale_y_continuous(labels = scales::percent) +
    ggplot2::facet_wrap(~country, scales = "free")
  
}

# function for producing figure 3: true and adjusted daily new case curves for top n countries
figure3Fun <- function(data)
{
 
  trueEstimatesAfterthirtythCase <- data %>%
    dplyr::group_by(country) %>%
    dplyr::arrange(country, date) %>%
    #dplyr::filter(cases > 30) %>% 
    dplyr::mutate(weeks = as.double(difftime(lubridate::ymd(date),
                                             lubridate::ymd("2020-01-19")),
                                    units = "weeks")) %>%
    dplyr::ungroup() %>%
    # dplyr::mutate(casesAdjustedTmp = imputeTS::na_ma(trueCasesMid, k = 4, weighting = "linear")) %>%
    dplyr::mutate(casesMA = forecast::ma(cases, order = 6)) %>%
    dplyr::mutate(trueCasesMidMA = forecast::ma(trueCasesMid, order = 6)) %>%
    #dplyr::group_by(country) %>%
    dplyr::mutate(country = dplyr::case_when(country == "United States of America" ~ "US",
                                             country != "United States of America" ~ country)) %>%
    dplyr::mutate(country = dplyr::case_when(country == "United Kingdom" ~ "UK",
                                             country != "United Kingdom" ~ country))
    #dplyr::filter(!dplyr::row_number() %in% 1:40 | weeks < 9) %>%
    #dplyr::mutate(weeks = weeks - 7)
  
  
  topCountries <- trueEstimatesAfterthirtythCase %>%
    dplyr::group_by(country) %>%
    dplyr::summarise(totalAdjustedCases = sum(trueCasesMid)) %>%
    dplyr::arrange(desc(totalAdjustedCases)) %>% 
    dplyr::top_n(8) %>%
    dplyr::select(country) %>%
    dplyr::pull(country) 
  
  p1 <- trueEstimatesAfterthirtythCase %>%
    dplyr::filter(country %in% topCountries) %>%
    dplyr::group_by(country) %>%
    #dplyr::filter(country == "South Korea") %>%
    ggplot2::ggplot(ggplot2::aes(x = date, y = casesMA,  color = country)) +
    ggplot2::geom_line() +
    #ggplot2::xlim(49, 16.5) +
    ggplot2::scale_x_date(date_breaks = "2 weeks",
                          labels = scales::date_format("%d %b"),
                          limits = as.Date(c("2020-03-15", "2020-05-14"))) +
    ggplot2::scale_y_log10(limits = c(300, NA)) +
    ggplot2::xlab("Date") + 
    ggplot2::ylab("") +
    directlabels::geom_dl(ggplot2::aes(label = country), 
                          method = list(directlabels::dl.combine("last.points"),
                                        cex = 0.6)) +
    cowplot::theme_cowplot() +
    #ggplot2::theme_gray() +
    ggplot2::theme(legend.position = "none") 
  
  
  p2 <- trueEstimatesAfterthirtythCase %>%
    dplyr::filter(country %in% topCountries) %>%
    dplyr::group_by(country) %>%
    #dplyr::filter(country == "South Korea") %>%
    ggplot2::ggplot(ggplot2::aes(x = date, y = trueCasesMidMA,  color = country)) +
    ggplot2::geom_line() +
    #ggplot2::xlim(49, NA) +
    ggplot2::scale_x_date(date_breaks = "2 weeks",
                          labels = scales::date_format("%d %b"),
                          limits = as.Date(c("2020-03-15", "2020-05-14"))) + 
    ggplot2::scale_y_log10(limits = c(3000, NA)) +
    ggplot2::xlab("Date") + 
    ggplot2::ylab("") +
    directlabels::geom_dl(ggplot2::aes(label = country), 
                          method = list(directlabels::dl.combine("last.points"), 
                                        cex = 0.6)) +
    cowplot::theme_cowplot() +
    #ggplot2::theme_gray() +
    ggplot2::theme(legend.position = "none")
  
  
  figure3 <- ggpubr::ggarrange(p1, p2,
                               labels = c("A", "B"), 
                               ncol = 2, nrow = 1)
  
  ggpubr::annotate_figure(
    figure3,
    left = ggpubr::text_grob("Smoothed moving average of daily adjusted case counts", rot = 90),
    bottom = ggpubr::text_grob("Time (number of weeks since surpassing 30 cases)"))

}

figureSuppFun <- function()
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


