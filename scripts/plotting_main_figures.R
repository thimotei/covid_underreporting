# ------- script to make all the figures ------- #

# ------- setting working directory -------#
here::here() %>% setwd()

library(patchwork)
source("covid_underreporting/R/data_helper_functions.R")
source("covid_underreporting/R/plotting_helper_functions.R")
source("covid_underreporting/R/plotting_functions.R")

#----------- making temporal variation under-reporting and testing effort figure -------------#

# calling the plotting function to make figure 1 style plots for every country in one large .pdf

countries_of_interest <- c("Australia", "Denmark", "South Korea", 
                            "Austria", "Switzerland", "USA",
                            "Belgium", "Italy", "United Kingdom")

row1 <- c("AUS", "DNK", "KOR")
row2 <- c("AUT", "CHE", "USA")
row3 <- c("BEL", "ITA", "GBR")

iso_codes_of_interest <- c(row1, row2, row3)

#--- gathering data for figure 1
testing_data <- getFigure1TestData()
under_reporting_data <- getFigure1UnderReportingData()

#--- making plot

png("covid_underreporting/figures/figure_S4.png", width = 10, height = 8, units = "in", res = 100)
fontsize1 <- 1
par(mar = c(3, 3, 3, 3))
par(oma = c(2, 2, 2, 2))
par(mfrow = c(3, 3))
for(iso_code_arg in iso_codes_of_interest)
{
  tryCatch(
    {
      figure_1_fun(under_reporting_data, testing_data, iso_code_arg)
    },
    error=function(e){})
}
mtext("Date",
      side = 1,
      col = "black",
      line = 0,
      outer = TRUE,
      cex = fontsize1)
mtext("Symptomatic cases reported (%)",
      side = 2,
      col = "black",
      line = 0,
      outer = TRUE,
      cex = fontsize1)
mtext("Tests performed per new confirmed case",
      side = 4,
      line = 0,
      outer = TRUE,
      cex = fontsize1)
dev.off()

#----------- making adjusted case curves figure -------------#

options(scipen = 999)
figure2 <- figure_2_fun()

ggplot2::ggsave("covid_underreporting/figures/figure_S5.png",
                plot = figure2, 
                width = 16,
                height = 8,
                units = "in")

#----------- making serology figure -------------#

figure3 <- figure_3_fun()
ggplot2::ggsave(
  "covid_underreporting/figures/figure_S6.png",
  plot = figure3, 
  width = 10,
  height = 5,
  units = "in")


#------------ making a map of the prevalence of COVID -----------------#

figure4 <- figure_4_fun()

ggplot2::ggsave("covid_underreporting/figures/figure_4.png",
                plot = figure4, 
                width = 10,
                height = 5,
                units = "in")



#--- supplementary figure 1: all under-ascertainment estimates (not age adjusted)

adjusted_cases <- getAdjustedCaseDataNational()

plotS1 <- adjusted_cases %>%
  dplyr::group_by(country) %>%
  dplyr::filter(dplyr::n() > 50) %>%
  ggplot2::ggplot() + 
  ggplot2::geom_line(ggplot2::aes(x = date, y = estimate)) + 
  ggplot2::geom_ribbon(ggplot2::aes(x = date, ymin = lower, ymax = upper), fill = "dodgerblue", alpha = 0.4) + 
  ggplot2::facet_wrap(~country, scales = "free_x", ncol = 10) + 
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(size=5, angle=45)) +
  ggplot2::labs(x = "Date", y = "Symptomatic cases reported (%)")

ggplot2::ggsave(
  "covid_underreporting/figures/figure_S7.png",
  plot = plotS1, 
  width = 12,
  height = 12,
  units = "in")

#--- supplementary figure 1: all testing effort estimates

testing_data <- getFigure1TestData()

plotS2 <- testing_data %>%
  dplyr::group_by(country) %>%
  #dplyr::filter(dplyr::n() > 20) %>%
  ggplot2::ggplot() + 
  ggplot2::geom_line(ggplot2::aes(x = date, y = testing_effort)) +
  ggplot2::facet_wrap(~country, scales = "free", ncol = 6) + 
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(size=5, angle=45)) +
  ggplot2::labs(x = "Date", y = "Tests performed per new confirmed case")

ggplot2::ggsave(
  "covid_underreporting/figures/figure_S2.png",
  plot = plotS2, 
  width = 12,
  height = 12,
  units = "in")


#--- supplementary figure, bivariate scatterplot of
#--- under-ascertainment estimate against testing effort

under_reporting_testing_data <- under_reporting_data %>% 
  dplyr::left_join(testing_data) %>%
  tidyr::drop_na()

cor.test(under_reporting_data_no_lag$estimate, under_reporting_data_no_lag$testing_effort, method = "kendall")

plot_s3 <- figure_2_supp(under_reporting_testing_data)

ggplot2::ggsave("covid_underreporting/figures/figure_S3.png",
                plot = plot_s3, 
                width = 14,
                height = 7,
                units = "in")