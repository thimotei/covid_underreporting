dataCasesAndEstimates %>%
  dplyr::group_by(country) %>%
  ggplot2::ggplot(ggplot2::aes(x = date)) +
  ggplot2::xlab("Date") +
  ggplot2::ylab("Percentage of symptomatic cases reported") +
  ggplot2::facet_wrap(~country, scales = "free", ncol = 4) +
  ggplot2::geom_col(ggplot2::aes(y = deaths)) +
  ggplot2::geom_ribbon(ggplot2::aes(ymin = lower*100,
                                    ymax = upper*100), fill = "#440154FF", alpha = 0.3) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(size = 7, angle = 90),
                 axis.text.y = ggplot2::element_text(size = 7),
                 legend.position = "none") +
  ggplot2::scale_y_continuous(sec.axis = ggplot2::sec_axis(~./100))

# dataCasesAndEstimates %>%
#   ggplot2::ggplot(ggplot2::aes(x = date, color = country, group = country)) +
#   ggplot2::xlab("Date") +
#   ggplot2::ylab("Percentage of symptomatic cases reported") +
#   ggplot2::geom_col(ggplot2::aes(y = cases)) + 
#   ggplot2::facet_wrap(~country, scales = "free", ncol = 4) +
#   ggplot2::theme(axis.text.x = ggplot2::element_text(size = 7, angle = 90),
#                  axis.text.y = ggplot2::element_text(size = 7))
# 
# dataCasesAndEstimates %>%
#   ggplot2::ggplot(ggplot2::aes(x = date, color = country), group = country) +
#   ggplot2::xlab("Date") +
#   ggplot2::ylab("Percentage of symptomatic cases reported") +
#   ggplot2::geom_col(ggplot2::aes(y = deaths)) + 
#   ggplot2::facet_wrap(~country, scales = "free", ncol = 4) +
#   ggplot2::theme(axis.text.x = ggplot2::element_text(size = 7, angle = 90),
#                  axis.text.y = ggplot2::element_text(size = 7),
#                  legend.position = "none")
