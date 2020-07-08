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


mapPlottingFunction <- function(dataInput, europe = TRUE, plot_label)
{
  
  plotOutput <- dataInput %>%
    ggplot2::ggplot(ggplot2::aes(x = long, y = lat, group = group)) +
    ggplot2::geom_polygon(ggplot2::aes(fill = cumulative_incidence_mid*100)) + 
    ggplot2::geom_path(ggplot2::aes(x = long, y = lat, group = group), size = 0.3) + 
    theme_map(world = TRUE) +
    viridis::scale_fill_viridis(option = "magma", 
                                begin = 0.4, 
                                end = 0.95, 
                                name = "Estimated seroprevalence of SARS-CoV-2", 
                                direction = -1, 
                                breaks = c(0, 5, 10, 15, 20),
                                label = c("0%", "5%", "10%", "15%", "20%")) +
    ggplot2::labs(tag = plot_label)
  
  if(europe == TRUE)
  {
    plotOutput <- plotOutput + 
      ggplot2::coord_fixed(xlim = c(-9, 42.5),
                           ylim = c(36, 70.1),
                           ratio = 1.5) + 
      ggplot2::guides(fill = FALSE)
  }
  
  return(plotOutput)
  
}


binom_min <- function(x, n)
{
  tmp <- binom.test(x, n)
  signif(tmp$conf.int[1]*100, 2)
  
}

binom_max <- function(x, n)
{
  tmp <- binom.test(x, n)
  signif(tmp$conf.int[2]*100, 2)
  
}
