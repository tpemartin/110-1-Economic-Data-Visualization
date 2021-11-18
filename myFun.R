breaks = {
  breaks <-
    c(1979, seq(1985, 2015, by = 5), 2018)
  breaks
}
labels = {
  labels <- c(
    "1979", "85", "90", "95", "2000", "05", "10", "15", "18"
  )
  labels
}
ticks_major = seq(1980, 2015, by=5)
ticks_minor = c(1979, 2018)
my_axis_x <- function(breaks, labels, ticks_major, ticks_minor)
{
  list(
    scale_x_continuous(
      breaks = breaks,
      labels = labels
    ),
    theme(
      axis.ticks.length.x = unit(0, "mm")
    ),
    geom_rug(
      mapping = aes(
        x = ticks_major
      ),
      outside = TRUE, # draw rug outside the plot panel
      size = 0.5, # input$majorsize
      length = grid::unit(
        majorLength,
        "mm"
      )
    ),
    geom_rug(
      mapping = aes(
        x = ticks_minor
      ),
      outside = TRUE,
      size = 0.5, # input$minorsize
      length = grid::unit(
        minor_majorRatio * majorLength,
        "mm"
      )
    ),
    coord_cartesian(clip = "off"),
    theme(
      axis.text.x = element_text(
        margin = margin(
          12 # input$margin
        ),
        size = 16 # input$textSize
      )
    )
  )
}
