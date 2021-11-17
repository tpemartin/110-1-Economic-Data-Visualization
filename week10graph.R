ticks <- list()
ticks$major <- seq(1980, 2015, by = 5)
ticks$minor <- c(1979, 2018)

majorLength <- 2
minor_majorRatio <- 0.7

ggplot1$plot1 +
  geom_rug(
    mapping = aes(
      x = ticks$major
    ),
    outside = TRUE, # drawrugoutsidetheplotpanel
    size = 0.5,
    length = grid::unit(
      majorLength,
      "mm"
    )
  ) +
  geom_rug(
    mapping = aes(
      x = ticks$minor
    ),
    outside = TRUE,
    size = 0.5,
    length = grid::unit(
      minor_majorRatio * majorLength,
      "mm"
    )
  ) +
  coord_cartesian(clip = "off") -> # allowdrawingoutsidetheplotpanel
  ggplot1$plot2

ggplot1$plot2
