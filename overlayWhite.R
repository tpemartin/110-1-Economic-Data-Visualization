
geom_point_overlay = 
  econDV2::geom_overlay(geom_point)

.df=data.frame(
  x=c(1),
  y=c(1)
)
ggplot(
  data = .df
) +
  geom_line(
    data = data.frame(
      x = c(1, 2),
      y = c(1, 2)
    ),
    aes(x = x, y = y)
  ) +
  geom_point(
    mapping = aes(
      x = x, y = y
    ),
    color = "blue",
    fill = "dodgerblue2",
    alpha = 0.5,
    size = 3,
    shape=21
  )


