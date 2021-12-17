
# data import code from the author ----------------------------------------


library(readxl)
library(tidyr)

week13hw <- list()
week13hw$dat$raw <- read_excel("~/Downloads/Data_Extract_From_World_Development_Indicators-6.xlsx")
# week13hw$dat$raw|>View()
# week13hw$dat$raw |>names()
week13hw$dat$y2008 <- select(week13hw$dat$raw , c(-"2018 [YR2018]", -"Country Code", -"Series Code"))
week13hw$dat$y2008  <- spread(week13hw$dat$y2008 , key = "Series Name", value = "2008 [YR2008]")

week13hw$dat$y2008  <-select(week13hw$dat$y2008 , -"CO2 emissions (kt)")
#dat2008|>View()

week13hw$dat$y2018 <- select(week13hw$dat$raw, c(-"2008 [YR2008]", -"Country Code", -"Series Code"))
week13hw$dat$y2018  = spread(week13hw$dat$y2018 , key = "Series Name", value = "2018 [YR2018]")
# week13hw$dat$y2018|>names()
week13hw$dat$y2018  <-select(week13hw$dat$y2018 , -"CO2 emissions (kt)")
#dweek13hw$dat$y2018|>View()
week13hw$dat$y2018$year<- '2018'
# week13hw$dat$y2018|>View()

names(week13hw$dat$y2018) <- c("country", 'co2year18', 'gdpyear18' )
names(week13hw$dat$y2008) <- c("country", 'co2year08', 'gdpyear08' )

week13hw$dat$final<-
  {
    week13hw$dat$y2008|>#names()
      left_join(week13hw$dat$y2018[-4], by="country")
  }
week13hw$dat$final


# construct proper long and wide data frame -------------------------------

df_all_long <- {
  df_08 <-
    data.frame(
      country = week13hw$dat$final$country,
      x = week13hw$dat$final$gdpyear08,
      y = week13hw$dat$final$co2year08,
      size = week13hw$dat$final$co2year08 / 2
    )
  df_18 <-
    data.frame(
      country = week13hw$dat$final$country,
      x = week13hw$dat$final$gdpyear18,
      y = week13hw$dat$final$co2year18,
      size = week13hw$dat$final$co2year18 / 2
    )
  df_08$year <- "2008"
  df_18$year <- "2018"
  dplyr::bind_rows(
    df_08, df_18
  )
}
df_all_wide <-
  tidyr::pivot_wider(
    data = df_all_long,
    names_from = "year",
    names_prefix = "year",
    values_from = c("x", "y", "size")
  )


ggplot() +
  geom_point(
    data = df_all_long,
    mapping = aes(
      x = x, y = y, size = size, alpha = year
    ),
    color = "blue"
  ) +
  geom_segment(
    data = df_all_wide,
    aes(
      x = x_year2008,
      y = y_year2008,
      xend = x_year2018,
      yend = y_year2018
    ), arrow = arrow(length = unit(0.2, "cm")), size = 0.5
  )
