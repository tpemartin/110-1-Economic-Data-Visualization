library(ggplot2)
library(dplyr)

data.frame(
  x=seq(
    from=lubridate::ymd("20100101"),
    to=lubridate::ymd("20211201"),
    by="1 quarter"
  )) -> .df
set.seed(2893)
.df$y = sample(0:100, 48, T)
.df$y2 = sample(50:110,48, T)
.df |> tidyr::pivot_longer(
  cols = -x,
  names_to ="type",
  values_to = "y"
) -> .df_long

# ggplot ----

ggplot(
  data=.df_long
)+geom_line(
  aes(
    x=x,y=y, color=type
  )
)

## find switching points
source("support/time.R")
.df |> find_switchingPoint() -> switchingPoint
# OR
# devtools::install_github("tpemartin/econDV2", force=T)
#  then
.df |> econDV2::find_switchingPoint() -> switchingPoint

ggplot(
)+geom_line(
  data=.df_long  |> # time has to be datetime class since switchingPoint$x is a datetime class
    mutate(
      x=lubridate::as_datetime(x)
    ),
  aes(
    x=x,y=y, color=type
  )
) +
  geom_point(
    data=switchingPoint,
    aes(
      x=x,y=y
    )
  )

# support ----


