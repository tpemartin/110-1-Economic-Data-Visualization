# data import -------------------------------------------------------------

library(readr)
library(dplyr)    
library(ggplot2)
library(scales)
library(tidyr)
台灣gdp <- read_csv("https://github.com/QBace/110-1-Economic-Data-Visualization/raw/main/%E5%8F%B0%E7%81%A3gdp.csv", col_types = cols(year1 = col_integer()))
select2<-select(台灣gdp,8:14) 

p1<-mutate(select2,low_min = pmin(商品及服務輸出佔比, 商品及服務輸入佔比), low_max = pmax(商品及服務輸出佔比, 商品及服務輸入佔比),high_min = low_min, high_max = low_max)
p1$low_min[p1$商品及服務輸出佔比 > p1$ 商品及服務輸入佔比] = NA
p1$low_max[p1$商品及服務輸出佔比 > p1$ 商品及服務輸入佔比] = NA
p1$high_min[p1$商品及服務輸出佔比 <= p1$ 商品及服務輸入佔比] = NA
p1$high_max[p1$商品及服務輸出佔比 <= p1$ 商品及服務輸入佔比] = NA

# code example ------------------------------------------------------------


econDV2::Object(week13)
week13$data_wide <- p1
week13$data_wide |>
  mutate(
    year1=lubridate::ymd(paste(year1, "1","1")) |> lubridate::as_datetime()
  ) -> week13$data_wide

week13$data_wide |>
  tidyr::pivot_longer(
    cols = 2:7, # 以p1[,2:7] 為目標，將它轉成long form
    names_to = "項目", # 其中2:7的原本的column name變成long form的項目column
    values_to = "GDP佔比" # 而2:7column下的值則變成long form的GDP佔比
  ) -> week13$data_long


week13$ggplot = ggplot()
week13$geom_line = {
  list(
    geom_line(
      data=week13$data_long,
      mapping=aes(x=year1, y=GDP佔比, colour=項目)),
    scale_colour_manual(
      limits=c('商品及服務輸出佔比','商品及服務輸入佔比','民間消費佔比','政府消費佔比','資本形成佔比','經濟成長'),
      values=c('#6fc063','#b7524c','#499eb3','#F0C10F','#3e4142','#F52318')
    ))
}
week13$ggplot+week13$geom_line
week13$geom_event$data = {
  as.data.frame(
    rbind(
      c(xmin = 1981,xmax = 1986),
      c(xmin = 2010,xmax = 2020),
      c(xmin = 1990,xmax = 2000)
    )
  ) |>
    mutate(
      xmin = lubridate::ymd(paste(xmin, "1","1")) |> lubridate::as_datetime(),
      xmax = lubridate::ymd(paste(xmax, "1","1")) |> lubridate::as_datetime()
    )
}

week13$geom_event$geom = {
    geom_rect(
      data=week13$geom_event$data,
      mapping=aes(
        xmin=xmin, xmax=xmax,
        ymax= Inf, ymin=-Inf
      ),
      fill=alpha("grey",0.3), inherit.aes = F)
}

plotnow <- week13$ggplot+week13$geom_line+week13$geom_event

week13$geom_ribbon$data <- 
{
  targetColumns = c("x"="year1",
    "y"="商品及服務輸出佔比", "y2"="商品及服務輸入佔比")
  econDV2::get_switching_regime_dataframe(week13$data_wide, targetColumns) 
}
week13$geom_ribbon$geom <- 
    geom_ribbon(
      data=week13$geom_ribbon$data,
      mapping=aes(
        x=x,
        ymin=ymin,
        ymax=ymax,
        fill=regime
      )
    )

week13$save()

plotnow+week13$geom_ribbon$geom

attach(week13)
ggplot+geom_line+geom_event$geom +geom_ribbon$geom
detach(week13)


