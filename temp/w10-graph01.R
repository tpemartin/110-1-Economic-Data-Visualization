###########################################
#  01  平日較多夜間停留人數的區域           #
###########################################

library(readr)
library(tidyverse)

demo <- read_csv("temp/2020Nov_district.csv",locale = locale(encoding = "BIG5"))

subset1 <- demo %>%
  filter(NIGHT_WORK >= NIGHT_WEEKEND) %>% #View()
  select(COUNTY, TOWN) %>%
  mutate(
    COUNTY= as.factor(COUNTY)
  ) 

levels(subset1$COUNTY)
subset2 <- subset1 %>%
  group_by(COUNTY) %>%
  summarise(
    count=n()
  ) %>%
  arrange(count) %>%
  pull(COUNTY) -> newLevels

subset1$COUNTY <- factor(subset1$COUNTY, levels=as.character(newLevels))
subset1$COUNTY

# subset1$COUNTY <- as.factor(subset1$COUNTY)
# levels(subset1$COUNTY)
plot1 <- ggplot(
  data = subset1,
  aes( y = COUNTY))+
  geom_bar( fill = "cyan3",
            width = 0.5 #input$width
            )+
  geom_text(stat = "count",
            aes(label = (..count..)),
            vjust = -0.1, #input$vjust
            hjust = 0.1, #input$hjust
            size = 4 #input$size
    )+ 
  labs(title = "2020年11月各縣市平日夜間停留人數較多的區域數量",
    subtitle="")
  # + scale_x

plot1
