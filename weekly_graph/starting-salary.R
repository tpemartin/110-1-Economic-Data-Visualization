jsonlite::fromJSON("https://raw.githubusercontent.com/tpemartin/110-1-Economic-Data-Visualization/main/data/salary.json") -> salary
salary$data$byIndustry %>% #View()
  filter(
    學歷別=="大學-薪資"
  ) %>%
  arrange(薪資) %>%
  pull(大職業別) -> newLevels
salary$data$byIndustry$大職業別 <- 
  factor(
    salary$data$byIndustry$大職業別,
    levels=newLevels
  )
salary$data$byIndustry %>%
  mutate(
    學歷別=factor(學歷別, levels=c("高中或高職-薪資", "大學-薪資")),
    學歷別2=factor(學歷別, levels=rev(c("高中或高職-薪資", "大學-薪資")))
  ) -> 
  salary$data$byIndustry
dy <- 200 #input$dy
ggplot() +
  geom_col(
    data = salary$data$byIndustry,
    aes(
      x = 大職業別,
      y = 薪資,
      fill = 學歷別2,
      group = 學歷別
    ),
    position = "dodge"
  ) +
  geom_text(
    data = salary$data$byIndustry,
    mapping = aes(
      x = 大職業別,
      y = 薪資 - dy,
      label = {
        label_ntd <- scales::label_dollar(
          prefix = "", suffix = "元"
        )
        label_ntd(薪資)
      },
      group = 學歷別
    ),
    hjust = 1,
    size = 4.5, #input$labelSize
    color = "white",
    position = position_dodge(width = 1)
  ) +
  scale_fill_brewer(
    name = "學歷",
    labels = c("大學", "高中或高職"),
    breaks = c("大學-薪資", "高中或高職-薪資"),
    type = "qual", palette = 3, direction = -1
  ) +
  scale_y_continuous(
    expand = expansion(0, 0),
    labels = label_ntd,
    position = "right"
  ) +
  theme(
    axis.line.x = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    axis.text = element_text(
      size = 12
    ),
    panel.grid.major.x = element_line(
      color = "grey"
    ),
    legend.position = "top",
    legend.text = element_text(
      size = 12
    )
  ) +
  coord_flip()+
  labs(
    title="2019年臺灣不同類別產業平均起薪行情"
  ) -> plot0
plot0
