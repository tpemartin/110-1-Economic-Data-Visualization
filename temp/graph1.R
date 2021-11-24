library(readxl)
# index <- read_excel("C:/Users/user/Desktop/index.xlsx")
index <- read_excel("temp/index.xlsx")

View(index)
colnames(index)[3] <- "日經225"

index %>%
  mutate(
    across(
      .cols = 2:8,
      .fns =  function(x) (x/x[[1]] -1)*100 #anonymous function
    )
  ) -> index4

index4 <- index
for(ii in 2:8){
  index4[ii] <- 
    (index4[ii]-index[[1,ii]])/index4[[1,ii]]*100
}


point_line <- function( data=index4,y=index$道瓊工業平均,color="red",linesize=2,pointsize=3) {
  list(geom_line(
    data = data,
    mapping=aes(
      x=`月份`,
      y=y),
    color="white",
    size=linesize+1,
    
  ),
  geom_line(
    data = data,
    mapping=aes(
      x=`月份`,
      y=y),
    color=color,
    size=linesize,
    
    
  ),
  geom_point(
    data= data,
    mapping=aes(
      x=`月份`,
      y=y),
    fill=color,
    size=pointsize,
    shape=21,  
    color="white",
    stroke=0.8,
  ))
}


ggplot()+
  geom_hline(
    yintercept = 0,
    color="#f30606",
    linetype=5,
    size=1 
  )+
  point_line(y=index4$道瓊工業平均,color="#983d4d" )+
  point_line(y=index4$日經225,color="blue")+
  point_line(y=index4$`香港(恆生指數)`,color="#076ea1")+
  point_line(y=index4$蘇黎士,color="#9e9ca0")+
  point_line(y=index4$`法蘭克福(商銀指數)`,color="#648596")+
  point_line(y=index4$`倫敦(金融時報指數)`,color="#585659")+
  point_line(y=index4$臺灣加權股價指數,color="#2dc1d3")+
  geom_text(
    mapping=aes(
      x=7,
      y=0.5,
      label="臺灣加權股價指數"),
    color="#2dc1d3",
    size=4.5, 
    hjust=0 ,
    vjust=-8 
  )+
  geom_text(
    mapping=aes(
      x=9,
      y=0,
      label="日經225"),
    color="blue",
    size=4.5, 
    hjust=-0.3 ,
    vjust=-0.5 
  )+geom_text(
    mapping=aes(
      x=8,
      y=-10,
      label="道瓊工業平均"),
    color="#983d4d",
    size=4.5, 
    hjust=0.6 ,
    vjust=-1 
  )+geom_text(
    mapping=aes(
      x=3,
      y=-25,
      label="法蘭克福(商銀指數)"),
    color="#648596",
    size=4.5, 
    hjust=0.5 ,
    vjust=1.3 
  )+geom_text(
    mapping=aes(
      x=4,
      y=-10,
      label="蘇黎士"),
    color="#9e9ca0",
    size=4.5, 
    hjust=0.5 ,
    vjust=-1 
  )+geom_text(
    mapping=aes(
      x=8,
      y=-12,
      label="香港(恆生指數)"),
    color="#076ea1",
    size=4.5, 
    hjust=-1 ,
    vjust=4.5
  )+geom_text(
    mapping=aes(
      x=8,
      y=-22,
      label="倫敦(金融時報指數)"),
    color="#585659",
    size=4.5, 
    hjust=1 ,
    vjust=1 #input$v
  )+scale_x_continuous(
    breaks = c(seq(1,10,2),12), 
    labels=c("Jan.","Mar.","May.","Jul.","Sept.","Dec.")
  ) +
  labs( title="2020年國際主要股市指數",
        subtitle="percentage change since January 2020",
        caption="Source:https://www.twse.com.tw/zh/",
        x = "", 
        y = "% change",
  )+
  theme(  plot.title =element_text( size=22 ),
          plot.subtitle =element_text( size=14 ),
          text = element_text( size=13 )  ,
          axis.text=element_text( size=11 )
  )
