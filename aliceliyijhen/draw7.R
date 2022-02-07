## ---------------------------------------------------------
library(dplyr)
library(stringr)
library(readxl)

careplace3 <- read_excel("C:/Users/user/Documents/careplace3 .xlsx")
#careplace3|>View()

careplace3[c(-17,-21,-22),]->careplace
careplace
#careplace|>View()
#careplace現在是沒有離島資料的數量dataframe

#接下來要合併sf_taiwan_simplified
#sf_taiwan_simplified$'台灣本島'$縣市|>View()
#careplace|>View()


## ---------------------------------------------------------

dplyr::left_join(sf_taiwan_simplified$'台灣本島'$縣市,careplace,by=c("map_id"="縣市"))->careplace

#careplace|>View()
#現在的careplace是完整資料型態的dataframe


## ---------------------------------------------------------
#第二種切法
careplace$數量|>
  cut(c(0,20,40,60,100,200,220),ordered_result = T)->.fct

.fct|>class()

levels(.fct ) <- c("0-20間","20-40間","40-60間","60-100間","100-200間","200-220間")

.fct->careplace$數量

careplace|>View()
arrange(careplace,desc(數量))|>View()


## ---------------------------------------------------------
#mp$choose_palette()
econDV2::Object(crplace)
crplace$register_palette <-
colorspace::sequential_hcl(n = 12, h = c(300, 212), c = c(62, NA, 2), l = c(25, 95), power = c(1.2, 1.3), register = "crplace")
library(shiny)
library(shinyjs)


## ---------------------------------------------------------
# background$'台灣本島'$縣市|>View()
#sf_taiwan_simplified$'台灣本島'$縣市|>View()

library(ggplot2)


careplace_first <- function(){
  background$'台灣本島'$縣市()+
    geom_sf(
      data=careplace,
      mapping=aes(
        fill=數量,
        label=map_id
      ),
      color="#e4df9a",
      size=0.15,
      inherit.aes=F
    )+
  colorspace::scale_fill_discrete_sequential(
    palette="crplace"
  )+theme_void()+
  labs(title="老人長期照顧、安養機構數縣市分布",subtitle="僅呈現台灣本島",caption="資料來源:行政院性別平等會")
}

plotly::ggplotly(careplace_first())

careplace_first()



## ---------------------------------------------------------
library(readxl)
library(dplyr)
demography <- read_excel("C:/Users/user/Downloads/demographytl1.xlsx")

#demography|>View()

#arrange(careplace,desc(數量))|>View()
dplyr::left_join(sf_taiwan_simplified$'台灣本島'$縣市,demography,by=c("map_id"="縣市"))->demography
#demography|>View()
arrange(demography,desc(人數))->demography
#demography|>View()
library(dplyr)

demography$人數




## ---------------------------------------------------------
#練習另一種
#先cut再合併不同於圖一
demography <- read_excel("C:/Users/user/Downloads/demographytl1.xlsx")


#demography|>View()
class(demography$八十歲以上人口數)

demography|>
  mutate(
    八十歲以上人口數=cut(人數,c(9000,10000,20000,30000,50000,70000,90000,120000),ordered_result=T)
  )->demography



#demography|>View()




dplyr::left_join(sf_taiwan_simplified$'台灣本島'$縣市,demography,by=c("map_id"="縣市"))->demography2
#demography2|>View()
arrange(demography2,desc(人數))


## ---------------------------------------------------------
library(ggplot2)
demographyph <- function(){
  background$'台灣本島'$縣市()+
    geom_sf(
      data=demography2,
      mapping=aes(
        fill=八十歲以上人口數,
        label=paste(map_id,人數,八十歲以上人口數)
      ),
      color="#e4df9a",
      size=0.15,
      inherit.aes=F
    )+
  colorspace::scale_fill_discrete_sequential(
    palette="crplace"
  )+theme_void()+
  labs(title="八十歲以上長者在各縣市分佈",subtitle="僅呈現台灣本島",caption="內政部戶政司全球資訊網")
}
#+
  #scale_fill_discrete(breaks=demography2$八十歲以上人口數,labels="台東","","","","","","","")
#length(demography2$八十歲以上人口數)
demographyph()
plotly::ggplotly(demographyph())


## ---------------------------------------------------------
data_ftt <- data.frame(
  x=c(1,2,4,5),
  #y_a是yes(有)
  #y_b是no(無)
  有規劃=c(17.4,24.2,58.76,51.09),
  無規劃=c(82.6,75.8,41.24,48.91)
)

data_ftt|>
  tidyr::pivot_longer(
    cols=有規劃:無規劃,
    names_to ="fill",
    values_to ="y"
  )->data_ftt

draw_ftt2 <- function(data1=data_ftt){
  ggplot(data=data1)+
    geom_col(
      aes(
        x=x,
        y=y,
        fill=fill
      )
    )+
    geom_text(
      aes(
        x=x,
        y=y,
        label=y,
        group=fill
      ),
      position=position_stack(vjust=0.5),
      size=3
  )+
    xlim(0,6)
}

draw_ftt2(data=data_ftt)




#1-102女
#2-102男
#4-106女
#5-106男

breaks=c(1,1.5,2,4,4.5,5)
labels=c("女","102年","男","女","106年","男")

draw_ftt2(data=data_ftt)+
  scale_x_discrete(
    breaks=breaks,
    labels=labels
  )->draw_ftt2$xaxis


ggplot(data=data_ftt)+
    geom_col(
      aes(
        x=x,
        y=y,
        fill=fill
      )
    )+
    geom_text(
      aes(
        x=x,
        y=y,
        label=y,
        group=fill
      ),
      position=position_stack(vjust=0.5),
      size=3
  )+
  scale_x_continuous(
    name='',
    breaks=breaks,
    labels=labels
  )+scale_y_continuous(
    name='比例'
  )+scale_fill_discrete(
    name=''
  )+theme(panel.background = element_rect(
    fill="#f2f2f2"
  ))+
  theme(plot.background = element_rect(
    fill="#f2f2f2"))+labs(
    title="台灣老人55-64歲退休規劃分析",
    subtitle="",
    caption="資料來源:衛生福利部統計處"
  )


  #scale_x_discrete(
    #breaks=breaks,
    #labels=labels
  #)


#geom_rug調整
#換背景
#結尾文字


