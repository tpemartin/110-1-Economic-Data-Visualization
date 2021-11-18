
#' Create flexible time x-axis design following Economist style
#'
#' @param breaks integer vector. where the labels will be placed at x axis
#' @param labels character vector. years where readers would see in the graph
#' @param ticks a list of two named elements, minor and major. ticks$major define where long ticks sit; ticks$minor defines where short ticks sit.
#' @param majorLength lenth of major ticks
#' @param minor_majorRatio minor-major ticks length ratio
#'
#' @return
#' @export
#'
#' @examples none
axis_x_cool <- function(breaks, labels, ticks, majorLength, minor_majorRatio) {
  list(
  scale_x_continuous(
    breaks=breaks,
    labels=labels
  ) ,
    theme(
      axis.ticks.length.x = unit(0,"mm")
    ) ,
    geom_rug(
      mapping=aes(
        x=ticks$major
      ),
      outside=TRUE, # draw rug outside the plot panel
      size=0.5, #input$majorsize
      length=grid::unit(
        majorLength, 
        "mm"
      )
    ) ,
    geom_rug(
      mapping=aes(
        x=ticks$minor
      ),
      outside = TRUE,
      size=0.5, #input$minorsize
      length=grid::unit(
        minor_majorRatio*majorLength,
        "mm"
      )
    ),
    coord_cartesian(clip="off"),
    theme(
      axis.text.x = element_text(
        margin = margin(
          12 #input$margin
        ),
        size=16 #input$textSize
      ))
  )
}

dataSet1 <- 
  data.frame(
    x=1979:2018
  )
set.seed(2038)
dataSet1$y <- sample(10:40, length(dataSet1$x), T)

ggplot()+
  geom_step(
    data=dataSet1,
    mapping=
      aes(
        x=x,
        y=y
      )
  )-> plot0


plot0+axis_x_cool(
  labels=c(
    "1979", "85", "90", "95", "2000", "05", "10", "15", "18"
  ),
  breaks= c(1979,seq(1985, 2015, by=5),2018),
  ticks=list(
    major=seq(1980, 2015, by=5),
    minor=c(1979, 2018)
  ),
  majorLength = 3,
  minor_majorRatio = 0.7
)
