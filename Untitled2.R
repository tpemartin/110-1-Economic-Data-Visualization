axis_x_custom2 <- function(breaks, labels, ticks, majorLength, minor_majorRatio) {
  list(
    scale_x_continuous(
      breaks=breaks,
      labels=labels
    ), 
    theme(
      axis.ticks.length.x = unit(0,"mm")
    ), 
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
    ), 
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
      ))  )

}

