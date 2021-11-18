my2ndplot <- function(myx, myy) {
  ggplot()+
    geom_col(
      mapping=
        aes(
          x=myx,
          y=myy
        )
    )+
    coord_flip()
}

my2ndplot(
  myx=c("Tom", "Barry", "Jane"),
  myy=c(177, 182, 170)
)

  candidate_vote_outcome <- function(candidate_votes) {
    dx=4 #input$dx
    h=0.5 #input$h
    dt=0 #input$dt
    ggplot()+
      geom_col(
        mapping=aes(
          x=c(1, 1),
          y=candidate_votes,
          fill=c("A","B")
        ),
        width=1
      )+
      geom_segment(
        mapping=aes(
          x=1-h,
          y=sum(candidate_votes)/2+1,
          xend=1+h,
          yend=sum(candidate_votes)/2+1
        )
      )+
      geom_text(
        mapping=aes(
          x=1+dt,
          y=sum(candidate_votes)/2+1,
          label=as.character(sum(candidate_votes)/2+1)
        ),
        size=8 #input$text
      )+
      xlim(1-dx, 1+dx)+ # make sure cover 0.5-1.5 so the bar width can be accomodate
      coord_flip()+
      theme_void()+
      theme(legend.position = "none") 
  }

candidate_vote_outcome(
  candidate_votes = c(299, 172)
) -> plot1
plot1
