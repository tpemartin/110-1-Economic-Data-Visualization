transfer_leftBreaks = function(breaks){
  scales::rescale(breaks, 
    from=c(-10,10), to=c(4, 12))
}
transferInv_leftBreaks <- function(breaks){
  scales::rescale(breaks, 
    to=c(-10,10), from=c(4, 12))
}
ggplot(
  data= ggplot4$data |>
    subset(time <= "2014-06-01"),
  mapping=aes(x=time)
) +
  geom_col(
    mapping=aes(
      y=ind_procution_change
    ),
    fill="#04a2d0"
  )+
  scale_y_continuous(
    name="Industrial production",
    limits = c(-10, 11),
    breaks = seq(-10, 10, by=5),
    labels = seq(-10, 10, by=5),
    sec.axis = sec_axis(
      name="Unemployment",
      trans=transfer_leftBreaks,
      breaks=seq(4, 12, by=2)
    )
  )#+

# add scaled unemployment
  geom_line(
    aes(
      y=transferInv_leftBreaks(unemploymentRate)
    ),
    color="#77230f"
  )
