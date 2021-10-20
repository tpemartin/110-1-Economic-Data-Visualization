type1 <- function(){
  dnorm(1990:2010, mean=1991, sd=3)*2500 -> x
    round(x, digits = 0)
}
type2 <- function(){
  dnorm(1994:2010, mean=1998, sd=2)*1000->x
  round(x, digits=0)
}
type3 <- function(){
  dnorm(2002:2010, mean=2005, sd=2.3)*800->x
  round(x, digits=0)
}

data_set4 <- data.frame(
  year=c(
    1990:2010,
    1994:2010,
    2002:2010),
  storage_type=
    c(
      rep("type1", 21),
      rep("type2", 17),
      rep("type3", 9)),
  sales_amount=c(
    type1(), 
    type2(),
    type3()
  )
)


