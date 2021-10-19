convert <- function(x, sbd, sd, to_t_ha = TRUE){
  if(to_t_ha){
    y <- (x * sd * sbd)/20
  } else {
    y <- (x * 20)/(sd * sbd)
  }
  return(y)
}
