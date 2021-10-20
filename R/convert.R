convert <- function(x, SBD, SD, to_t_ha = TRUE){
  if(to_t_ha){
    y <- (x * SD * SBD)/20
  } else {
    y <- (x * 20)/(SD * SBD)
  }
  return(y)
}
