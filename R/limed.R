
# adjust parameter a based on clay content
.aclay <- function(ECEC, clay){
  pmin(0.5 + (ECEC/clay)/40, 0.8)
}


# Sum of Bases

# difference
.d_sb <- function(LR, ECEC, clay = NULL, a = 0.8){
  if(!is.null(clay)){
    a <- .aclay(ECEC, clay)
  }
  a * LR
}

# final
.f_sb <- function(LR, exch_ac, ECEC, clay = NULL, a = 0.8){
  d_sb <- .d_sb(LR, ECEC, clay, a)
  i_sb <- ECEC - exch_ac
  return(i_sb + d_sb)
}


# Exchangeable acidity

# difference
.d_ac <- function(LR, exch_ac, ECEC, clay = NULL, a = 0.8, b = 0.2){
  d_sb <- .d_sb(LR, ECEC, clay, a)
  d_sb * (1 - b * LR/exch_ac)
}

# final
.f_ac <- function(LR, exch_ac, ECEC, clay = NULL, a = 0.8, b = 0.2){
  d_ac <- .d_ac(LR, exch_ac, ECEC, clay, a, b)
  exch_ac - d_ac
}

# ECEC
# final
.f_ECEC <- function(LR, exch_ac, ECEC, clay = NULL, a = 0.8, b = 0.2){
  f_sb <- .f_sb(LR, exch_ac, ECEC, clay, a)
  f_ac <- .f_ac(LR, exch_ac, ECEC, clay, a, b)
  f_sb + f_ac
}

# Al Saturation
# final
.f_Al_sat <- function(LR, exch_ac, ECEC, clay = NULL, a = 0.8, b = 0.2){
  f_ac <- .f_ac(LR, exch_ac, ECEC, clay, a, b)
  f_ECEC <- .f_ECEC(LR, exch_ac, ECEC, clay, a, b)
  100 * f_ac/f_ECEC
}
