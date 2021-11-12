lr <- function(method, unit = "meq/100g", check_Ca = TRUE, ...){
  
  l <- list(...)
  meth <- substr(tolower(method), 1, 2)
  
  # check inputs.  ########
  
  ## General -----
  av.meth <- c("my", "ka", "co", "nu", "bv", "br", "gt", "te")
  
  if(length(meth) > 1){
    stop("lime requirement can be calculated with only one method at a time")
  }
  
  if(!meth %in% av.meth){
    stop("unrecognized method")
  }
  
  if(unit %in% c("kg/ha", "t/ha") | check_Ca){
    if(is.null(l$SBD) | is.null(l$SD)){
      stop("Soil bulk density (SBD) and lime incorporation depth (SD) are needed to estimate lime requirement in kg or t per ha and for Ca deficiencies check")
    }
  }
  
  if(check_Ca & is.null(l$exch_Ca)){
    stop("exch_Ca is needed to test for Ca deficiencies")
  }
  
  ## Flexibility ------
  if(is.null(l$exch_ac)){
    if(is.null(l$exch_Al)){
      stop("Either exchangeable acidity or exchangeable Al should be provided")
    } else {
      if(!is.null(l$exch_H)){
        l$exch_ac <- l$exch_Al + l$exch_H  
      } else {
        l$exch_ac <- l$exch_Al
      }
    }
  }  
  
  if(is.null(l$ECEC) & meth != "ka"){
    nl <- names(l)
    ib <- which(grepl("exch", nl) & !grepl("exch_Al", nl) & !grepl("exch_H", nl))
    if(length(ib) == 0){
      stop("exchangeable bases values should be provided when l$ECEC is missing")
    }
    lb <- l[ib]
    l$ECEC <- Reduce(`+`, lb) + l$exch_ac
  }
  
  if(meth %in% c("bv", "br", "gt", "te")){
    l$exch_bases <- l$ECEC - l$exch_ac
    if(is.null(l$CEC_7) & is.null(l$pot_ac)){
      stop("CEC_7 or pot_ac is needed for this method")
    }
    if(meth %in% c("bv", "br") & is.null(l$CEC_7) & !is.null(l$pot_ac)){
      l$CEC_7 <- l$pot_ac + l$exch_bases
    }
    if(meth %in% c("gt", "te") & !is.null(l$pot_ac) & is.null(l$CEC_7)){
      l$pot_ac <- l$CEC_7 - l$exch_bases
    }
  }

  ## Specific ----
  if(meth %in% c("my", "co", "nu")){
    if(is.null(l$TAS)) stop("TAS is needed for this method")
  }
  
  if(meth %in% c("bv","br")){
    if(is.null(l$target_Ve) & is.null(l$crop_type)){
      stop("target base saturation (target_Ve) or crop type are needed for this method")
    }
  }
  
  if(meth %in% c("gt", "te")){
    if(is.null(l$pH)|is.null(l$OM)|is.null(l$pot_ac)){
      stop("pH, OM, and pot_ac or CEC_7 are needed for Goncalvez Texeira method")
    }
  }
  
  
  # compute lime requirement by method ######
  
  if(meth == "ka"){
    # message("using Kamprath (1970) method")
    lime <- .lr_ka(exch_ac = l$exch_ac)
  }
  
  if(meth == "co"){
    # message("using Cochrane et al. (1980) method")
    lime <- .lr_co(exch_ac = l$exch_ac, ECEC = l$ECEC, TAS = l$TAS)
  }
  
  if(meth == "nu"){
    # message("using NuMaSS method")
    lime <- .lr_nu(exch_ac = l$exch_ac, ECEC = l$ECEC, TAS = l$TAS, clay = l$clay)
  }
  
  if(meth == "my"){
    # message("using my method")
    lime <- .lr_my(exch_ac = l$exch_ac, ECEC = l$ECEC, TAS = l$TAS, clay = l$clay)
  }
  
    
  if(meth %in% c("bv","br")){
    # message("using Brazil V method")
    lime <- .lr_bv(exch_bases = l$exch_bases, CEC_7 = l$CEC_7, 
                   target_Ve = l$target_Ve, crop_type = l$crop_type)
  }
  
  if(meth %in% c("te", "gt")){
    # message("using Goncalvez Teixeira method")
    lime <- .lr_gt(pH = l$pH, OM = l$OM, pot_ac = l$pot_ac, X = l$X, 
                   exch_Ca = l$exch_Ca, exch_Mg = l$exch_Mg)
  }
  
  # Final steps #########
  
  # remove negative values 
  lime[lime < 0] <- 0
  
  # unit transformation
  if(unit %in% c("kg/ha", "t/ha")){
    lime <- convert(lime, l$SBD, l$SD, to_t_ha = TRUE)
    if(unit == "kg/ha") lime <- lime * 1000
  }
  
  if(check_Ca){
    lime <- .ca_def(lime = lime, exch_Ca = l$exch_Ca, ECEC = l$ECEC, 
                    unit = unit, SBD = l$SBD, SD = l$SD)
  }
  return(lime)
}

# end limer 






# individual formulas

# Kamprath (1970) 
.lr_ka <- function(exch_ac, lf = 1.5){
  lime <- exch_ac * lf
  return(lime)
}


# Cochrane, et al. (1980).
.lr_co <- function(exch_ac, ECEC, TAS){
  ias <- 100 * exch_ac/ECEC
  d_ac <- exch_ac - (TAS/100 * ECEC)
  llf <- TAS > ias/3
  lime <- ifelse(llf, 1.5 * d_ac, 2 * d_ac)
  return(lime)
}

# NUMASS (Osmond et al., 2002)

# lf is converted from original formula to get lime recommendation in meq/100g assuming that the original formula considered a soil depth of 15 cm and a soil bulk density of 1g/cm3

.lr_nu <- function(exch_ac, ECEC, TAS, clay = NULL){
  # normal lf (high clay activity)
  lime1 <- 26/15 * exch_ac - (TAS/100 * ECEC)
  #if clay data is available, modify lf when clay activity is very low
  if(!is.null(clay)){
    lclay <- ECEC/clay < 4.5 & !is.na(clay) & !is.na(ECEC)
    lime1[lclay] <- lime1[lclay] * 25/13    
  }
  # second part of the formula, which has to be positive
  lime2 <- 10 * ((19 - TAS)/100 * ECEC)
  lime2[lime2 < 0] <- 0
  # add first and second part
  lime <- lime1 + lime2
  return(lime)
}


# Aramburu Merlos et al. xxx
.lr_my <- function(exch_ac, ECEC, TAS, clay = NULL){
  b <- 0.2
  if(is.null(clay)){
  a <- 0.8  
  } else {
    a <- 0.5 + (ECEC/clay)/40
    a[a > 0.8] <- 0.8
  }
  a.ac <- a * exch_ac
  tas <- TAS/100
  in.sqrt <- a.ac^2 - 4*b*a.ac * (tas - 1) * (tas*ECEC - exch_ac)
  # to do: find a solution by optimization when in.sqrt is negative (?)
  # in.sqrt[in.sqrt < 0] <- 0
  lime <- exch_ac * ((a.ac - sqrt(in.sqrt)) / (2*b*a.ac * (1 - tas)))
  return(lime)
}


# Brazil V method (van Raij 1996) 

.lr_bv <- function(exch_bases, CEC_7, target_Ve = NULL, crop_type){
  if(is.null(target_Ve)){
    Ve <- data.frame(ct = c("pasture", "cereal", "legume", "vegetable", "fruit"), 
                     tVe = c(40, 50, 50, 70, 70))
    i <- match(crop_type, Ve$ct)
    target_Ve <- Ve$tVe[i]
    if(any(is.na(target_Ve))){
      stop("unrecognized crop type. Crop type can be pasture, cereal, legume, vegetable, or fruit.")
    }
  }
  Ve <- target_Ve/100
  lime <- CEC_7 * Ve - exch_bases
  return(lime)  
}


# Goncalvez Teixeira et al., (2020)
.lr_gt <- function(pH, OM, pot_ac, X = NULL, exch_Ca = NULL, exch_Mg = NULL){
  om5 <- 0.0699 * (((5.8 - pH)* OM)^0.9225)
  ac5 <- 0.3750 * (((5.8 - pH)* pot_ac)^0.9127)
  om6 <- 0.1059 * (((6.0 - pH)* OM)^0.8729)
  ac6 <- 0.4558 * (((6.0 - pH)* pot_ac)^0.9162)
  m <- cbind(om5, om6, ac5, ac6)
  
  if(!is.null(X)){
    if(length(X) > 1) stop("X should be only one value")  
    if(is.null(exch_Ca) | is.null(exch_Mg)){
      stop("exchangeable Ca and Mg are required to adjust for crop Ca and Mg requirement")
    }
    if(length(exch_Ca) != length(exch_Mg)){
      warning("exchangeable Ca and Mg have different lengths")
    }
    # lime required to reach the minimum Ca and Mg level 
    lr_CaMg <- rep(X, length(exch_Ca)) - exch_Ca - exch_Mg
    # omit lime requirements that do not meet the min Ca-Mg level
    lX <- apply(m, 2, FUN = function(x) x < lr_CaMg)
    m[lX] <- NA
    # if all lr are lower than the min level for Ca and Mg
    all.na <- apply(m, 1, function(x)all(is.na(x)))
    # assign the min lr for Ca and Mg in the first column 
    m[all.na, 1] <- ifelse(length(lr_CaMg) == 1, lr_CaMg, lr_CaMg[all.na])
  }
  # select the min lr for each observation
  lime <- apply(m, 1, min, na.rm = T)
  # the lr cannot exceed the potential acidity of the soil
  lime[lime > pot_ac] <- pot_ac
  return(lime)
}

# check Ca deficiencies: 150kg/ha when Ca saturation < 25% (PS Book)
.ca_def <- function(lime, exch_Ca, ECEC, unit, SBD, SD){
  def <- exch_Ca/ECEC < 0.25 & !is.na(exch_Ca) & !is.na(ECEC)
  if(unit == "kg/ha"){
    dose <- 150
  } else if(unit == "t/ha"){
    dose <- 0.15
  } else {
    dose <- convert(0.15, SBD, SD, to_t_ha = FALSE)[def]
    dose[is.na(dose)] <- 0 # if dose is.na, keep current lime rate (including NAs)
  }
  lime[def] <- pmax(lime[def], dose)
  return(lime)
}
