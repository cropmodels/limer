lr <- function(exch_ac = NULL, ECEC = NULL, method, 
               unit = "meq/100g", SBD = NULL, SD = NULL, 
               check_Ca = TRUE, ...){
  
  l <- list(...)
  meth <- substr(tolower(method), 1, 2)
  
  # check inputs.  ########
  # Add flexibility for exchangeable Al and acidity.
  
  av.meth <- c("my", "ka", "co", "nu", "bv", "br", "gt", "te")
  
  if(length(meth) > 1){
    stop("lime requirement can be calculated with only one method at a time")
  }
  
  if(!meth %in% av.meth){
    stop("unrecognized method")
  }
  
  
  if(unit %in% c("kg/ha", "t/ha") | check_Ca){
    if(is.null(SBD) | is.null(SD)){
      stop("Soil bulk density (SBD) and lime incorporation depth (SD) are needed to estimate lime requirement in kg or t per ha and for Ca deficiencies check")
    }
  }
  
  if(check_Ca & is.null(l$exch_Ca)){
    stop("exch_Ca is needed to test for Ca deficiencies")
  }
  
  if(is.null(exch_ac)){
    if(is.null(l$exch_Al)){
      stop("Either exchangeable acidity or exchangeable Al should be provided")
    } else {
      if(!is.null(l$exch_H)){
        exch_ac <- l$exch_Al + l$exch_H  
      } else {
        exch_ac <- l$exch_Al
      }
    }
  }  
  
  if(is.null(ECEC)){
    nl <- names(l)
    ib <- which(grepl("exch", nl) & !grepl("exch_Al", nl) & !grepl("exch_H", nl))
    if(length(ib) == 0){
      stop("exchangeable bases values should be provided when ECEC is missing")
    }
    lb <- l[ib]
    ECEC <- Reduce(`+`, lb) + exch_ac
  }
  
  if(is.null(l$CEC_7) & !is.null(l$pot_ac)){
    l$CEC_7 <- ECEC - exch_ac + l$pot_ac
  }
  
  if(is.null(l$pot_ac) & !is.null(l$CEC_7)){
    l$pot_ac <- l$CEC_7 - (ECEC - exch_ac)
  }
  
  
  # compute lime requirement by method ######
  
  if(meth == "my"){
    message("using my method")
    if(is.null(l$TAS)) stop("TAS is needed for my method")
    if(is.null(l$a)) l$a <- 0.8
    if(is.null(l$b)) l$b <- 0.2
    lime <- lr_my(l$TAS, exch_ac, ECEC, a = l$a, b = l$b, clay = l$clay)
  }
  
  
  if(meth == "ka"){
    message("using Kamprath (1970) method")
    lime <- lr_ka(exch_ac)
  }
  
  
  if(meth == "co"){
    message("using Cochrane (1980) method")
    if(is.null(l$TAS)) stop("TAS is needed for Cochrane method")
    lime <- lr_co(l$TAS, exch_ac, ECEC)
  }
  
  
  if(meth == "nu"){
    message("using NuMaSS method")
    if(is.null(l$TAS)) stop("TAS is needed for NuMaSS method")
    if(is.null(l$clay)){
      warning("no clay data. High ECEC/clay is assumed")
      l$clay <- 0.01
    }
    lime <- lr_nu(l$TAS, exch_ac, ECEC, l$clay)
  }
  
  
  if(meth %in% c("bv","br")){
    message("using Brazil V method")
    if(is.null(l$CEC_7)){
      stop("CEC (at pH 7) is needed for Brazil V method")
    }
    if(is.null(l$target_Ve) & is.null(l$crop_type)){
      warning("no target base saturation or crop type provided for Brazil V method. A cereal crop type with a target base saturation of 50% was assumed")
      l$target_Ve <- 50
    }
    exch_bases <- ECEC - exch_ac
    lime <- lr_bv(exch_bases, l$CEC_7, l$target_Ve, l$crop_type)
  }
  
  
  if(meth %in% c("te", "gt")){
    message("using Goncalvez Teixeira method")
    if(is.null(l$pH)|is.null(l$OM)|is.null(l$pot_ac)){
      stop("pH, OM and pot_ac are needed for Goncalvez Texeira method")
    }
    if(!is.vector(l$pH)){
      stop("Goncalvez Teiexeira method only accepts vector values")
    }
    lime <- lr_gt(l$pH, l$OM, l$pot_ac, l$X, l$exch_Ca, l$exch_Mg)
  }
  
  # remove negative values 
  lime[lime < 0] <- 0
  
  # unit transformation
  if(unit %in% c("kg/ha", "t/ha")){
    lime <- convert(lime, SBD, SD, to_t_ha = TRUE)
    if(unit == "kg/ha") lime <- lime * 1000
  }
  
  # check Ca deficiencies: 150kg/ha when Ca saturation < 25% (PS Book)
  if(check_Ca){
    
    Ca_def <- l$exch_Ca/ECEC < 0.25
    
    if(unit == "kg/ha"){
      lime[Ca_def & lime < 150] <- 150
    } else if(unit == "t/ha"){
      lime[Ca_def & lime < 0.15] <- 0.15
    } else {
      Ca_lime <- convert(0.15, SBD, SD, to_t_ha = FALSE)
      i <- Ca_def & lime < Ca_lime
      lime[i] <- Ca_lime[i]
    }
  }
  return(lime)
}

# end limer 






# individual formulas

# Kamprath (1970) 
lr_ka <- function(exch_ac, lf = 1.5){
  lime <- exch_ac * lf
  return(lime)
}


# Cochrane, et al. (1980).
lr_co <- function(TAS, exch_ac, ECEC){
  ias <- 100 * exch_ac/ECEC
  
  #lime <- ifelse(TAS > ias/3, 1.5 * d_al, 2 * d_al)
  lime <- 2 * (exch_ac - (TAS/100 * ECEC))
  #lf is 1.5 when TAS is lower than IAS/3, it is 2 otherwise
  llf <- TAS > ias/3  
  # changing the lf from 2 to 1.5 is he same as multiplying by 0.75 (1.5/2)
  lime[llf] <- lime[llf] * 0.75 
  
  return(lime)
}

# NUMASS (Osmond et al., 2002)

# lf is converted from original formula to get lime recommendation in meq/100g assuming that the original formula considered a soil depth of 15 cm and a soil bulk density of 1g/cm3

lr_nu <- function(TAS, exch_ac, ECEC, clay){
  #lf <- ifelse(ECEC/clay < 4.5, 10/3, 26/15) 
  #lime <- lf * (exch_ac - ECEC * TAS/100) + pmax(10 * ((19 - TAS)/100 * ECEC), 0)
    
  if(!is.vector(TAS) | length(TAS) > 1){
    stop("only one TAS is accepted")
  }
  
  lime1 <- 26/15 * (exch_ac - ECEC * TAS/100)
  lclay <- ECEC/clay < 4.5
  # divide and multiply by different lf based on ECEC/clay
  lime1[lclay] <- lime1[lclay] * (10/3) / (26/15)
  
  # second part of the formula, which has to be positive
  lime2 <- 10 * ((19 - TAS)/100 * ECEC)
  lime2[lime2 < 0] <- 0
  # add first and second part
  lime <- lime1 + lime2
  
  return(lime)
}


# Aramburu Merlos et al. xxx
lr_my <- function(TAS, exch_ac, ECEC, a = 0.8, b = 0.2, clay = NULL){
  if(!is.null(clay)){
    a <- 0.5 + (ECEC/clay)/40
    a[a > 0.8] <- 0.8
  }
  TAS <- TAS/100 
  in.sqrt <- (a*exch_ac)^2 - 4*a*b*exch_ac * (TAS - 1) * (TAS*ECEC - exch_ac)
  # to do: find a solution by optimization when in.sqrt is negative (?)
  # in.sqrt[in.sqrt < 0] <- 0
  lime <- exch_ac * ((a*exch_ac - sqrt(in.sqrt)) / (2*a*b*exch_ac * (1 - TAS)))
  return(lime)
}



# Brazil V method (van Raij 1996) 

lr_bv <- function(exch_bases, CEC_7, target_Ve = NULL, crop_type = "cereal"){
  if(is.null(target_Ve)){
    Ve <- data.frame(ct = c("pasture", "cereal", "legume", "vegetable", "fruit"), 
                     tVe = c(40, 50, 50, 70, 70))
    i <- match(crop_type, Ve$ct)
    target_Ve <- Ve$tVe[i]
    if(any(is.na(target_Ve))){
      stop("unrecognized crop type. Crop type can be pasture, cereal, legume, vegetable, or fruit.")
    }
  }
  
  V <- 100 * (exch_bases) / CEC_7
  lime <- CEC_7 * (target_Ve - V)/100
  return(lime)  
}


# Goncalvez Teixeira et al., (2020)
lr_gt <- function(pH, OM, pot_ac, X = NULL, exch_Ca = NULL, exch_Mg = NULL){
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

