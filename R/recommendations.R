lr <- function(method, unit = "meq/100g", check_Ca = TRUE, ...){
  
  l <- list(...)
  meth <- substr(tolower(method),1, 2)
  
  # check inputs. Add flexibility for exchangeable Al and acidity.

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
  
  if(is.null(l$exch_ac) & is.null(l$Al_sat)){
    if(is.null(l$exch_Al)){
      stop("Either exchangeable acidity, exchangeable Al, or Al saturation should be provided")
    } else {
      if(!is.null(l$exch_H)){
        if(length(l$exch_H) != length(l$exch_Al)){
          warning("Exchangeable Al and exchangeable H have different lengths")
        }
        l$exch_ac <- l$exch_Al + l$exch_H  
      } else {
        l$exch_ac <- l$exch_Al
      }
    }
  }  
  
  if(is.null(l$exch_ac) & is.null(l$ECEC)){
    stop("exchangeable acidity or exchangeable Al is needed to calculate ECEC")
  }
  
  if(is.null(l$ECEC)){
    nl <- names(l)
    ib <- which(grepl("exch", nl) & !grepl("exch_Al", nl) & !grepl("exch_H", nl))
    if(length(ib) == 0){
      stop("exchangeable bases values should be provided when ECEC is missing")
    }
    lb <- l[ib]
    if(length(unique(c(sapply(lb,length), length(l$exch_ac)))) != 1){
      warning("exchangeable cations values don't have the same length") 
    }
    l$ECEC <- Reduce(`+`, lb) + l$exch_ac
  }
  
  if(is.null(l$exch_ac)){
    if(length(l$Al_sat)!=length(l$ECEC)){
      stop("exchangeable cations and/or ECEC values should be the same length")
    }
    l$exch_ac <- l$Al_sat / 100 * l$ECEC
  }
  
  if(is.null(l$CEC_7) & !is.null(l$pot_ac) & !is.null(l$ECEC) & !is.null(l$exch_ac)){
    sum_of_bases <- l$ECEC - l$exch_ac
    l$CEC_7 <- sum_of_bases + l$pot_ac
  }
  
  if(is.null(l$pot_ac) & !is.null(l$CEC_7) & !is.null(l$ECEC) & !is.null(l$exch_ac)){
    sum_of_bases <- l$ECEC - l$exch_ac
    l$pot_ac <- l$CEC_7 - sum_of_bases
  }
  
  
  
  # compute lime requirement by method
  if(meth == "my"){
    message("using my method")
    if(is.null(l$TAS)) stop("TAS is needed for my method")
    if(is.null(l$a)) l$a <- 0.8
    if(is.null(l$b)) l$b <- 0.2
    lime <- lr_my(l$TAS, l$exch_ac, l$ECEC, a = l$a, b = l$b, clay = l$clay)
  }
  
  
  if(meth == "ka"){
    message("using Kamprath (1970) method")
    lime <- lr_ka(l$exch_ac)
  }
  
  
  if(meth == "co"){
    message("using Cochrane (1980) method")
    if(is.null(l$TAS)) stop("TAS is needed for Cochrane method")
    exch_Ca <- l$ECEC - l$exch_ac
    exch_Mg <- rep(0, length(exch_Ca))
    lime <- lr_co(l$TAS, l$exch_ac, exch_Ca, exch_Mg)
  }
  
  
  if(meth == "nu"){
    message("using NuMaSS method")
    if(is.null(l$TAS)) stop("TAS is needed for NuMaSS method")
    if(is.null(l$clay)){
      warning("no clay data. High ECEC/clay is assumed")
      l$clay <- 0.01
    } else {
      if(any(is.na(l$clay))){
        warning("NA in clay data. High ECEC/clay is assumed")
        l$clay[is.na(l$clay)] <- 0.01
      }
    } 
    lime <- lr_nu(l$TAS, l$exch_ac, l$ECEC, l$clay)
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
    exch_Ca <- l$ECEC - l$exch_ac
    exch_Mg <- exch_K <- exch_Na <- rep(0, length(exch_Ca))
    lime <- lr_bv(exch_Ca, exch_Mg, exch_K, exch_Na, 
                    l$CEC_7, l$target_Ve, l$crop_type)
  }
  
  
  if(meth %in% c("te", "gt")){
    message("using Goncalvez Teixeira method")
    if(is.null(l$pH)|is.null(l$OM)|is.null(l$pot_ac)){
      stop("pH, OM and pot_ac are needed for Goncalvez Texeira method")
    }
    lime <- lr_gt(l$pH, l$OM, l$pot_ac, l$X, l$exch_Ca, l$exch_Mg)
  }
  
  # check Ca deficiencies: 150kg/ha when Ca saturation < 25% (PS Book)
  if(check_Ca){
    if(is.null(l$exch_Ca)) stop("exch_Ca is needed to test for Ca deficiencies")
    Ca_def <- exch_Ca/ECEC < 0.25
    # 150 kg/ha to meq_Ca/ha 
    Ca_lime <- convert(0.15, l$SBD, l$SD, to_t_ha = FALSE)
    
    # correct lime rate to avoid Calcium deficiencies
    lime[Ca_def & lime < Ca_lime] <- Ca_lime
    
  }
  
  
  # remove negative values (for subtraction methods where the Al Sat<l$TAS or pH < TpH)
  lime[lime < 0] <- 0
  
  # unit transformation
  if(unit %in% c("kg/ha", "t/ha")){
    lime <- convert(lime, l$SBD, l$SD, to_t_ha = TRUE)
    if(unit == "kg/ha") lime <- lime * 1000
  }
  
  
  
  return(lime)
}

# end limer 






# individual formulas

# Kamprath (1970) 
lr_ka <- function(exch_Al, lf = 1.5){
  lime <- exch_Al * lf
  return(lime)
}


# Cochrane, et al. (1980).
lr_co <- function(TAS, exch_Al, exch_Ca, exch_Mg){
  ias <- 100 * exch_Al/(exch_Al + exch_Ca + exch_Mg)
  d_al <- exch_Al - (TAS/100 * (exch_Al + exch_Ca + exch_Mg))
  
  #lime <- ifelse(TAS > ias/3, 1.5 * d_al, 2 * d_al)
  lime <- 2 * d_al
  lime[TAS > ias/3] <- 1.5 * d_al
  
  return(lime)
}

# NUMASS (Osmond et al., 2002)

# lf is converted from original formula to get lime recommendation in meq/100g assuming that the original formula considered a soil depth of 15 cm and a soil bulk density of 1g/cm3

lr_nu <- function(TAS, exch_ac, ECEC, clay){
  #lf <- ifelse(ECEC/clay < 4.5, 10/3, 26/15) 
  lf <- rep(26/15, length(ECEC))
  lf[ECEC/clay < 4.5] <- 10/3
  
  #lime <- lf * (exch_ac - ECEC * TAS/100) + pmax(10 * ((19 - TAS)/100 * ECEC), 0)
  if(TAS >= 19){
    lime <- lf * (exch_ac - ECEC * TAS/100)  
  } else{
    lime <- lf * (exch_ac - ECEC * TAS/100) + 10 * ((19 - TAS)/100 * ECEC)
  }
  
  
  return(lime)
}


# Aramburu Merlos et al. xxx
lr_my <- function(TAS, exch_ac, ECEC, a = 0.8, b = 0.2, clay = NULL){
  if(!is.null(clay)){
    #a <- pmin(0.5 + (ECEC/clay)/40, 0.8)
    a <- 0.5 + (ECEC/clay)/40
    a[a > 0.8] <- 0.8
  }
  a.ac <- a * exch_ac
  TAS <- TAS/100 
  in.sqrt <- a.ac^2 - 4 * b * a.ac * (TAS - 1) * (TAS * ECEC - exch_ac)
  # to do: find a solution by optimization when in.sqrt is negative (?)
  # in.sqrt[in.sqrt < 0] <- 0
  lime <- exch_ac * ((a.ac - sqrt(in.sqrt)) / (2 * b * a.ac * (1 - TAS)))
  return(lime)
}



# Brazil V method (van Raij 1996) 

lr_bv <- function(exch_Ca, exch_Mg, exch_K, exch_Na, CEC_7, 
                    target_Ve = NULL, crop_type = "cereal"){
  if(is.null(target_Ve)){
    Ve <- data.frame(ct = c("pasture", "cereal", "legume", "vegetable", "fruit"), 
                     tVe = c(40, 50, 50, 70, 70))
    i <- match(crop_type, Ve$ct)
    target_Ve <- Ve$tVe[i]
    if(any(is.na(target_Ve))){
      stop("unrecognized crop type. Crop type can be pasture, cereal, legume, vegetable, or fruit.")
    }
  }
  
  V <- 100 * (exch_Ca + exch_Mg + exch_K + exch_Na) / CEC_7
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

