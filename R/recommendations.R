# wrapper function to compute lime requriments in a single step by selecting the method and specifying additional variables

limer <- function(..., 
                  method = c("standard", "Brazil V", "", "Al tolerance")){
  
}


# Pedro Sanchez book formulas ----------

## Sanchez, P. A. (2019). Properties and Management of Soils in the Tropics. Cambridge University Press. Chapter 9


## Standard lime recommendation ----------

# Kamprath (1970) https://doi.org/10.2136/sssaj1970.03615995003400020022x
# alpha ranges between 1.65 and 4 depending on soil buffer capacity and crop Al tolerance

st_lime <- function(exch_Al, alpha = 1.65){
  lime <- exch_Al * alpha
  return(lime)
}


# Aluminum tolerance method -------------------

# Cochrane, et al. (1980).

# "the factor 1.8 is replaced by 2.4 when the estimated lime required using the factor 1.8 is greater than the chemical lime equivalent of the exchangeable Al" 

# target_Al_sat might be the Al saturation tolerated by the crop

at_lime <- function(exch_Al, target_Al_sat, exch_Ca, exch_Mg, 
                              alpha = 1.8){
  lime <- alpha * (exch_Al - target_Al_sat/100 * (exch_Al + exch_Ca + exch_Mg))
  return(lime)
                   
}



# Brazil V method ------------------

bv_lime <- function(exch_Ca, exch_Mg, exch_K, exch_Na, CEC_7, 
                          target_Ve = NULL, crop_type = "cereal"){
  if(is.null(target_Ve)){
    Ve <- cbind(c("pasture", "cereal", "legume", "vegetable", "fruit"), 
                c(40,50,50,70,70))
    target_Ve <- Ve[Ve[,1] == crop_type, 2]
  }
  V <- 100 * (exch_Ca + exch_Mg + exch_K + exch_Na) / CEC_7
  lime <- CEC_7 * (target_Ve - V)
  return(lime)  
}





# Other functions --------------------------

# ACID4 method (Yost et al., 1988)

# "adapted from Cochrane et al. (1980): the value 1.4 represents the relation of the cmol of CaC03 required to neutralize 1 cmol of AI + H in field studies adjusted for both bulk density and depth of incorporation. In this case 1.9 cmol of Ca was required for each cmol ofAl + H. the bulk density was assumed to be 1.0. and the depth of incorporation was assumed to be 15 cm"

a4_lime <- function(exch_ac, target_Al_sat, ECEC){
  lime <- 1.4 * (exch_ac - (target_Al_sat * ECEC /100))
  return(lime)
}

## North Carolina function ----

# https://content.ces.ncsu.edu/soil-acidity-and-liming-basic-information-for-farmers-and-gardeners
nc_lime <- function(lime, CCE, pH, Ac) {
	lime <- lime / 2471    # kg/ha
	lime <- lime * CCE/90  # standard CCE = 90%
	lime * (6.6 - pH) / Ac 
}
# https://www.ncagr.gov/agronomi/obt21.htm
# Estimtes Ac in meq/100 cm3

nc_acidity <- function(BpH) {
	4 * (6.6 - BpH)
}


