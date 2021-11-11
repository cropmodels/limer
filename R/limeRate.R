
if (!isGeneric("limeRate")) {setGeneric("limeRate", function(x, ...) standardGeneric("limeRate"))}

setMethod("limeRate", signature(x="SpatRaster"), 
	function(x, method, unit = "meq/100g", check_Ca=TRUE)  {
		lapp(x, method=method, unit=unit, check_Ca = check_Ca)
	}
)


setMethod("limeRate", signature(x="data.frame"), 
	function(x, method, unit = "meq/100g", check_Ca=TRUE)  {
		lr(exch_ac=x$exch_ac, ECEC=x$ECEC, method=method, unit=unit, 	
			SBD = x$SBD, SD = x$SD, check_Ca = check_Ca)
	}
)
