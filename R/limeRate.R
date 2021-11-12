
if (!isGeneric("limeRate")) {setGeneric("limeRate", function(x, ...) standardGeneric("limeRate"))}

setMethod("limeRate", signature(x="SpatRaster"), 
	function(x, method, unit = "meq/100g", check_Ca=TRUE, SD = 20,
	         TAS = NULL, target_Ve = NULL, X = NULL, crop_type = NULL)  {
		# does not work, I think I know why and will look into it
		terra::lapp(x, fun = .lr, usenames=TRUE, 
		            method=method, unit=unit, check_Ca=check_Ca, SD=SD,
		            TAS=TAS, target_Ve=target_Ve, X=X, crop_type=crop_type)
		# for now:
		#out <- rast(x, nlyrs=1)
		#setValues(out, limeRate(values(x), method=method, unit=unit, check_Ca=check_Ca, SD=SD))
	}
)

setMethod("limeRate", signature(x="data.frame"), 
	function(x, method, unit = "meq/100g", check_Ca=TRUE, SD = 20,
	         TAS = NULL, target_Ve = NULL, X = NULL, crop_type = NULL)  {
		x <- as.list(x)
		x$method <- method 
		x$unit <- unit
		x$check_Ca <- check_Ca
		x$SD <- SD
		x$TAS <- TAS
		x$target_Ve <- target_Ve
		x$X <- X
		x$crop_type <- crop_type
		do.call(.lr, x)
	}
)

setMethod("limeRate", signature(x="matrix"), 
	function(x, method, unit = "meq/100g", check_Ca=TRUE, SD = 20,
	         TAS = NULL, target_Ve = NULL, X = NULL, crop_type = NULL)  {
		limeRate(as.data.frame(x), 
		         method=method, unit=unit, check_Ca=check_Ca, SD=SD,
		         TAS=TAS, target_Ve=target_Ve, X=X, crop_type=crop_type)
	}
)
