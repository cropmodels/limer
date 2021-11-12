
if (!isGeneric("limeRate")) {setGeneric("limeRate", function(x, ...) standardGeneric("limeRate"))}

setMethod("limeRate", signature(x="SpatRaster"), 
	function(x, method, unit = "meq/100g", check_Ca=TRUE)  {
		# does not work, I think I know why and will look into it
		#lapp(x, fun=lr, usenames=TRUE, method=method, unit=unit, check_Ca = check_Ca)

		# for now:
		out <- rast(x, nl=1)
		setValues(out, limeRate(values(x), method=method, unit=unit, check_Ca=check_Ca)	)
	}
)

setMethod("limeRate", signature(x="data.frame"), 
	function(x, method, unit = "meq/100g", check_Ca=TRUE)  {
		x <- as.list(x)
		x$method=method 
		x$unit=unit
		x$check_Ca = check_Ca
		do.call(lr, x)
	}
)

setMethod("limeRate", signature(x="matrix"), 
	function(x, method, unit = "meq/100g", check_Ca=TRUE)  {
		limeRate(as.data.frame(x), method=method, unit=unit, check_Ca=check_Ca)
	}
)
