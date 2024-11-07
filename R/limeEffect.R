
if (!isGeneric("limeEffect")) {setGeneric("limeEffect", function(x, ...) standardGeneric("limeEffect"))}

setMethod("limeEffect", signature(x="SpatRaster"), 
	function(x, ...)  {
		terra::lapp(x, fun = .litas_inverse, filename=filename, overwrite=overwrite, wopt=list(...))
	}
)

setMethod("limeEffect", signature(x="data.frame"), 
	function(x) {	 
		do.call(.litas_inverse, as.list(x))
	}
)

setMethod("limeEffect", signature(x="matrix"), 
	function(x)  {
		limeEffect(as.data.frame(x))
	}
)
