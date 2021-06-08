
# https://content.ces.ncsu.edu/soil-acidity-and-liming-basic-information-for-farmers-and-gardeners
CCE_adjust_size <- function(CCE, fine, medium) {
	# fine = smaller than 60-mesh size
	# medium is between 8- and 60-mesh size
	CCE * (medium * 0.5 + fine)
}

#https://ag.umass.edu/turf/fact-sheets/soil-ph-liming
CCE_adjust_depth <- function(CCE, depth) {
	x <- c(7.5, 10, 12.5, 17.5)
	y <- c(0.4, 0.6, 0.7, 1)
	CCE * stats::approx(x, y, depth, rule=2)$y
}


CCE <- function() {
	#https://extension.psu.edu/soil-acidity-and-aglime

	y <- matrix(c("Pure calcitic limestone", "CaCO3", "100", "Dolomitic limestone", "(Ca, Mg)CO3", "109", "Calcium oxide; lime, burnt, lump, or unslaked lime, quicklime", "CaO", "179", "Calcium hydroxide; hydrated, slaked, or builders' lime", "Ca(OH)2", "136", "Marl and shells", "CaCO3", "70-90", "Slag (various)", "CaSiO3", "60-90"), ncol=3, byrow=TRUE)
	colnames(y) <- c("Material", "Formula", "CCE (%)")
	y
}

