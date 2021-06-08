
# https://content.ces.ncsu.edu/soil-acidity-and-liming-basic-information-for-farmers-and-gardeners
effective_CCE <- function(CCE, fine, medium) {
	# fine = smaller than 60-mesh size
	# medium is between 8- and 60-mesh size
	CCE * (medium * 0.5 + fine)
}

CCE <- function() {
	#https://extension.psu.edu/soil-acidity-and-aglime

	y <- matrix(c("Pure calcitic limestone", "CaCO3", "100", "Dolomitic limestone", "(Ca, Mg)CO3", "109", "Calcium oxide; lime, burnt, lump, or unslaked lime, quicklime", "CaO", "179", "Calcium hydroxide; hydrated, slaked, or builders' lime", "Ca(OH)2", "136", "Marl and shells", "CaCO3", "70-90", "Slag (various)", "CaSiO3", "60-90"), ncol=3, byrow=TRUE)
	colnames(y) <- c("Material", "Formula", "CCE (%)")
	y
}

