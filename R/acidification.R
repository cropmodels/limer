
acidification_equivalents <- function() {
	x <- matrix(c("Potassium Nitrate", "13", "0", "Calcium Nitrate", "15.5", "0", "Ammonium Nitrate", "34", "3.6", "Anhydrous Ammonia", "80-82", "3.6", "Urea", "46", "3.6", "UAN Solutions", "28-32", "3.6", "Diammonium Phosphate (DAP)", "18", "5.4", "Ammonium Sulfate", "21", "7.2", "Monoammonium Phosphate (MAP)", "10-11", "7.2"), ncol=3, byrow=TRUE)
	colnames(x) <- c("Fertilizer", "N content (%)", "lime needed (kg(ECC)/kg(N))")
	x <- data.frame(x, check.names=FALSE)
	x[, 3] <- as.numeric(x[,3])
	x
}

