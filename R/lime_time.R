
.lime_time <- function(years=15, exch_ac, ECEC, TAS, acidification=0, decay=0.22, rates=FALSE) {
	
	message('to be removed. Use "maintenanceRate" or "reacidification" instead')

	y <- data.frame(exch_ac=exch_ac, ECEC=ECEC, TAS=TAS)
	nms <- c("year", apply(y, 1, \(i) paste(i, collapse="_")))
	rate <- limeRate(y, method="my", check_Ca=FALSE, TAS=y$TAS)
	exal <- y$ECEC * y$TAS / 100 
	if (rates) {
		y$exch_ac <- pmin(y$exch_ac, exal + decay + acidification)
		rate2 <- limeRate(y, method="my", check_Ca=FALSE, TAS=y$TAS)
		out <- data.frame(c(0, 1), rbind(rate, rate2))	
		rownames(out) <- c("initial", "maintenance")
	} else {
		yrs <- if (length(years)==1) 1:years else years
		exal <- sapply(exal, \(i) i + yrs * decay + acidification)
		exal <- t(pmin(t(exal), y$exch_ac))
		out <- data.frame(yrs, exal)
	}
	colnames(out) <- nms
	out
}
