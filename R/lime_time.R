
.lime_time <- function(nyears=15, exch_ac, ECEC, TAS, acidification=0, decay=0.22, rates=FALSE) {
	y <- data.frame(exch_ac=exch_ac, ECEC=ECEC, TAS=TAS)
	rate <- limeRate(y, method="my", check_Ca=FALSE, TAS=y$TAS)
	exal <- y$ECEC * y$TAS / 100 
	yrs <- 0:nyears
	if (rates) {
		y$exch_ac = exal + decay + acidification
		rate2 <- limeRate(y, method="my", check_Ca=FALSE, TAS=y$TAS)
		out <- data.frame(year=c(0, 1), rbind(rate, rate2))	
	} else {
		exal <- sapply(exal, \(i) i + yrs * decay + acidification)
		out <- data.frame(year=yrs, pmin(exal, y$exch_ac))
	}
	colnames(out) <- c("year", apply(y, 1, \(i) paste(i, collapse="_")))
	out
}
