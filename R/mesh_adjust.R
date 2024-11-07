#https://extension.psu.edu/media/wysiwyg//extensions/catalog_product/6defa07cdbee4061809612d041f2c89c/e/f/effect-of-aglime-fineness-on-speed-of-reaction.jpg


.litas_inverse <- function(lime, exch_ac, ECEC) {
# estimate exch_ac for a lime application
	e <- exch_ac - 0.6 * lime
    0.03775 + 0.07448*exch_ac -0.03546 * ECEC + 1.18901*e + 0.07343*exch_ac*e + -0.14587*e*e
}

# Aramburu Merlos et al. xxx
.lr_litas <- function(exch_ac, ECEC, TAS, a = 0.6, b = 0.2){
  tas <- TAS/100
  lf <- 1/(a + tas * (b-a))
  pmax(0, lf * (exch_ac - tas * ECEC))
}


.mesh_effect <- function(mesh) {
# relative effect of mesh size on (timing of) peak change in exch_ac
# for mesh=100, rel_effect is 1, and peak is 6 at months after application
	rel_effect <- pmax(0, -0.3096 + 0.02243 * mesh + -0.00009334 * mesh^2)
	peak <- (60 -1.064*mesh + 0.00524 * mesh^2) / 12
	cbind(rel_effect, peak)
}


.reaction_time <- function(lime, mesh, exch_ac, ECEC, decay=0.25) {
# exch_ac over time, given a lime application and mesh_size
	m <- .mesh_effect(mesh)
	exac100 <- .litas_inverse(lime, exch_ac, ECEC)
	exac <- exch_ac - (exch_ac - exac100) * m[,1]
	remaining <- (1-m[,1]) * lime
	yrs <- remaining / decay
	if (yrs > 0.5) {
		ryrs <- round(yrs)
		yrate <- (remaining/yrs) * seq(0.9, 0.3, length.out=ryrs)
		yrate <- decay - yrate
		plateau <- pmin(exch_ac, exac + cumsum(yrate))
		dec <- (exch_ac - max(plateau)) / decay
		data.frame(years=c(0, m[,2], m[,2]+(1:ryrs), m[,2]+ryrs+dec),
				exch_ac=c(exch_ac, exac, plateau, exch_ac))
	} else {
		dec <- (exch_ac - exac) / decay
		data.frame(years=c(0, m[,2], m[,2]+dec),
				exch_ac=c(exch_ac, exac, exch_ac))
	}
}


#plot(reaction_time(4, 100, 3, 6), type="b", xlim=c(0,13), ylim=c(0.5,3))
#lines(reaction_time(4, 75, 3, 6), type="b", col="red")
#lines(reaction_time(4, 50, 3, 6), type="b", col="blue")
#lines(reaction_time(4, 25, 3, 6), type="b", col="green")

#plot(reaction_time(4, 100, 3, 6), type="b", xlim=c(0,20), ylim=c(0.8,3))
#lines(reaction_time(4.5, 75, 3, 6), type="b", col="red")
#lines(reaction_time(5.8, 50, 3, 6), type="b", col="blue")
#lines(reaction_time(11.5, 25, 3, 6), type="b", col="green")
