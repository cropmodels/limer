kamp <- read.csv("inst/data-raw/kamprath_1970.csv")
save(kamp, file = "data/kamp.RData")
d <- kamp
rm(kamp)
data(kamp)
all.equal(d, kamp)
rm(d, kamp)



coch <- read.csv("inst/data-raw/cochrane_1980.csv")

save(coch, file = "data/coch.RData")
rm(list = ls())

data(coch)


linc <- read.csv("inst/data-raw/lime_incubation_studies.csv")
save(linc, file = "data/linc.RData")

data(linc)
