# required libraries
library(lattice)
library(lubridate)
library(sf)

# define local functions
mvavg <- function(tab, njours) {
    out <- tab
    for (j in njours:ncol(tab)) {
        jj <- (j - njours + 1):j
        out[, j] <- apply(tab[, jj, drop = FALSE], 1, mean, na.rm = TRUE)
    }
    out <- out[, -(1:(njours - 1))]
    return(out)    
}

# define parameters
iskon <- "2021-11-24"

# read the latest file for metropolitan France
dat <- readRDS(file = "data/sidep.rds")

# prepare working data
dat2 <- subset(dat, date > iskon)

# compute population per department
popdep <- aggregate(pop ~ dep, data = dat2, FUN = max)
popinv <- diag(10^6 / popdep$pop)
dimnames(popinv) <- list(popdep$dep, popdep$dep)

# tabulate positives tests per department / date
tabpos1 <- tapply(dat2$P, list(dat2$dep, dat2$date), FUN = min)
# compute 7-day average (and drop first 6 days)
tabpos2 <- mvavg(tabpos1, njours = 7)
# compute values per million
tabpos3 <- popinv %*% tabpos2
tabpos4 <- popinv %*% tabpos1[, -(1:6)]

# build the data.frame
dat3 <- data.frame(dep = rep(row.names(tabpos3), each = ncol(tabpos3)),
           date = as.Date(rep(colnames(tabpos3), nrow(tabpos3))),
           cpm1 = as.numeric(t(tabpos4)),
           cpm7 = as.numeric(t(tabpos3)))

# additional variables
dat3[, "zero"] <- 0
dat3[, "dow"] <- lubridate::wday(dat3$date, week_start = 1)

# departement full name
departements <- readRDS(file = "data/departements.rds")
idx <- match(dat3$dep, departements$code)
dat3$dept <- departements$dep[idx]

# read the map
france <- readRDS(file = "data/map_france.rds")

# read the latest file of 7-day smoothed data for metropolitan France
dat7 <- readRDS(file = "data/sidep7.rds")

# convert to wide format
tab7 <- reshape(dat7[, c("dep", "date", "cpm7")], idvar = "dep", timevar = "date", direction = "wide")
row.names(tab7) <- NULL

# merge with map
france7 <- merge(france, tab7, by.x = "code", by.y = "dep", all = TRUE)

# write the files
saveRDS(dat3, "data/sidep7.rds")
saveRDS(tab7, "data/tab7.rds")
saveRDS(france7, "data/france7.rds")
