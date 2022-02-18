# required libraries
library(httr)

# SIDEP file permalink 
src <- "https://www.data.gouv.fr/fr/datasets/r/406c6a23-e283-4300-9484-54e78c8ae675" 

# get the real file name
resp <- httr::GET(src)
csv <- paste0("data/", basename(resp$url))

# download the latest file and read the data
download.file(url = src, destfile = csv, mode = "wb")
dat <- read.csv(csv, fileEncoding = "UTF-8", sep = ";", stringsAsFactors = FALSE)
saveRDS(dat, file = "data/sidep_all.rds")

# only metropolitan France
dat <- subset(dat, nchar(as.character(dep)) == 2)
# all ages class 
dat <- subset(dat, cl_age90 == 0)
# add date-type 'date' column
dat$date <- as.Date(dat$jour)
saveRDS(dat, file = "data/sidep.rds")

