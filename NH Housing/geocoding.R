#
# Geocoding via ggmap
#

netid <- "apd29"      # Change this.  Lower case letters!

##
## WARNING: This script will take about 15 minutes to run.
##

#####################################################
# Don't change anything below.  Run the entire script
# in some folder where you've put the script and the
# 625location.csv file.  The result will be a file
# like geocoded_netid.csv that has your range of
# properties geocoded.  Put this in your Dropbox.com
# folder.

set.seed(1)
n <- read.csv("http://www.stat.yale.edu/~jay/625/.hidden/netids.csv", as.is=TRUE)
n$myval <- sample(1:nrow(n)) %% 28 + 1
if (length(this <- which(n$User.ID==netid))==1) {
  myval <- n$myval[this]
} else stop("I don't believe your netid.")

starts <- c(1, seq(1000, 27000, by=1000))
ends <- c(starts[-1] - 1, 27307)

x <- read.csv("http://www.stat.yale.edu/~jay/625/.hidden/NHlocations.csv", as.is=TRUE)
x$lat <- NA
x$lon <- NA

if (!require(ggmap)) install.packages("ggmap")
library(ggmap)

# Examples:
#geocode("24 Hillhouse Ave, New Haven, CT")   # Definitely 2500/day
#geocode("24 Hillhouse Ave, New Haven, CT", source="dsk")  # ? limited

myrange <- starts[myval]:ends[myval]

locs <- paste(x$location[myrange], ", New Haven, CT", sep="")
ans <- geocode(locs)
x$lon[myrange] <- ans$lon
x$lat[myrange] <- ans$lat

write.csv(x, paste("geocoded_", netid, ".csv", sep=""),
          row.names=F)

print("DONE!  Take the geocoded_netid.csv file and upload to Dropbox.com.")

