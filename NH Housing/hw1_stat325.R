## 09/05/16
## ADAM DAVIS - STAT 325 - HOMEWORK #1
## No collaboration

#
# STAT 325/625: Fall 2016
#
# You can work alone.  We'd recommend forming small teams of 2-3.
# We want to keep 325 and 625 groups independent (because independence
# is a good thing), so please respect this boundary as you team up.
# We'll try to facilitate via Piazza.
#
# Goal: a CSV file, basically building on what we currently have with 6
# columns, one per team.  Be ready for validation on Thursday, Sept 15,
# at 9 AM.  Choose a "team leader" to upload the file to his/her dropbox,
# with file name like 325_netid1_netid2_netid3.csv or 625_netid1_netid2.csv.
#
# (Note: At first glance, the number of variables we are asking you to 
#        scrape will seem overwhelming. Upon closer inspection, you will
#        see that many of the variables can be processed using the
#        same exact approach.)
# 
# New Haven Housing!  Ultimately, we want the following.
#    We should all use the same variable names for coding efficiency:
#
#    x Location and appraised value (from HW1)
#      'pid', 'location', 'totval'
#    x Owner address (may be useful for zip code, used with caution)
#      'address'
#    1 Any sale dates and sale prices (say, up to the 5 most recent),
#      along with the name of the owner on that same line.
#      * you decide how to deal with this information *
#    0 Year built
#      'yearbuilt'
#    0 Living area (this is in square feet)
#      'sqft'
#    0 Replacement cost
#      'replcost'
#    0 Building percent good
#      'pctgood'
#    2 Style, model, grade, occupancy, AC Type, bedrooms, bathrooms,
#      half baths, bath style, kitchen style
#      'style', 'model', 'grade', 'occupancy', 'actype', 'bedrooms',
#      'bathrooms, 'halfbaths', 'bathstyle', 'kstyle'
#    3 The sum of the value of any extra features
#      'exval'
#    0 Land size (in acres)
#      'acres'
#    0 Land use zone (like RS1, RS2, ...)
#      'zone'
#    0 Neighborhood (like 0200, which should not be a number)
#      'neighborhood'
#    0 Appraised value of the land
#      'landval'
#    4 Gross area of anything that seems like a 'garage'
#      'garagesqft'


# Local file location
setwd("/Users/adamdavis/Documents/Documents/AdamDavis/College/4. Senior/Fall/STAT 325 - Statistical Case Studies/Week 1/VisionAppraisal/newdata2016")
setwd("~/Documents/STAT 325/VisionAppraisal/newdata2016/")
dir() 

# Number of files
n <- 27307

# List of potential file names
files <- as.list(paste0(seq(1,n,1), ".html"))

# Create data frame
properties <- data.frame("pid" = seq(1,n,1), 
                         "location" = character(n), 
                         "totval" = numeric(n),
                         "bedrooms" = numeric(n),
                         "bathrooms" = numeric(n),
                         "halfbaths" = numeric(n))

# Build a list of all files read in, equal to NULL if the file does not exist or.
property_data <- lapply(files, function(x)tryCatch({scan(x, what="", sep="\n")}, 
                                                   error = function(e){return(NULL)}))


sample_size <- 1000
sample_ <- sample(1:n, sample_size)
files_sample <- as.list(paste0(sample_, ".html"))
property_data <- lapply(files_sample, function(x)tryCatch({scan(x, what="", sep="\n")}, 
                                                          error = function(e){return(NULL)}))
properties <- data.frame("pid" = sample_, 
                         "location" = character(sample_size), 
                         "totval" = numeric(sample_size),
                         "bedrooms" = numeric(sample_size),
                         "bathrooms" = numeric(sample_size),
                         "halfbaths" = numeric(sample_size))


# Location description function
find_loc <- function(x){
  tryCatch({
   if(is.null(x))return(NA)
    # Identifying tag
    loc <- gsub("<[^<>]*>", "", x[grep("id=\"MainContent_lblLocation\"", x)])
    loc <- gsub("^\\s+|\\s+$", "", loc)
    return(loc)
  }, error = function(e){return(NA)})
}

# Determine location
loc_list <- lapply(property_data, function(x)find_loc(x))
head(unlist(loc_list)) 

# Assign character vector to data frame
properties$location <- unlist(loc_list)

# Val description function
find_val <- function(x){
  tryCatch({
    if(is.null(x))return(NA)
    # Identify html tag for the current value table, but advancing a couple of lines to the
    # actual values inserted in the table
    val <- gsub("\t", "", x[grep("id=\"MainContent_grdCurrentValueAppr\"", x)+6], fixed = TRUE)
    # Remove closing html code, adding a ";", as some of the values contain ","
    # so cannot use "," in strsplit
    val <- gsub("</[^<>]*>", ";", val)
    # Strip remaining html, $, and commas
    val <- gsub("<[^<>]*>", "", val)
    val <- gsub("[$,]", "", val)
    # Organize as a single numeric vector
    val <- as.numeric(unlist(strsplit(val, ";")))
    # Check that the year is 2015, and return the appraisal value
    if(val[1] == 2015){
      return(val[4])
    } else {
      return(NA)
    }
  }, error = function(e){return(NA)})
}

# Determine totval
val_list <- lapply(property_data, function(x)find_val(x))

# Assign numeric vector to data frame
properties$totval <- unlist(val_list)

find_beds <- function(x){
  tryCatch({
    if(is.null(x))return(NA)
    if(sum(grepl("Total Bedrooms:", x))){
      beds <- gsub("\t", "", x[grep("Total Bedrooms:", x)], fixed = TRUE)
    } else if(sum(grepl("Ttl Bedrms:", x))){
      beds <- gsub("\t", "", x[grep("Ttl Bedrms:", x)], fixed = TRUE)
    } else if(sum(grepl("Total Bedrms", x))){
      beds <- gsub("\t", "", x[grep("Total Bedrms", x)], fixed = TRUE)
    }
    beds <- gsub("<[^<>]*>", "", beds)
    beds <- gsub("^\\s+|\\s+$", "", beds)
    return(as.numeric(gsub("[^0-9]", "", beds)))
  }, error = function(e){return(NA)})
}

beds_list <- lapply(property_data, function(x)find_beds(x))

# Multiple entries for some of the input -- we will sum them excluding NAs
beds_2 <- rep(0, length(beds_list))
for(i in 1:length(beds_list)){
  if(length(na.omit(beds_list[[i]])) == 0){
    beds_2[i] <- NA
  } else {
    beds_2[i] <- sum(na.omit(beds_list[[i]]))
  }
}

# The only ones that dont have these should be NULL
l1 <- unlist(lapply(property_data, function(x)sum(grepl("Total Bedrooms:", x))))
l2 <- unlist(lapply(property_data, function(x)sum(grepl("Ttl Bedrms:", x))))
l3 <- unlist(lapply(property_data, function(x)sum(grepl("Total Bedrms", x))))

l4 <- cbind(l1, l2, l3)

identical(sample_[which(!rowSums(l4))], 
          sample_[which(unlist(lapply(property_data, is.null)))])

beds_list[which(unlist(lapply(beds_list, length)) != 1)]
properties$bedrooms <- beds_2

find_bath <- function(x){
  tryCatch({
    if(is.null(x))return(NA)
    if(sum(grepl("Total Bthrms:", x))){
      baths <- gsub("\t", "", x[grep("Total Bthrms:", x)], fixed = TRUE)
    } else if(sum(grepl("Ttl Bathrms:", x))){
      baths <- gsub("\t", "", x[grep("Ttl Bathrms:", x)], fixed = TRUE)
    } else if(sum(grepl("Total Baths", x))){
      baths <- gsub("\t", "", x[grep("Total Baths", x)], fixed = TRUE)
    }
    baths <- gsub("<[^<>]*>", "", baths)
    baths <- gsub("^\\s+|\\s+$", "", baths)
    return(as.numeric(gsub("[^0-9.+]", "", baths)))
  }, error = function(e){return(NA)})
}

bath_list <- lapply(property_data, function(x)find_bath(x))


bath_2 <- rep(0, length(bath_list))
for(i in 1:length(bath_list)){
  if(length(na.omit(bath_list[[i]])) == 0){
    bath_2[i] <- NA
  } else {
    bath_2[i] <- sum(na.omit(bath_list[[i]]))
  }
}

# The only ones that dont have these should be NULL
l1 <- unlist(lapply(property_data, function(x)sum(grepl("Total Bthrms:", x))))
l2 <- unlist(lapply(property_data, function(x)sum(grepl("Ttl Bathrms:", x))))
l3 <- unlist(lapply(property_data, function(x)sum(grepl("Total Baths", x))))

l4 <- cbind(l1, l2, l3)

identical(sample_[which(!rowSums(l4))], 
          sample_[which(unlist(lapply(property_data, is.null)))])

properties$bathrooms <- bath_2
table(properties$bathrooms)


find_half <- function(x){
  tryCatch({
    if(is.null(x))return(NA)
    if(sum(grepl("Total Half Baths:", x))){
      halfs <- gsub("\t", "", x[grep("Total Half Baths:", x)], fixed = TRUE)
    } else if(sum(grepl("Ttl Half Bths:", x))){
      halfs <- gsub("\t", "", x[grep("Ttl Half Bths:", x)], fixed = TRUE)
    }
    halfs <- gsub("<[^<>]*>", "", halfs)
    halfs <- gsub("^\\s+|\\s+$", "", halfs)
    return(as.numeric(gsub("[^0-9.]", "", halfs)))
  }, error = function(e){return(NA)})
}

half_list <- lapply(property_data, function(x)find_half(x))
half_list[which(unlist(lapply(half_list, length)) != 1)]

half_2 <- rep(0, length(half_list))
for(i in 1:length(half_list)){
  if(length(na.omit(half_list[[i]])) == 0){
    half_2[i] <- NA
  } else {
  half_2[i] <- sum(na.omit(half_list[[i]]))
  }
}

l1 <- unlist(lapply(property_data, function(x)sum(grepl("Total Half Baths:", x))))
l2 <- unlist(lapply(property_data, function(x)sum(grepl("Ttl Half Bths:", x))))
l3 <- unlist(lapply(property_data, function(x)sum(grepl("Total Baths", x))))

l4 <- cbind(l1, l2, l3)

bath_2[l3 != 0]

sample_[which(!rowSums(l4))]
sample_[which(unlist(lapply(property_data, is.null)))]

identical(sample_[which(!rowSums(l4))], 
          sample_[which(unlist(lapply(property_data, is.null)))])


properties$halfbaths <- half_2

## Owner address
find_address <- function(x){
  tryCatch({
    if(is.null(x))return(NA)
    address <- gsub("\t", "", x[grep("MainContent_lblAddr1", x)], fixed = TRUE)
    address <- gsub("Address", "", address)
    address <- gsub("<[^<>]*>", " ", address)
    address <- gsub("\\s+", " ", address)
    return(gsub("^\\s+|\\s+$","",address))
  }, error = function(e){return(NA)})
}

address_list <- lapply(property_data, function(x)find_address(x))
properties <- within(properties, {
  address <- unlist(address_list)
})

# A few of these problems can be solved in the same way, referencing some
# MainContentlbl tag

main_content_vars <- list('yearbuilt', 'sqft', 'replcost', 'pctgood', 
                          'acres', 'zone', 'neighborhood', 'landval')

main_content_labs <- list('ctl01_lblYearBuilt', 'ctl01_lblBldArea',
                          'ctl01_lblRcn"', 'ctl01_lblPctGood', 'lblLndAcres',
                          'lblZone', 'lblNbhd', 'lblLndAppr')

main_content <- list()
for(i in 1:length(main_content_vars)){
  main_content[[main_content_vars[[i]]]] <- main_content_labs[[i]]
}

find_main_content <- function(prop, main_content){
  tryCatch({
    if(is.null(prop))return(NA)
    content_lines <- lapply(main_content_vars, function(x)return(prop[grep(main_content[[x]], prop)]))
    return(content_lines)
    }, error = function(e){return(NA)})
}

multi_prop_map <- function(x, main_content, main_content_vars){
  tryCatch({
    if(is.null(x))return(NA)
    lapply(main_content_vars, function(z){
      single_var_list <- lapply(x, function(y){
        return(y[grep(main_content[[z]], y)])
      })
      return(single_var_list)
    })}, error = function(e){return(NA)})
}

type_0_vars <- multi_prop_map(property_data, main_content, main_content_vars)
type_0_vars

type_0_vals <- lapply(type_0_vars, function(x){
  tryCatch({
    if(is.null(x))return(NA)
    temp <- gsub("<[^<>]*>", "", x)
    temp <- gsub("\t", "", temp, fixed = TRUE)
    temp <- gsub("[$,]", "", temp)
    text <- c("Size \\(Acres\\)", "Zone", "Neighborhood", "Appraised Value")
    for(i in text){
      temp <- gsub(i, "", temp)
    }
    temp <- gsub("^\\s+|\\s+$", "", temp)
    temp[temp == ""] <- NA
    temp[is.null(temp)] <- NA
    temp[temp == "NULL"] <- NA
    return(temp)
  }, error = function(e){return(NA)})
})

type_0_vals

numeric_class <- c(1:5, 8)
for(i in numeric_class){
  type_0_vals[[i]] <- as.numeric(unlist(type_0_vals[[i]]))
}

type_0_vals



properties <- within(properties, {
  for(i in 1:length(main_content_vars)){
    assign(main_content_vars[[i]], unlist(type_0_vals[[i]]))
  }
  i <- NULL
})

head(properties)


write.csv(properties, "sample_1.csv")
