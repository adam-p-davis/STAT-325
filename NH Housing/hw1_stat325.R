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
#    x Any sale dates and sale prices (say, up to the 5 most recent),
#      along with the name of the owner on that same line.
#      * you decide how to deal with this information *
#    x Year built
#      'yearbuilt'
#    x Living area (this is in square feet)
#      'sqft'
#    x Replacement cost
#      'replcost'
#    x Building percent good
#      'pctgood'
#    x Style, model, grade, occupancy, AC Type, bedrooms, bathrooms,
#      half baths, bath style, kitchen style
#      'style', 'model', 'grade', 'occupancy', 'actype', 'bedrooms',
#      'bathrooms, 'halfbaths', 'bathstyle', 'kstyle'
#    x The sum of the value of any extra features
#      'exval'
#    x Land size (in acres)
#      'acres'
#    x Land use zone (like RS1, RS2, ...)
#      'zone'
#    x Neighborhood (like 0200, which should not be a number)
#      'neighborhood'
#    x Appraised value of the land
#      'landval'
#    x Gross area of anything that seems like a 'garage'
#      'garagesqft'

# NEW 9/13/16
#    x A logical (TRUE/FALSE) called 'multibuilding'.  See the post
#      on Piazza re: detecting this.  Don't spend time doing anything
#      special or fancy for any of the values of such a property.  Not
#      worth the effort for us.
#    x A logical 'nineplus' indicating the cases where bedrooms was "9+",
#      and then the value of 'bedrooms' should be the numeric value 9.
#    x There may be more than one "Grade" rows in some of the files.  When
#      this happens, use the first one.
#
# 9/13/16: Past sales!  We want up to 5 recent past sales.  Buyer,
#          sale date, and price.  Let's make 15 columns,
#          'buyer1', 'date1', 'price1', 'buyer2', ..., 'price5'
#
# 9/13/16: EVERYONE needs to do their block of geocoding, uploading the
#          resulting file to Dropbox.com before 8 AM on Thursday.  This
#          is done individually in a way tied to your NETID so we can get
#          the whole city covered.


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


sample_size <- 500
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
    return(gsub("[^0-9+.]", "", beds))
  }, error = function(e){return(NA)})
}

beds_list <- lapply(property_data, function(x)find_beds(x))

properties <- within(properties, {
  nineplus <- as.logical(unlist(lapply(lapply(beds_list, function(x)grepl("+",x, fixed = TRUE)), sum)))
})

beds_list <- lapply(beds_list, function(x)as.numeric(gsub("[^0-9.]","", x)))

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
#l1 <- unlist(lapply(property_data, function(x)sum(grepl("Total Bedrooms:", x))))
#l2 <- unlist(lapply(property_data, function(x)sum(grepl("Ttl Bedrms:", x))))
#l3 <- unlist(lapply(property_data, function(x)sum(grepl("Total Bedrms", x))))

#l4 <- cbind(l1, l2, l3)

#identical(sample_[which(!rowSums(l4))], 
#          sample_[which(unlist(lapply(property_data, is.null)))])

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
    return(as.numeric(gsub("[^0-9.]", "", baths)))
  }, error = function(e){return(NA)})
}

# (B)(a)*(thr)(oo)*(m)

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
# l1 <- unlist(lapply(property_data, function(x)sum(grepl("Total Bthrms:", x))))
# l2 <- unlist(lapply(property_data, function(x)sum(grepl("Ttl Bathrms:", x))))
# l3 <- unlist(lapply(property_data, function(x)sum(grepl("Total Baths", x))))
# 
# l4 <- cbind(l1, l2, l3)
# 
# identical(sample_[which(!rowSums(l4))], 
#           sample_[which(unlist(lapply(property_data, is.null)))])

properties$bathrooms <- bath_2

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

# The Total Baths template includes no Half bath extension
# l1 <- unlist(lapply(property_data, function(x)sum(grepl("Total Half Baths:", x))))
# l2 <- unlist(lapply(property_data, function(x)sum(grepl("Ttl Half Bths:", x))))
# l3 <- unlist(lapply(property_data, function(x)sum(grepl("Total Baths", x))))
# 
# l4 <- cbind(l1, l2, l3)
# 
# bath_2[l3 != 0]
# 
# identical(sample_[which(!rowSums(l4))], 
#           sample_[which(unlist(lapply(property_data, is.null)))])


properties$halfbaths <- half_2

# properties$halfbaths[which(properties$bathrooms %% 1 == .5)] <- properties$halfbaths[which(properties$bathrooms %% 1 == .5)] + 1
# properties$bathrooms[which(properties$bathrooms %% 1 == .5)] <- properties$bathrooms[which(properties$bathrooms %% 1 == .5)] - .5


## Owner address
find_address <- function(x){
  tryCatch({
    if(is.null(x))return(NA)
    address <- gsub("\t", "", x[grep("MainContent_lblAddr1", x)], fixed = TRUE)
    address <- gsub("Address", "", address)
    address <- gsub("<br>", ", ", address)
    address <- gsub("<[^<>]*>", " ", address)
    address <- gsub("\\s+", " ", address)
    return(gsub("^\\s+|\\s+$","",address))
  }, error = function(e){return(NA)})
}

address_list <- lapply(property_data, function(x)find_address(x))
properties <- within(properties, {
  address <- unlist(address_list)
})

properties$address

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

### Back to TYPE 2 stuff (baths, half, etc.)
# Style, model, grade, occupancy, AC Type, bedrooms, bathrooms,
#      half baths, bath style, kitchen style
#      'style' (X), 'model' (X), 'grade' (X), 'occupancy' (X), 'actype' (X), 'bedrooms' (X),
#      'bathrooms' (X), 'halfbaths' (X), 'bathstyle' (X), 'kstyle' (X)




find_style <- function(x){
  tryCatch({
    if(is.null(x))return(NA)
    if(sum(grepl("STYLE", x))){
      styles <- gsub("STYLE", "", gsub("\t", "", x[grep("STYLE", x)], fixed = TRUE))
    } else if(sum(grepl("<td>Style</td>", x, fixed = TRUE))){
      styles <- gsub("Style", "", gsub("\t", "", x[grep("<td>Style</td>", x)], fixed = TRUE))
    }
    # } else if(sum(grepl("Total Baths", x))){
    #   styles <- gsub("\t", "", x[grep("Total Baths", x)], fixed = TRUE)
    # }
    styles <- gsub("<[^<>]*>", "", styles)
    styles <- gsub("^\\s+|\\s+$", "", styles)
    return(styles)
  }, error = function(e){return(NA)})
}



style_list <- lapply(property_data, function(x)find_style(x))
style_list

style_2 <- rep(0, length(style_list))
for(i in 1:length(style_list)){
  if(length(na.omit(style_list[[i]])) == 0){
    style_2[i] <- NA
  } else {
    style_2[i] <- paste(unique(na.omit(style_list[[i]])), collapse = ', ')
  }
}

style_2

# The only ones that dont have these should be NULL
# l1 <- unlist(lapply(property_data, function(x)sum(grepl("STYLE", x))))
# l2 <- unlist(lapply(property_data, function(x)sum(grepl("<td>Style</td>", x))))
# l3 <- unlist(lapply(property_data, function(x)sum(grepl("Total Baths", x))))
# 
# l4 <- cbind(l1, l2)#, l3)
# 
# identical(sample_[which(!rowSums(l4))], 
#           sample_[which(unlist(lapply(property_data, is.null)))])

properties <- within(properties, {
  style <- style_2
})

## more type 2
key_ <- list('model', 'grade', 'occupancy', 'actype', 'bathstyle', 'kstyle')

search <- list(
  model = c("MODEL", "Model"),
  grade = c("Grade:", "Grade"),
  occupancy = c("Occupancy", "Occupancy"),
  actype = c("AC Type:", "AC Type"),
  bathstyle = c("Bath Style:", "Baths/Plumbing"),
  kstyle = c("Kitchen Style:", "Kitchen Style:")
)
reg <- list(
  model = list(
    c("<[^<>]*>", ""),c("^\\s+|\\s+$", "")
  ),
  grade = list(
    c("<[^<>]*>", ""),c("^\\s+|\\s+$", "")
  ),
  occupancy = list(
    c("<[^<>]*>", ""),c("^\\s+|\\s+$", "")
  ),
  actype = list(
    c("<[^<>]*>", ""),c("^\\s+|\\s+$", "")
  ),
  bathstyle = list(
    c("<[^<>]*>", ""),c("^\\s+|\\s+$", "")
  ),
  kstyle = list(
    c("<[^<>]*>", ""),c("^\\s+|\\s+$", "")
  )
)

type_2 <- function(x, key_, search, reg){
  result <- list()  
  for(i in 1:length(key_)){
      result[[key_[[i]]]] <- lapply(x, function(y, key = key_, searches = search, regex = reg){
        tryCatch({
          if(is.null(y))return(NA)
          if(sum(grepl(searches[[key[[i]]]][1], y))){
            keys <- gsub(searches[[key[[i]]]][1], "", gsub("\t", "", y[grep(searches[[key[[i]]]][1], y)], fixed = TRUE))
          } else if(sum(grepl(searches[[key[[i]]]][2], y, fixed = TRUE))){
            keys <- gsub(searches[[key[[i]]]][2], "", gsub("\t", "", y[grep(searches[[key[[i]]]][2], y)], fixed = TRUE))
          }
          keys <- gsub(regex[[key[[i]]]][[1]][1], regex[[key[[i]]]][[1]][2], keys)
          keys <- gsub(regex[[key[[i]]]][[2]][1], regex[[key[[i]]]][[2]][2], keys)
          keys <- gsub("^$|^\\s+$", NA, keys)
          return(keys)
        }, error = function(e){return(NA)})
      })
  }
  return(result)
}

two <- type_2(property_data, key_, search, reg)
# Still have to do some amount of manual processing
# model
model_2 <- rep(0, length(two$model))
for(i in 1:length(two$model)){
  if(length(na.omit(two$model[[i]])) == 0){
    model_2[i] <- NA
  } else {
    model_2[i] <- paste(unique(na.omit(two$model[[i]])), collapse = ', ')
  }
}

properties <- within(properties, {
  model <- model_2
})

# grade
grade_2 <- rep(0, length(two$grade))
for(i in 1:length(two$grade)){
  if(length(na.omit(two$grade[[i]])) == 0){
    grade_2[i] <- NA
  } else {
    grade_2[i] <- paste(unique(na.omit(two$grade[[i]])), collapse = ', ')
  }
}

properties <- within(properties, {
  grade <- grade_2
})

# occupancy
two$occupancy
occupancy_2 <- rep(0, length(two$occupancy))
for(i in 1:length(two$occupancy)){
  if(length(na.omit(two$occupancy[[i]])) == 0){
    occupancy_2[i] <- NA
  } else {
    occupancy_2[i] <- sum(na.omit(as.numeric(two$occupancy[[i]])))
  }
}

properties <- within(properties, {
  occupancy <- occupancy_2
})

# actype
actype_2 <- rep(0, length(two$actype))
for(i in 1:length(two$actype)){
  if(length(na.omit(two$actype[[i]])) == 0){
    actype_2[i] <- NA
  } else {
    actype_2[i] <- paste(unique(na.omit(two$actype[[i]])), collapse = ', ')
  }
}

properties <- within(properties, {
  actype <- actype_2
})

# bathstyle
two$bathstyle <- lapply(two$bathstyle, function(x)gsub("model", NA, x, fixed = TRUE))
two$bathstyle <- lapply(two$bathstyle, function(x)gsub("&nbsp;", NA, x, fixed = TRUE))
bathstyle_2 <- rep(0, length(two$bathstyle))
for(i in 1:length(two$bathstyle)){
  if(length(na.omit(two$bathstyle[[i]])) == 0){
    bathstyle_2[i] <- NA
  } else {
    bathstyle_2[i] <- paste(unique(na.omit(two$bathstyle[[i]])), collapse = ', ')
  }
}

properties <- within(properties, {
  bathstyle <- bathstyle_2
})
properties$bathstyle

# kstyle
two$kstyle <- lapply(two$kstyle, function(x)gsub("model", NA, x, fixed = TRUE))
two$kstyle <- lapply(two$kstyle, function(x)gsub("&nbsp;", NA, x, fixed = TRUE))
kstyle_2 <- rep(0, length(two$kstyle))
for(i in 1:length(two$kstyle)){
  if(length(na.omit(two$kstyle[[i]])) == 0){
    kstyle_2[i] <- NA
  } else {
    kstyle_2[i] <- paste(unique(na.omit(two$kstyle[[i]])), collapse = ', ')
  }
}

properties <- within(properties, {
  kstyle <- kstyle_2
})

## Type two are done woo

# The only ones that dont have these should be NULL
l1 <- unlist(lapply(property_data, function(x)sum(grepl("Bath Style:", x))))
l2 <- unlist(lapply(property_data, function(x)sum(grepl("Baths/Plumbing", x))))
l3 <- unlist(lapply(property_data, function(x)sum(grepl("Total Baths", x))))

l4 <- cbind(l1, l2)#, l3)

identical(sample_[which(!rowSums(l4))], 
          sample_[which(unlist(lapply(property_data, is.null)))])

# Type two checking routine ^

## Multi building
multi_list <- lapply(property_data, function(x){
  tryCatch({
    if(is.null(x))return(NA)
    multi <- gsub("\t", "", x[grep("MainContent_lblBldCount", x)])
    multi <- gsub("<[^<>]*>", "", multi)
    multi <- gsub("^\\s+|\\s+$", "", multi)
    return(as.numeric(multi))
    }, error = function(e){return(NA)})
  })

properties <- within(properties, {
  multibuilding <- unlist(multi_list) > 1
})

# exval
features_list <- lapply(property_data, function(x){
  tryCatch({
    if(is.null(x))return(NA)
    start <- "MainContent_panXF"
    end <- "</table>"
    features <- gsub("\t", "", x[grep(start, x):
                                   (grep(start, x) + 
                                      grep(end, x[grep(start, x):length(x)])[1])])
    # features <- gsub("<[^<>]*>", "", features)
    # features <- gsub("^\\s+|\\s+$", "", features)
    return(features)
  }, error = function(e){return(NA)})
})

features_list <- lapply(features_list, function(x)gsub("^\\s+|\\s+$", "", x))
features_list_2 <- lapply(features_list, function(x){
  tryCatch({
    if(sum(is.na(x)) == length(x))return(NA)
    if(sum(grepl("No Data for Extra Features", x)))return(NA)
    val_lines <- x[grep("$", x, fixed = TRUE)]
    val_lines <- gsub("<[^<>]*>", ";", val_lines)
    val_lines <- gsub(";+", ";", val_lines)
    val_lines <- gsub("^;|;$", "", val_lines)
    val_lines <- strsplit(val_lines, ";")
    val_lines <- lapply(val_lines, function(x){
      return(as.numeric(gsub("[$,]", "", x[grep("$", x, fixed = TRUE)])))
    })
    return(sum(unlist(val_lines)))
    #return(sum(na.omit(as.numeric(gsub("<[^<>]*>", "", x[grep("$", x, fixed = TRUE)])))))
    
  }, error = function(e){return(NA)})
})

properties <- within(properties, {
  exval <- unlist(features_list_2)
})

## garagesqft
garages_list <- lapply(property_data, function(x){
  tryCatch({
    if(is.null(x))return(NA)
    start <- "MainContent_ctl01_grdSub"
    end <- "</table>"
    subs <- gsub("\t", "", x[grep(start, x):
                                   (grep(start, x) + 
                                      grep(end, x[grep(start, x):length(x)])[1])], fixed = TRUE)
    return(subs)
  }, error = function(e){return(NA)})
})

garages_list <- lapply(garages_list, function(x)gsub("^\\s+|\\s+$", "", x))
garages_list_2 <- lapply(garages_list, function(x){
  tryCatch({
    if(sum(is.na(x)) == length(x))return(NA)
    val_lines <- x[grep("Garage", x, fixed = TRUE)+1]
    val_lines <- gsub("[^0-9.]", "", val_lines)
    if(identical(val_lines, character(0))){
      val_lines <- NA
    }
    return(sum(as.numeric(val_lines)))
  }, error = function(e){return(NA)})
})

properties <- within(properties, {
  garagesqft <- unlist(garages_list_2)
})

## recent sales
sales_list <- lapply(property_data, function(x){
    tryCatch({
      if(is.null(x))return(NA)
      start <- "MainContent_grdSales"
      end <- "</table>"
      sales <- gsub("\t", "", x[grep(start, x):
                                 (grep(start, x) + 
                                    grep(end, x[grep(start, x):length(x)])[1])], fixed = TRUE)
      return(sales)
    }, error = function(e){return(NA)})
})

sales_list <- lapply(sales_list, function(x)gsub("^\\s+|\\s+$", "", x))
sales_list_2 <- lapply(sales_list, function(x){
  tryCatch({
    if(sum(is.na(x)) == length(x))return(NA)
    val_lines <- x[(grep("<th scope=\"col\">Owner", x) + 2):length(x)]
    val_lines <- val_lines[1:grep("</table>", val_lines, fixed = TRUE)[1]]
    val_lines <- val_lines[2*seq(1:sum(grepl("<td>", val_lines))) - 1]
    val_lines <- gsub("\t", "", val_lines)
    val_lines <- gsub("[,$]", "", val_lines)
    val_lines <- gsub("<[^<>]*>", ",", val_lines)
    val_lines <- gsub("^,|,$", "", val_lines)
    val_lines <- lapply(val_lines, function(x)unlist(strsplit(x, ",")))
    if(identical(val_lines, character(0))){
      val_lines <- NA
    }
    return(val_lines)
  }, error = function(e){return(NA)})
})
sales_list_2
# 1, 3, 11

properties <- within(properties, {
  for(i in 1:5){
    assign(paste0("buyer", i), 
           unlist(lapply(sales_list_2, function(x, i_ = i){
             tryCatch({
               x[[i_]][1]
             }, error = function(e){return(NA)})
           })))
    assign(paste0("price", i), 
           unlist(lapply(sales_list_2, function(x, i_ = i){
             tryCatch({
               return(as.numeric(x[[i_]][3]))
             }, error = function(e){return(NA)})
           })))
    assign(paste0("date", i), 
           unlist(lapply(sales_list_2, function(x, i_ = i){
             tryCatch({
               x[[i_]][11]
             }, error = function(e){return(NA)})
           })))
  }
  i <- NULL
})

head(properties)

colSums(na.omit(properties == "&nbsp;"))


write.csv(properties, "325_apd29.csv", row.names = FALSE)

### COMPARE.R
#
# Statistical Clinic, Sept 16, 2016
#
# 

files <- dir("datafiles", full.names = TRUE)
all <- lapply(files, read.csv, as.is=TRUE)

sapply(all, dim)
all <- all[-5]       # Oops.

#lapply(all, names)

thisvar <- "acres"      # Change this, run the following:


x <- as.data.frame(sapply(student_csv, function(a) a[,thisvar]),
                   stringsAsFactors=FALSE)
table(nums <- apply(x, 1, function(a) length(unique(a))))

for (i in 2:ncol(x)) {
  if (any(nums==i)) {
    cat("\n\n----------------------", i, "\n")
    temp <- x[nums==i,]
    cat("# times there were", i, "unique values:",
        nrow(temp), "\n")
    print(temp[sample(1:nrow(temp), min(10,nrow(temp))),])
    
  }
}

files_csv <- as.list(dir("Sept15NH_325"))

student_csv <- lapply(files_csv, function(x)read.csv(paste0("Sept15NH_325/", x), as.is=TRUE))
student_csv <- student_csv[-5]
names(student_csv[[5]])[40] <- 'multibuilding'

list_of_names <- as.list(sapply(student_csv, names)[[2]])

x_list <- lapply(list_of_names[11:31], function(thisvar){
  cat("#####", thisvar, "####")
  x <- as.data.frame(sapply(student_csv, function(a) a[,thisvar]),
                     stringsAsFactors=FALSE)
  table(nums <- apply(x, 1, function(a) length(unique(a))))
  
  for (i in 2:ncol(x)) {
    if (any(nums==i)) {
      cat("\n\n----------------------", i, "\n")
      temp <- x[nums==i,]
      cat("# times there were", i, "unique values:",
          nrow(temp), "\n")
      print(temp[sample(1:nrow(temp), min(10,nrow(temp))),])
      
    }
  }
})

