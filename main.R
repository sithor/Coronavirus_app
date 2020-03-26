library(magrittr)
library(RCurl)
library(stringr)
library(dplyr)

source("functions.R")

file_path <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/0841024637ac29f2d629af1ff9314f1fa7002dc4/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"
# file_path_death <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv"
 CV <- read.csv(file_path, na.strings = "", stringsAsFactors = FALSE)
 CV %>% head
 CVT <- t(CV)
 CVT %>% head
CVT %<>% data.frame
CVT[] <- lapply(CVT, as.character)
names(CVT) <- CVT[1,]

## country names for no province
names(CVT)[ which(is.na(names(CVT)))] <- CVT[ 2 , which(is.na(names(CVT)))] 

CVT <- CVT[ -c(1:4), ]

# ######### fill missings with zeros -------
CVT[is.na(CVT)] <- 0

CVT$date <- row.names(CVT)
CVT$date <- str_replace_all(CVT$date, "X1", "Jan")
CVT$date <- str_replace_all(CVT$date, "X2", "Feb")
CVT$date <- str_replace_all(CVT$date, "X3", "Mar")
CVT$date <- str_replace_all(CVT$date, "X4", "Apr")
CVT$date_R <- strptime(CVT$date, "%b.%d.%y") %>% as.POSIXct
CVT$date_day <- as.Date(CVT$date_R,  origin = "1970-01-01")
CVT <- CVT[!duplicated(CVT$date_day), ]

# ## Calculate new cases per day
IVT <- CVT
rm(CVT)
IVT[, 1:(ncol(IVT)-3)] <- lapply(IVT[, 1:(ncol(IVT) - 3)], as.numeric)
 for (i in 1:(ncol(IVT)-3)) {
 for (j in (2:(nrow(IVT)))) {
   IVT[j, paste0(names(IVT)[i], "_incident_case")] <- IVT[j, i ] - IVT[j - 1, i ]
   IVT[j, paste0(names(IVT)[i], "_incident_case")] <- ifelse(IVT[j, paste0(names(IVT)[i], "_incident_case")] < 0, 0,
                                                             IVT[j, paste0(names(IVT)[i], "_incident_case")])
                              }}

x <- sort(colMeans(IVT[c((nrow(IVT) -5):nrow(IVT)), 
                       (grepl("_incident_case", names(IVT))) %>% which]), decreasing = TRUE)

names(x) <- sub("_incident_case", "", names(x))

## Convert missing to zeros.

IVT[is.na(IVT)] <- 0

## drop countries or cities with no cases
for (i in 1:length(x)) x[i] <- ifelse(sum(IVT[, names(x)[i], drop = FALSE]) == 0,
                                       FALSE, TRUE)

x <- x[x == 1]  