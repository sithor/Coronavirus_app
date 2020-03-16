if(!require(pacman)) install.packages("pacman")

pacman::p_load(magrittr, epiDisplay, RCurl, stringr)

source("functions.R")

file_path <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv"
file_path_death <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv"

CV <- read.csv(file_path, na.strings = "", stringsAsFactors = FALSE)

CVT <- t(CV)


CVT %<>% data.frame
CVT[] <- lapply(CVT, as.character)
names(CVT) <- CVT[1,]


names(CVT)[ which(is.na(names(CVT)))] <- CVT[ 2 , which(is.na(names(CVT)))] ## country names for no province

CVT <- CVT[ -c(1:4), ]


######### fill missings with zeros
CVT[is.na(CVT)] <- 0

CVT$date <- row.names(CVT)

CVT$date <- str_replace_all(CVT$date, "X1", "Jan")

CVT$date <- str_replace_all(CVT$date, "X2", "Feb")

CVT$date_R <- strptime(CVT$date, "%b.%d.%y")

CVT$date_day <- as.Date(CVT$date_R)


CVT <- CVT[!duplicated(CVT$date_day), ]


########### Cumulative cases per day



## Calculate new cases per day
IVT <- CVT

names(IVT)

IVT[, 1:(ncol(IVT)-3)] <- lapply(IVT[, 1:(ncol(IVT) - 3)], as.numeric) 

for (i in 1:(ncol(IVT)-3)) {
for (j in (2:(nrow(IVT)))) { 
  IVT[j, paste0(names(IVT)[i], "_incident_case")] <- IVT[j, i ] - IVT[j - 1, i ]
  IVT[j, paste0(names(IVT)[i], "_incident_case")] <- ifelse(IVT[j, paste0(names(IVT)[i], "_incident_case")] <0, 0,
                                                            IVT[j, paste0(names(IVT)[i], "_incident_case")])
                        }
                        }
IVT[is.na(IVT)] <- 0

x <- sort(colMeans(IVT[c((nrow(IVT) -5):nrow(IVT)), (grepl("_incident_case", names(IVT))) %>% which]), decreasing = TRUE)

names(x) <- sub("_incident_case", "", names(x))

