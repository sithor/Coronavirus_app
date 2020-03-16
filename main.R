pacman::p_load(magrittr, epiDisplay, RCurl, stringr)

rm(list = ls())

# Doesn't work at ARPHS

#x <- getURL("https://github.com/CSSEGISandData/COVID-19/blob/master/time_series/time_series_2019-ncov-Confirmed.csv")
source("functions.R")

file_path <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv"
file_path_death <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv"

CV <- read.csv(file_path, na.strings = "", stringsAsFactors = FALSE)

CV %>% head
CVT <- t(CV)
CVT %>% head

CVT %<>% data.frame
CVT[] <- lapply(CVT, as.character)
names(CVT) <- CVT[1,]

CVT %>% head

names(CVT)[ which(is.na(names(CVT)))] <- CVT[ 2 , which(is.na(names(CVT)))] ## country names for no province
names(CVT)

head(CVT)

CVT <- CVT[ -c(1:4), ]
CVT

######### fill missings with zeros
# CVT[is.na(CVT)] <- 0


row.names(CVT)

CVT$date <- row.names(CVT)
CVT$date
CVT$date %>% head

CVT$date <- str_replace_all(CVT$date, "X1", "Jan")
CVT$date <- str_replace_all(CVT$date, "X2", "Feb")

CVT$date 

CVT$date_R <- strptime(CVT$date, "%b.%d.%y")

CVT$date_R
CVT$date_day <- as.Date(CVT$date_R)

CVT$date_day

CVT <- CVT[!duplicated(CVT$date_day), ]


########### Cumulative cases per day

CVT %>% head

## Calculate new cases per day
IVT <- CVT
IVT %>% str
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
names(IVT)

str(IVT)

IVT %>% names

x <- sort(colMeans(IVT[c((nrow(IVT) -5):nrow(IVT)), (grepl("_incident_case", names(IVT))) %>% which]), decreasing = TRUE)

names(x) <- sub("_incident_case", "", names(x))

######## For function, see function file...

# 
# ##################################################################################################################
# 
# ######## look at deaths
# 
# file_path_death <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv"
# 
# CV <- read.csv(file_path_death, na.strings = "", stringsAsFactors = FALSE)
# 
# CV %>% head
# CVT <- t(CV)
# CVT %>% head
# 
# CVT %<>% data.frame
# CVT[] <- lapply(CVT, as.character)
# names(CVT) <- CVT[1,]
# 
# CVT %>% head
# 
# names(CVT)[ which(is.na(names(CVT)))] <- CVT[ 2 , which(is.na(names(CVT)))] ## country names for no province
# names(CVT)
# 
# head(CVT)
# 
# CVT <- CVT[ -c(1:4), ]
# CVT
# 
# ######### fill missings with zeros
# # CVT[is.na(CVT)] <- 0
# 
# 
# row.names(CVT)
# 
# CVT$date <- row.names(CVT)
# CVT$date
# CVT$date %>% head
# 
# CVT$date <- str_replace_all(CVT$date, "X1", "Jan")
# CVT$date <- str_replace_all(CVT$date, "X2", "Feb")
# 
# CVT$date 
# 
# CVT$date_R <- strptime(CVT$date, "%b.%d.%y")
# 
# CVT$date_R
# CVT$date_day <- as.Date(CVT$date_R)
# 
# CVT$date_day
# 
# CVT <- CVT[!duplicated(CVT$date_day), ]
# 
# 
# ########### Cumulative cases per day
# 
# CVT %>% head
# 
# ## Calculate new cases per day
# IVT <- CVT
# IVT %>% str
# names(IVT)
# 
# IVT[, 1:(ncol(IVT)-3)] <- lapply(IVT[, 1:(ncol(IVT) - 3)], as.numeric) 
# 
# for (i in 1:(ncol(IVT)-3)) {
# for (j in (2:(nrow(IVT)))) { 
#   IVT[j, paste0(names(IVT)[i], "_incident_case")] <- IVT[j, i ] - IVT[j - 1, i ]
#                         }
#                         }
# IVT[is.na(IVT)] <- 0Medical1
# Medical1
# 
# IVT
# 
# 
# 
# 
# plot(IVT$date_R, IVT$`Beijing_incident_case`, xlab = "Date", ylab = "Counts per day", 
#   main = paste0("Confirmed deaths in Beijing"))
# lines(lowess(IVT$date_R, IVT$`Beijing_incident_case`))
# 
# plot(IVT$date_R, IVT$`Shanghai_incident_case`, xlab = "Date", ylab = "Counts per day", 
#   main = "Confirmed deaths in Shanghai")
# lines(lowess(IVT$date_R, IVT$`Shanghai_incident_case`))
# 
# 
# plot(IVT$date_R, IVT$`Hubei_incident_case`, xlab = "Date", ylab = "Counts per day", 
#   main = "Confirmed deaths in Hubei")
# lines(lowess(IVT$date_R, IVT$`Hubei_incident_case`))
# 
# plot(IVT$date_R, IVT$`Singapore_incident_case`, xlab = "Date", ylab = "Counts per day", 
#   main = "Confirmed cases in Singapore")
# lines(lowess(IVT$date_R, IVT$`Singapore_incident_case`))
# 
# plot(IVT$date_R, IVT$`Diamond Princess cruise ship_incident_case`, xlab = "Date", ylab = "Counts per day", 
#   main = "Confirmed deaths onboard Diamond Princess cruise ship")
# lines(lowess(IVT$date_R, IVT$`Diamond Princess cruise ship_incident_case`))
# 
# plot(IVT$date_R, IVT$`Shandong_incident_case`, xlab = "Date", ylab = "Counts per day", 
#   main = "Confirmed deaths in Shandong")
# lines(lowess(IVT$date_R, IVT$`Shandong_incident_case`))
# 
# plot(IVT$date_R, IVT$`Anhui_incident_case`, xlab = "Date", ylab = "Counts per day", 
#   main = "Confirmed deaths in Anhui")
# lines(lowess(IVT$date_R, IVT$`Anhui_incident_case`))
# 
# 
# plot(IVT$date_R, IVT$`Zhejiang_incident_case`, xlab = "Date", ylab = "Counts per day", 
#   main = "Confirmed deaths in Zhejiang")
# lines(lowess(IVT$date_R, IVT$`Zhejiang_incident_case`))
# 
# 
# 
# 
# 
# 
# 
