## function for scatterplot and lowess

## Country is Country for which data reported (string)
## date_var is date variable (string)
## data is data.frame (name).

plot_and_lowess <- function(country = "Beijing", data = IVT, date_var = "date_R"){
  plot(data[, date_var], data[, paste0(country, "_incident_case")], xlab = "Date", ylab = "Counts per day", 
  main = paste0("Confirmed cases in ", country))
lines(lowess(data[, date_var], data[, paste0(country, "_incident_case")], f= 0.35))
}


# Find max value in data.frame
colMax <- function(data) sapply(data, max, na.rm = TRUE)

# Sort data.frame by maximum value
colSort <- function(data, ...) sapply(data, sort, ...)
