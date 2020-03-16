## function for scatterplot and lowess

## Country is Country for which data reported (string)
## date_var is date variable (string)
## data is data.frame (name).

plot_and_lowess <- function(country = "Beijing", data = IVT, date_var = "date_R"){
  #ggplot(data, aes(eval(parse(date_var)), eval(parse(paste0(country, "_incident_case"))))) + 
   # geom_point() +
    #geom_smooth(method = "loess", se = FALSE)
  
   plot(data[, date_var], data[, paste0(country, "_incident_case")], xlab = "Date", ylab = "Counts per day", 
   main = country)
  lines(lowess(data[, date_var], data[, paste0(country, "_incident_case")], f = 0.45), col = "red", lwd = 2)
}


# Find max value in data.frame
colMax <- function(data) sapply(data, max, na.rm = TRUE)

# Sort data.frame by maximum value
colSort <- function(data, ...) sapply(data, sort, ...)
