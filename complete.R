complete <- function(directory, id = 1:332) {
  # Returns the number of complete observation for a list of specified monitor
  #
  # Args:
  #   'directory': a character vector of length 1 indicating
  #                the location of the CSV files
  #   'id': an integer vector indicating the monitor ID numbers to be used
  #
  # Return:
  #   a data frame of the form:
  #   id nobs
  #   1  117
  #   2  1041
  #   ...
  #   where 'id' is the monitor ID number and 'nobs' is the
  #   number of complete cases
  #
  data <- data.frame(id=id,nobs = numeric(length(id)))
  # Create character vector containing paths to monitor files
  files <- paste(directory,"/",formatC(id, width=3, flag="0"),".csv",sep="")
  # Loop through each file and compute the number of complete observations
  for (i in seq_along(files)) {
    temp <- read.csv(files[i])
    data[i,"nobs"] <- sum(!is.na(temp$sulfate) & !is.na(temp$nitrate))
  }
  data
}