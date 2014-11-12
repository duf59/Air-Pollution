pollutantmean <- function(directory, pollutant, id = 1:332) {
  # Compute the mean of a pollutant across a list of monitors
  #
  # Args:
  #   'directory': a character vector of length 1 indicating the location
  #                of the CSV files
  #   'pollutant': a character vector of length 1 indicating the name of the 
  #                pollutant for which we will calculate the mean.
  #                either "sulfate" or "nitrate".
  #   'id': an integer vector indicating the monitor ID numbers to be used
  #
  # Returns:
  #   mean.pol.value: a numeric corresponding to the mean of the
  #                   pollutant across all monitors list in the 'id' vector
  #                   (ignoring NA values)
  #
  #
  data <- numeric()
  # Create character vector containing paths to monitor files
  files <- paste(directory,"/",formatC(id, width=3, flag="0"),".csv",sep="")
  # Loop through each file and concatenate the pollutant data
  for (i in seq_along(files)) {
    temp <- read.csv(files[i])
    data <- c(data,temp[,pollutant])
  }
  # Compute and return the mean pollutant value (ignoring NA)
  mean(data, na.rm = TRUE)
}

