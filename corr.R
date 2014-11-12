corr <- function(directory, threshold = 0) {
  # Return a numeric vector of correlations
  #
  # Args:
  #   'directory': a character vector of length 1 indicating the location
  #                of the CSV files
  #   'threshold': a numeric vector of length 1 indicating the number of
  #                completely observed observations (on all variables) required
  #                to compute the correlation between nitrate and sulfate;
  #                the default is 0
  #
  # Return:
  #     a numeric vector of correlations
  #
  cor.vector <- numeric()
  # Compute number of complete observations for all files in directory
  num.obs <- complete(directory, id = 1:332)
  # Limit the analysis to the monitor with enough observations
  enough.obs <- num.obs[num.obs[,"nobs"]>threshold, ]
  # if no monitors meet the threshold requirement, returns empty vector
  if (nrow(enough.obs) == 0) {
    return(numeric())
  }
  # Create character vector containing paths to monitor files
  files <- paste(directory,"/",formatC(enough.obs[,"id"], width=3, flag="0"),".csv",sep="")
  # Loop through each file and compute correlation
  for (i in seq_along(files)) {
    temp <- read.csv(files[i])
    valid.obs <- !is.na(temp$sulfate) & !is.na(temp$nitrate)
    cor.value <- cor(temp[valid.obs,"sulfate"],temp[valid.obs,"nitrate"])
    cor.vector <- c(cor.vector, cor.value)
  }
  cor.vector
}