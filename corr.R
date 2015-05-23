# Write a function that takes a directory of data files and a threshold for 
# complete cases and calculates the correlation between sulfate and nitrate 
# for monitor locations where the number of completely observed cases 
# (on all variables) is greater than the threshold. The function should return 
# a vector of correlations for the monitors that meet the threshold requirement. 
# If no monitors meet the threshold requirement, then the function should return 
# a numeric vector of length 0.

# For this function you will need to use the 'cor' function in R which calculates 
# the correlation between two vectors. Please read the help page for this function 
# via '?cor' and make sure that you know how to use it.

## 'directory' is a character vector of length 1 indicating
## the location of the CSV files

## 'threshold' is a numeric vector of length 1 indicating the
## number of completely observed observations (on all
## variables) required to compute the correlation between
## nitrate and sulfate; the default is 0

## Return a numeric vector of correlations
## NOTE: Do not round the result!

corr <- function(directory, threshold = 0) {
    source("complete.R")
    completeCases <- complete(directory)
    casesAboveThreshold <- completeCases[completeCases$nobs > threshold,1]
    allFiles <- list.files(path = directory, full.names = TRUE)
    correlations <- vector("numeric", length = 0)
    for (i in casesAboveThreshold) {
        fileData <- (read.csv(allFiles[i]))
        completeCases <- complete.cases(fileData)
        validSulfateData <- fileData[completeCases, 2]
        validNitrateData <- fileData[completeCases, 3]
        correlations[i] <- cor(x = validSulfateData, y = validNitrateData)
        # print(allFiles[i])
        # print(head(fileData[completeCases,]))
        # print(validSulfateData)
        # print(validNitrateData)
    }
    correlations <- correlations[complete.cases(correlations)]
}
