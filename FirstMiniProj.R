## =====================================================
## Problem 1 (pollutantmean function)
## =====================================================

pollutantmean <- function(directory, pollutant, id = 1:332) {
  
  ## sum of all observed values of pollutant
  sum = 0     
  
  ## total number of observed values of pollutant
  total = 0    
  
  ## loop through the directory's files specified in the id argument
  for (i in id) {
    
    ## specify path because of the format of the file name
    ## for example, if id = 1, we get 001.csv          
    if (i < 10) { 
      
      ## read the csv file
      file <- read.csv(paste("0","0", as.character(i), ".csv", sep=""),
                       header = T, na.strings=c("NA","NaN", " "))
    }
    
    else if (i < 100) { 
      file <- read.csv(paste("0", as.character(i), ".csv", sep=""),
                       header = T, na.strings=c("NA","NaN", " "))
    }
    
    else { 
      file <- read.csv(paste(as.character(i), ".csv", sep=""),
                       header = T, na.strings=c("NA","NaN", " "))
    }
    
    ## omitting those with NA values
    file = na.omit(file) 
    
    ##  cumulative addition of the complete observations
    total = total + nrow(file)
    
    ## aggregate the observed values
    if (pollutant == "sulfate") {sum = sum + sum(file$sulfate)}
    else {sum = sum + sum(file$nitrate)}
  }
  
  ## returning the mean of the pollutant values
  return (sum/total)
}

# test run
pollutantmean("specdata", "sulfate", 1:10)
pollutantmean("specdata", "nitrate", 70:72)
pollutantmean("specdata", "nitrate", 23)


## =====================================================
## Problem 2 (complete function)
## =====================================================

complete <- function(directory, id = 1:332) {
  
  ## set working directory through a function
  setwd(file.path(getwd(), directory))
  #setwd("~/specdata")
  
  ## create an empty data frame
  data <- data.frame()
  
  ## loop through the directory's files specified in the id argument
  for (i in id) {
    
    ## specify path because of the format of the file name
    ## for example, if id = 1, we get 001.csv          
    if (i < 10) { 
      
      ## read the csv file
      file <- read.csv(paste("0","0", as.character(i), ".csv", sep=""),
                       header = T, na.strings=c("NA","NaN", " "))
    }
    
    else if (i < 100) { 
      file <- read.csv(paste("0", as.character(i), ".csv", sep=""),
                       header = T, na.strings=c("NA","NaN", " "))
    }
    
    else { 
      file <- read.csv(paste(as.character(i), ".csv", sep=""),
                       header = T, na.strings=c("NA","NaN", " "))
    }
    
    ## omitting those with NA values
    file = na.omit(file) 
    
    ## easily fill each successive row of the dataframe by making it a matrix
    file = as.matrix(file)
    
    ## fill each successive row of the dataframe
    ## each row contains the monitor id and the number of complete cases
    data = rbind(data, c(i,nrow(file))) 
  }
  
  ## convert matix to data frame 
  data = data.frame(data) 

  ## column names
  names(data) = c('id', 'nobs')
  return (data) 
}

## test run
complete("specdata", 1)
complete("specdata", c(2, 4, 8, 10, 12))
complete("specdata", 30:25)
complete("specdata", 3)


## =====================================================
## Problem 3 (corr function)
## =====================================================

corr <- function(directory, threshold = 0) {
  
  ## initialize correlation matrix
  correlationVector = NULL 
  
  ## loop through the directory's files specified in the id argument
  for (i in 1:332) {
    
    ## specify path because of the format of the file name
    ## for example, if id = 1, we get 001.csv            
    if (i < 10) { 
      
      ## read the csv file
      file <- read.csv(paste("0","0", as.character(i), ".csv", sep=""),
                       header = T, na.strings=c("NA","NaN", " "))
    }
    
    else if (i < 100) { 
      file <- read.csv(paste("0", as.character(i), ".csv", sep=""),  
                       header = T, na.strings=c("NA","NaN", " "))
    }
    
    else { 
      file <- read.csv(paste(as.character(i), ".csv", sep=""),
                       header = T, na.strings=c("NA","NaN", " "))
    }
    
    ## omitting those with NA values
    file = na.omit(file) 
    
    ## if the number of complete observed cases meets the quota,
    ## find the correlation between the pollutants for the given monitor
    ## store the results in the correlation matrix
    if (nrow(file) > threshold) {
      correlationVector = c(correlationVector, cor(file[,2], file[,3]))
    }
  }
  
  ## return a numeric vecor of correlations
  return (correlationVector)
}

## test run 1
cr <- corr("specdata", 150)
head(cr); summary(cr)

## test run 2
cr <- corr("specdata", 400)
head(cr); summary(cr)

## test run 3
cr <- corr("specdata", 5000)
head(cr); summary(cr)

## test run 4
cr <- corr("specdata")
head(cr); summary(cr); length(cr)


## =====================================================
## Problem 4 (Hospital data)
## =====================================================

outcome <- read.csv('outcome-of-care-measures.csv', colClasses = "character")
head(outcome)


## plot the 30-day mortality rates for heart attack given the data set outcome-of-care-measures.csv.

## make a simple histogram of the 30-day death rates from heart attack 
## column 11 in the outcome data set
outcome[, 11] <- as.numeric(outcome[, 11])
hist(outcome[, 11], col="light blue", xlab = "Deaths", main = "Hospital 30-day Death (Mortality) Rates from Heart Attack")

## run the program

