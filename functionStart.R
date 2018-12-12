# R 3.5.1

# This script works lucy through the start of building functions to display tables and plots.

# Open packages
library(tidyverse)
library(readxl)
library(DT)

# Read in data
reed2yr <- read_excel('data/Reed2yr.xlsx')
reed6yr <- read_excel('data/Reed6yr.xlsx')

# We want to create a function that will display just spring and fall VSCI by year regardless of how many years in original dataset

#testing zone
annualData <- reed6yr

basicTable <- function(annualData){
  # grab info we want from input dataframe
  tableData <- select(annualData, Year, Fall, Spring) %>%
    mutate(Fall = format(Fall, digits=3),
           Spring = format(Spring, digits=3))
  
  #make table the way we want
  datatable(tableData, rownames=F, options = list(scrollY = '200px', pageLength= 6, dom = 't'))
  
  
}
# remove testing objects
rm(annualData);rm(tableData)

# test your function!!!!
basicTable(reed6yr)
basicTable(reed2yr)





# new inner function 