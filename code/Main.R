library(tidyr)
library(dplyr)
library(ompr)
library(ompr.roi)

# Read Spreadsheet
baseDir <- getwd()
codeDir <- paste(baseDir, '/code', sep = '')
filePath <- file.path(paste(codeDir, '/ReadSpreadsheet.R', sep = ''))
source(filePath)

# Prepare Model Variables
filePath <- file.path(paste(codeDir, '/PrepareModel.R', sep = ''))
source(filePath)

# Set hard coded parameters
penaltyForUnmetDemand <- 100

# Invoke a solver

