library(tidyr)
library(dplyr)
# library(ompr)
# library(ompr.roi)

# Read Spreadsheet
baseDir <- getwd()
codeDir <- paste(baseDir, '/code', sep = '')
filePath <- file.path(paste(codeDir, '/ReadSpreadsheet.R', sep = ''))
source(filePath)

# Prepare Model Variables
filePath <- file.path(paste(codeDir, '/PrepareModelData.R', sep = ''))
source(filePath)

# Set hard coded parameters
penaltyForUnmetDemand <- 100
valueOfCoverCrop <- penaltyForUnmetDemand/5

# Invoke a solver

