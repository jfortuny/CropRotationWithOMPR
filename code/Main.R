library(tidyr)
library(dplyr)
# library(ompr)
# library(ompr.roi)

# Naming the run
myTitle <- "15 Crops in 20 Fields - includes cover crops"

# Timing the code
op <- options(digits.secs = 2)
startTime <- Sys.time()
print(paste("Started On: ", startTime, sep = ""))


# Read Spreadsheet
baseDir <- getwd()
codeDir <- paste(baseDir, '/code', sep = '')
filePath <- file.path(paste(codeDir, '/ReadSpreadsheet.R', sep = ''))
source(filePath)

# Prepare Model Variables
filePath <- file.path(paste(codeDir, '/PrepareModelData.R', sep = ''))
source(filePath)

# Set hard coded parameters
penaltyForUnmetDemand <- 1000
penaltyForRelaxedRotation <- penaltyForUnmetDemand/2
valueOfCoverCrop <- penaltyForUnmetDemand/5

# Invoke a solver
filePath <- file.path(paste(codeDir, '/InvokeGLPKsolver.R', sep = ''))
source(filePath)

print(paste("Ended On: ", Sys.time(), sep = ""))
