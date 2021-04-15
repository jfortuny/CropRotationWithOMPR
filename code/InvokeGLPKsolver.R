library(dplyr)
# library(fs)

# In this script we create the data in the CPLEX LP format, we store the data in a file for review
# via lpsolve and visually and then invoke the solver (after reading the lp file again - a bit kludgy)

# CPLEX LP File Storage location
baseDir <- getwd()
dataDir <- paste(baseDir, '/data/cplexData', sep = '')
fileName <- paste('model-', as.character(Sys.Date()), sep = '')
filePath <- file.path(paste(dataDir, '/', fileName, '.lp', sep = ''))

# Create the stub for the problem sections
title = paste('\\* Crop Rotation ', as.character(lubridate::year(Sys.Date())), ' *\\', sep = '')
objective = c('', 'minimize', 'Z:')
constraintsLand = ''
constraintsDemand = ''
bounds = ''
general = c('', 'general')
integer = c('', 'integer')
binary = c('', 'binary')

# BUILD VARIABLES
# Add Variables related to Crops Planted: varCropsPlantingMonthYearFieldFull$varID
binary = c(binary, paste(' ', varCropsPlantingMonthYearFieldFull$varID, sep = ''))

# Add variables related to fields not planted: varUnusedFieldYearMonth$varID
binary = c(binary, varUnusedFieldYearMonth$varID)

# Add variables related to unmet demand: varUnmetDemandProductYear$varID
general <- c(general, varUnmetDemandProductYear$varID)

# BUILD OBJECTIVE FUNCTION
row <- paste(' + ', as.character(penaltyForUnmetDemand), ' ', varUnmetDemandProductYear$varID, sep = '')
objective <- c(objective, row)

# BUILD CONSTRAINTS

# WRITE TO FILE
# Write the title with overwrite
write(title, file = filePath, ncolumns = 1, append = FALSE)

# Append the Objective function
write(objective, file = filePath, ncolumns = 1, append = TRUE)

# Append Binary Variables
write(binary, file = filePath, ncolumns = 1, append = TRUE)

# Append Continuous/general Variables
write(general, file = filePath, ncolumns = 1, append = TRUE)
