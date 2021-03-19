library(readxl)
library(tidyr)
library(dplyr)

# Define path to sample template spreadsheet
baseDir <- getwd()
dataDir <- paste(baseDir, '/data', sep = '')
filePath <- file.path(paste(dataDir, '/Crop Rotation Sample Template.xlsx', sep = ''))

# List the sheets/tabs in the sample template spreadsheet
#sheets <- excel_sheets(filePath)

# Read Farmland sheet; will throw error it the sheet doesn't exist
farmland <- read_excel(filePath, sheet = 'Farmland')
         
# Keep only included fields
fieldsIncluded <- subset(as.data.frame(farmland), farmland$'Include?' == 'Y')

# Read demand
demand <- read_excel(filePath, sheet = 'Demand')

# Read crops (skip first row which only contains the label Planting Dates)
crops <- read_excel(filePath, sheet = 'Crops', skip = 1)

# Keep only selected fields
cropsIncluded <- subset(as.data.frame(crops), crops$'Plant?' == 'Y')

