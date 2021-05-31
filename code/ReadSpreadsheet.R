library(readxl)
library(tidyr)
library(dplyr)

# Define path to sample template spreadsheet
baseDir <- getwd()
dataDir <- paste(baseDir, '/data', sep = '')
# filePath <- file.path(paste(dataDir, '/Crop Rotation Sample Template.xlsx', sep = ''))
# filePath <- file.path(paste(dataDir, '/Crop Rotation Sample Template - 2 crops and 2 fields.xlsx', sep = ''))
# filePath <- file.path(paste(dataDir, '/Crop Rotation Sample Template - 1 crop and 1 field.xlsx', sep = ''))
filePath <- file.path(paste(dataDir, '/Crop Rotation Sample Template - Botanical Family test.xlsx', sep = ''))

# List the sheets/tabs in the sample template spreadsheet
#sheets <- excel_sheets(filePath)

# Read Farmland sheet; will throw error it the sheet doesn't exist
farmland <- read_excel(filePath, sheet = 'Farmland')
         
# Keep only included fields
fieldsIncluded <- subset(as.data.frame(farmland), farmland$'Include?' == 'Y')

# Read crops (skip first row which only contains the label Planting Dates)
crops <- read_excel(filePath, sheet = 'Crops', skip = 1)

# Keep only selected fields
cropsIncluded <- subset(as.data.frame(crops), crops$'Plant?' == 'Y')

# Read demand
demand <- read_excel(filePath, sheet = 'Demand')
# Demand can be met only if crops that satisfy it are included; keep only same as crops
# whose demand can be met as well as cover crops that can be planted (which should have a zero demand)
demandMeetable <- cropsIncluded %>%
  select('Crop', 'Is Same Crop As', 'Is Cover Crop?', 'Yield per Unit of Field')
demandMeetable <- merge(demandMeetable, demand, by = 'Is Same Crop As')
