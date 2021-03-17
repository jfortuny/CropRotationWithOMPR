library (readxl)
library(tidyr)
library(dplyr)

# Define path to sample template spreadsheet
baseDir <- getwd()
dataDir <- paste(baseDir, '/data', sep = '')
filePath <- file.path(paste(dataDir, '/Crop Rotation Sample Template.xlsx', sep = ''))

# List the sheets/tabs in the sample template spreadsheet
sheets <- excel_sheets(filePath)

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

# To construct model variables we'll use the following naming convention:
# Suffixes: c = crop; f = field, y = year, m = month
c <- cropsIncluded[,'Crop']
f <- fieldsIncluded[,'Field']
y <- seq(1:4)
m <- month.abb

# Crops planted variables: a crop can be planted on a field in a year and a month; we need to create
# the binary variables that represent possible planting dates
# Start by gathering the crops included and their planting months
cropPlantingMonths <- cropsIncluded %>% 
  select('Crop', 'Jan':'Dec') %>%
  gather(PlantingMonth, CanPlant, 'Jan':'Dec') %>%
  filter(CanPlant == 'Y')
# Identify the month from planting when the crop will release the field: this is the month after the harvest
cropFieldOccupation <- cropsIncluded %>%
  select('Crop', 'Days To Maturity')
cropFieldRelease <- cropFieldOccupation %>% 
  mutate('Months In Field' = round(`Days To Maturity`/30)) %>%
  select('Crop', 'Months In Field')

