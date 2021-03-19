library(tidyr)
library(dplyr)

baseDir <- getwd()
codeDir <- paste(baseDir, '/code', sep = '')
filePath <- file.path(paste(codeDir, '/ReadSpreadsheet.R', sep = ''))
source(filePath)

# To construct model variables we'll use the following naming convention:
# Suffixes: c = crop; f = field, y = year, m = month
c <- cropsIncluded[,'Crop']
f <- fieldsIncluded[,'Field']
y <- seq(1:4)
m <- month.abb

# Crops planted variables: a crop can be planted on a field in a year and a month; we need to create
# the binary variables that represent possible planting dates
# Identify the month from planting when the crop will release the field: this is the month after the harvest
cropFieldOccupation <- cropsIncluded %>%
  select('Crop', 'Days To Maturity')
cropFieldRelease <- cropFieldOccupation %>% 
  mutate('Months In Field' = round(`Days To Maturity`/30)) %>%
  select('Crop', 'Months In Field')
cropFieldOccupation <- left_join(cropFieldOccupation, cropFieldRelease, by = 'Crop')

# Gathering the crops included and their planting months and field release
cropPlantingMonths <- cropsIncluded %>% 
  select('Crop', 'Jan':'Dec') %>%
  gather(PlantingMonth, CanPlant, 'Jan':'Dec') %>%
  filter(CanPlant == 'Y') %>%
  select('Crop', 'PlantingMonth') %>%
  left_join(cropFieldOccupation, by = 'Crop') 
cropPlantingMonths <- cropPlantingMonths %>%
  mutate(plantingFieldMonth = match(cropPlantingMonths$PlantingMonth,month.abb)) %>%
  mutate(releaseFieldMonth = match(cropPlantingMonths$PlantingMonth,month.abb) + cropPlantingMonths$`Months In Field`) 
cropPlantingMonths <- cropPlantingMonths %>%
  mutate('Release Field Month' = month.abb[cropPlantingMonths$releaseFieldMonth]) %>%
  mutate('Crop-PlantingMonth' = paste(cropPlantingMonths$Crop, '-', cropPlantingMonths$PlantingMonth, sep = ''))

# Variables related to Crop Plantings over time
varCropsPlantingMonthYear <- merge(cropPlantingMonths$`Crop-PlantingMonth`, as.character(y))
varCropsPlantingMonthYear <- varCropsPlantingMonthYear %>%
  mutate(CropMonthYear = paste(varCropsPlantingMonthYear$x, '-', varCropsPlantingMonthYear$y, sep=''))
