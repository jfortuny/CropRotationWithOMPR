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
  mutate(releaseFieldMonth = match(cropPlantingMonths$PlantingMonth,month.abb) + 
           cropPlantingMonths$`Months In Field`) 
cropPlantingMonths <- cropPlantingMonths %>%
  mutate('Release Field Month' = month.abb[cropPlantingMonths$releaseFieldMonth]) %>%
  mutate('CropMonth' = paste(cropPlantingMonths$Crop, '-', 
                             cropPlantingMonths$PlantingMonth, sep = ''))
cropPlantingMonths <- cropPlantingMonths %>%
  mutate(SameYearRelease = (match(cropPlantingMonths$PlantingMonth,m) <= 
                              match(cropPlantingMonths$`Release Field Month`,m)))


# Variables related to Crop Plantings over time
varCropsPlantingMonthYear <- merge(cropPlantingMonths$`Crop-PlantingMonth`, as.character(y), by = NULL)
varCropsPlantingMonthYear <- varCropsPlantingMonthYear %>%
  mutate(CropMonthYear = paste(varCropsPlantingMonthYear$x, '-', varCropsPlantingMonthYear$y, sep='')) %>%
  rename(CropMonth = x, Year = y)
varCropsPlantingMonthYearField <- merge(varCropsPlantingMonthYear$CropMonthYear, f)
varCropsPlantingMonthYearField <- varCropsPlantingMonthYearField %>%
  mutate(CropMonthYearField = paste(varCropsPlantingMonthYearField$x, '-', varCropsPlantingMonthYearField$y, sep = '')) %>%
  rename(CropMonthYear = x, Field = y)

# Variables related to field/land not used in a certain month and year
varUnusedFieldYear <- merge(f, as.character(y), by = NULL)
varUnusedFieldYear <- varUnusedFieldYear %>%
  mutate(FieldYear = paste(varUnusedFieldYear$x, '-', varUnusedFieldYear$y, sep = '')) %>%
  rename(Field = x, Year = y)
varUnusedFieldYearMonth <- merge(varUnusedFieldYear, m, by = NULL)
varUnusedFieldYearMonth <- varUnusedFieldYearMonth %>%
  mutate(FieldYearMonth = paste(FieldYear, '-', y, sep = '')) %>%
  rename(Month = y)

# Variables related to field released from a harvested crop in a certain month and year


# Constraints related to field transfers and allocations to crops

