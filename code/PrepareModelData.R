library(tidyr)
library(dplyr)

# baseDir <- getwd()
# codeDir <- paste(baseDir, '/code', sep = '')
# filePath <- file.path(paste(codeDir, '/ReadSpreadsheet.R', sep = ''))
# source(filePath)

# To construct model variables we'll use the following naming convention:
# Suffixes: c = crop; f = field, y = year, m = month
c <- cropsIncluded[,'Crop']
f <- fieldsIncluded[,'Field']
y <- seq(1:4)
y <- 1
m <- month.abb

# Crops planted variables: #####################################################
# A crop can be planted on a field in a year and a month; we need to create
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
cropPlantingMonths <- cropPlantingMonths %>%
  mutate('CropMonthRelease' = paste(cropPlantingMonths$Crop, '-', cropPlantingMonths$`Release Field Month`, sep = ''))
cropPlantingMonths <- left_join(cropPlantingMonths, crops, by = 'Crop')
cropPlantingMonths <- cropPlantingMonths %>%
  select('Crop', 'PlantingMonth', 'Days To Maturity.x', 'Months In Field',
         'plantingFieldMonth', 'releaseFieldMonth', 'Release Field Month',
         'CropMonth', 'SameYearRelease', 'CropMonthRelease', 'Yield per Unit of Field')
cropPlantingMonths <- cropPlantingMonths %>% 
  rename(DaysToMaturity = 'Days To Maturity.x', YieldPerUnitOfField = 'Yield per Unit of Field')

# Variables related to Crop Plantings over time ################################
varCropsPlantingMonthYear <- merge(cropPlantingMonths$`CropMonth`, as.character(y), by = NULL)
varCropsPlantingMonthYear <- varCropsPlantingMonthYear %>%
  mutate(CropMonthYear = paste(varCropsPlantingMonthYear$x, '-', varCropsPlantingMonthYear$y, sep='')) %>%
  rename(CropMonth = x, Year = y)

varCropsPlantingMonthYearField <- merge(varCropsPlantingMonthYear$CropMonthYear, f)
varCropsPlantingMonthYearField <- varCropsPlantingMonthYearField %>%
  mutate(CropMonthYearField = paste(varCropsPlantingMonthYearField$x, '-', varCropsPlantingMonthYearField$y, sep = '')) %>%
  rename(CropMonthYear = x, Field = y)

varCropsPlantingMonthYearField <- inner_join(varCropsPlantingMonthYearField, varCropsPlantingMonthYear,
                                             by = 'CropMonthYear')
varCropsPlantingMonthYearFieldFull <- inner_join(varCropsPlantingMonthYearField, cropPlantingMonths,
                                             by = 'CropMonth') %>%
  select(Crop, PlantingMonth, 'Release Field Month', SameYearRelease, CropMonth, CropMonthRelease,
         YieldPerUnitOfField, Field, Year, CropMonthYear, CropMonthYearField)
varCropsPlantingMonthYearFieldFull <-
  varCropsPlantingMonthYearFieldFull %>%
  mutate(FieldYearMonthRelease =
           if_else(
             SameYearRelease,
             paste(
               Field,
               '-',
               Year,
               '-',
               varCropsPlantingMonthYearFieldFull$'Release Field Month',
               sep = ''
             ),
             paste(
               Field,
               '-',
               Year,
               '-',
               varCropsPlantingMonthYearFieldFull$'Release Field Month',
               sep = ''
             )
           )) %>%
  mutate(varID = paste('cpmy', as.character(row_number()), sep = '_'))
varCropsPlantingMonthYearFieldFull <- left_join(varCropsPlantingMonthYearFieldFull, fieldsIncluded,
                                                by = 'Field') 
varCropsPlantingMonthYearFieldFull <- varCropsPlantingMonthYearFieldFull %>%
  select('Crop', 'PlantingMonth', 'Release Field Month', 'SameYearRelease', 'CropMonth',
         'CropMonthRelease', 'YieldPerUnitOfField', 'Field', 'Available', 'Year', 'CropMonthYear', 
         'CropMonthYearField', 'FieldYearMonthRelease', 'varID')
varCropsPlantingMonthYearFieldFull <- left_join(varCropsPlantingMonthYearFieldFull, fieldsIncluded, by = "Field")
varCropsPlantingMonthYearFieldFull <- varCropsPlantingMonthYearFieldFull %>%
  rename('Available' = 'Available.x') %>%
  select('Crop', 'PlantingMonth', 'Release Field Month', 'SameYearRelease', 'CropMonth',
         'CropMonthRelease', 'YieldPerUnitOfField', 'Field', 'Measure', 'Available', 'Year', 'CropMonthYear', 
         'CropMonthYearField', 'FieldYearMonthRelease', 'varID')
# Long variable names **********************************************************
varCropsPlantingMonthYearFieldFull <- varCropsPlantingMonthYearFieldFull %>%
  mutate(varIDlong = paste(varID, "_", substr(Crop, 1, 2), "_", substr(Field, 5,5), "_", Year, "_", PlantingMonth, sep = ""))
varCropsPlantingMonthYearFieldFull <- varCropsPlantingMonthYearFieldFull %>%
    select('Crop', 'PlantingMonth', 'Release Field Month', 'SameYearRelease', 'CropMonth',
         'CropMonthRelease', 'YieldPerUnitOfField', 'Field', 'Measure', 'Available', 'Year', 'CropMonthYear', 
         'CropMonthYearField', 'FieldYearMonthRelease', 'varIDlong') %>%
  rename('varID' = 'varIDlong')
# Long variable names **********************************************************
# Clean up unneeded variables
rm(varCropsPlantingMonthYear)
rm(varCropsPlantingMonthYearField)

# Variables related to field/land not used in a certain month and year #########
varUnusedFieldYear <- merge(f, as.character(y), by = NULL)
varUnusedFieldYear <- varUnusedFieldYear %>%
  mutate(FieldYear = paste(varUnusedFieldYear$x, '-', varUnusedFieldYear$y, sep = '')) %>%
  rename(Field = x, Year = y)
varUnusedFieldYearMonth <- merge(varUnusedFieldYear, m, by = NULL)
varUnusedFieldYearMonth <- varUnusedFieldYearMonth %>%
  mutate(FieldYearMonth = paste(FieldYear, '-', y, sep = '')) %>%
  rename(Month = y) %>%
  mutate(varID = paste('uf', as.character(row_number()), sep = '_'))
# Long variable names **********************************************************
varUnusedFieldYearMonth <- varUnusedFieldYearMonth %>%
  mutate (varIDlong = paste(varID, "_", substr(Field, 5,5), "_", Year, "_", Month, sep = ""))
varUnusedFieldYearMonth <- varUnusedFieldYearMonth %>%
  select ('Field', 'Year', 'FieldYear', 'Month', 'FieldYearMonth', 'varIDlong') %>%
  rename('varID' = 'varIDlong')
# Long variable names **********************************************************
# Clean up unneeded variables
rm(varUnusedFieldYear)

# Variables related to unmet demand for "Is Same Crop As" product ##############
varUnmetDemandProductYear <- merge(demandMeetable, as.character(y), by = NULL)
varUnmetDemandProductYear <- varUnmetDemandProductYear %>%
  rename(Year = y)
varUnmetDemandProductYear <- varUnmetDemandProductYear %>%
  mutate(UnmetDemandProductYear = paste(varUnmetDemandProductYear$'Is Same Crop As', '-', 
                                        varUnmetDemandProductYear$Year, sep = '')) %>%
  mutate(varID = paste('ud', as.character(row_number()), sep = '_'))
# Long variable names **********************************************************
varUnmetDemandProductYear <- varUnmetDemandProductYear %>%
  mutate(varIDlong = paste(varID, "_", substr(Crop, 1, 2), "_", Year, sep = ""))
varUnmetDemandProductYear <- varUnmetDemandProductYear %>%
  select('Is Same Crop As', 'Crop', 'Is Cover Crop?', 'Yield per Unit of Field',
         'Yearly Demand', 'Year', 'UnmetDemandProductYear', 'varIDlong') %>%
  rename('varID' = 'varIDlong')
# Long variable names **********************************************************

# # Variables for Constraints for field transfers over time (Field-Year-Month)
# varConstraintFieldYearMonth <- merge(f, as.character(y), by = NULL)
# varConstraintFieldYearMonth <- varConstraintFieldYearMonth %>%
#   mutate(FieldYear = paste(varConstraintFieldYearMonth$x, '-', varConstraintFieldYearMonth$y, sep = '')) %>%
#   rename(Field = x, Year = y)
# varConstraintFieldYearMonth <- merge(varConstraintFieldYearMonth, m, by = NULL)
# varConstraintFieldYearMonth <- varConstraintFieldYearMonth %>%
#   mutate(FieldYearMonth = paste(FieldYear, '-', y, sep = '')) %>%
#   rename(Month = y)

# Variables related to Crops Planted in Year 1 month 1 #########################
varCropsPlantedInFirstMonthFirstYear <- varCropsPlantingMonthYearFieldFull %>% 
  filter(Year == "1" & PlantingMonth == "Jan") %>%
  select(CropMonthYearField)
# Variables related to Fields Unused, not planted, in Year 1 Month 1
varFieldsUnusedInFirstMonthFirstYear <- varUnusedFieldYearMonth %>% 
  filter(Year == "1" & Month == "Jan") %>% 
  select(FieldYearMonth)

