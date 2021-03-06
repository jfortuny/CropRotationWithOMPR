library(tidyr)
library(dplyr)

# baseDir <- getwd()
# codeDir <- paste(baseDir, '/code', sep = '')
# filePath <- file.path(paste(codeDir, '/ReadSpreadsheet.R', sep = ''))
# source(filePath)

# To construct model variables we'll use the following naming convention:
# Suffixes: c = crop; f = field, y = year, m = month
c <- cropsIncluded[,'Crop']
bf <- distinct(cropsIncluded, Family)
b <- bf[, 'Family']
f <- fieldsIncluded[,'Field']
y <- seq(1:4)
# y <- 1
m <- month.abb


# Crops planted variables: #####################################################
# A crop can be planted on a field in a year and a month; we need to create
# the binary variables that represent possible planting dates
# Identify the month from planting when the crop will release the field: this is the month after the harvest
cropFieldOccupation <- cropsIncluded %>%
  select('Crop', 'Days To Maturity')
cropFieldRelease <- cropFieldOccupation %>% 
#  mutate('Months In Field' = round(`Days To Maturity`/30)) %>% 
  mutate('Months In Field' = ceiling(`Days To Maturity`/30)) %>%
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
# Same Year Release calculation
cropPlantingMonths <- cropPlantingMonths %>% 
  mutate('CropMonth' = paste(cropPlantingMonths$Crop, '-', 
                             cropPlantingMonths$PlantingMonth, sep = '')) %>%
  mutate('Release Field Month' = as.integer(cropPlantingMonths$releaseFieldMonth %% 12))
cropPlantingMonths <- cropPlantingMonths %>%
  mutate(SameYearRelease = cropPlantingMonths$plantingFieldMonth <=
           cropPlantingMonths$`Release Field Month`)
#
cropPlantingMonths <- cropPlantingMonths %>%
  mutate('CropMonthRelease' = paste(cropPlantingMonths$Crop, '-', 
                                    cropPlantingMonths$`Release Field Month`, 
                                    sep = ''))
# If 'Release Field Month' == 0 then make 'Release Field Month' = releaseFieldMonth
# and SameYearRelease = TRUE
cropPlantingMonths <- cropPlantingMonths %>%
  mutate('x' = ifelse(cropPlantingMonths$'Release Field Month' == 0, 
                      TRUE, 
                      cropPlantingMonths$'SameYearRelease')
         ) %>%
  mutate('y' = ifelse(cropPlantingMonths$'Release Field Month' == 0,
                      12,
                      cropPlantingMonths$'Release Field Month')
         )
cropPlantingMonths <- left_join(cropPlantingMonths, crops, by = 'Crop')
cropPlantingMonths <- cropPlantingMonths %>%
  select('Crop', 'PlantingMonth', 'Days To Maturity.x', 'Months In Field',
         'plantingFieldMonth', 'releaseFieldMonth', 'y',
         'CropMonth', 'x', 'CropMonthRelease', 'Yield per Unit of Field')
cropPlantingMonths <- cropPlantingMonths %>% 
  rename(DaysToMaturity = 'Days To Maturity.x', 
         YieldPerUnitOfField = 'Yield per Unit of Field', 
         SameYearRelease = x)
cropPlantingMonths <- cropPlantingMonths %>%
  mutate('Release Field Month' = month.abb[y])
cropPlantingMonths <- cropPlantingMonths %>%
  select('Crop', 'PlantingMonth', 'DaysToMaturity', 'Months In Field', 'plantingFieldMonth',
         'releaseFieldMonth', 'CropMonth', 'SameYearRelease', 'CropMonthRelease',
         'YieldPerUnitOfField', 'Release Field Month')

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
# # Long variable names **********************************************************
# varCropsPlantingMonthYearFieldFull <- varCropsPlantingMonthYearFieldFull %>%
#   mutate(varIDlong = paste(varID, "_", substr(Crop, 1, 2), "_", substr(Field, 5,5), "_", Year, "_", PlantingMonth, sep = ""))
# varCropsPlantingMonthYearFieldFull <- varCropsPlantingMonthYearFieldFull %>%
#     select('Crop', 'PlantingMonth', 'Release Field Month', 'SameYearRelease', 'CropMonth',
#          'CropMonthRelease', 'YieldPerUnitOfField', 'Field', 'Measure', 'Available', 'Year', 'CropMonthYear', 
#          'CropMonthYearField', 'FieldYearMonthRelease', 'varIDlong') %>%
#   rename('varID' = 'varIDlong')
# # Long variable names **********************************************************

# NOT SURE THIS IS NEEDED HERE *************************************************
# Add crop rotation constraints required variables #############################
varCropsPlantingMonthYearFieldRotation <- left_join(varCropsPlantingMonthYearFieldFull, cropsIncluded, by = 'Crop')
varCropsPlantingMonthYearFieldRotation <- 
  varCropsPlantingMonthYearFieldRotation[, which(colnames(varCropsPlantingMonthYearFieldRotation)=='Crop'):
                                           which(colnames(varCropsPlantingMonthYearFieldRotation)=='Months between plantings on same field')]
# set Months between plantings on same field to 12 if undefined
varCropsPlantingMonthYearFieldRotation$'Months between plantings on same field'[is.na(varCropsPlantingMonthYearFieldRotation$'Months between plantings on same field')]<-12
# Add months in field of the planted crop
varCropsPlantingMonthYearFieldRotation <- left_join(varCropsPlantingMonthYearFieldRotation, cropFieldRelease, by = 'Crop')
varCropsPlantingMonthYearFieldRotation <- varCropsPlantingMonthYearFieldRotation %>%
  rename(MonthsBetweenPlantings = `Months between plantings on same field`,
         MonthsInField = `Months In Field`)

# Clean up unneeded variables ##################################################
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
# # Long variable names **********************************************************
# varUnusedFieldYearMonth <- varUnusedFieldYearMonth %>%
#   mutate (varIDlong = paste(varID, "_", substr(Field, 5,5), "_", Year, "_", Month, sep = ""))
# varUnusedFieldYearMonth <- varUnusedFieldYearMonth %>%
#   select ('Field', 'Year', 'FieldYear', 'Month', 'FieldYearMonth', 'varIDlong') %>%
#   rename('varID' = 'varIDlong')
# # Long variable names **********************************************************
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
# # Long variable names **********************************************************
# varUnmetDemandProductYear <- varUnmetDemandProductYear %>%
#   mutate(varIDlong = paste(varID, "_", substr(Crop, 1, 2), "_", Year, sep = ""))
# varUnmetDemandProductYear <- varUnmetDemandProductYear %>%
#   select('Is Same Crop As', 'Crop', 'Is Cover Crop?', 'Yield per Unit of Field',
#          'Yearly Demand', 'Year', 'UnmetDemandProductYear', 'varIDlong') %>%
#   rename('varID' = 'varIDlong')
# # Long variable names **********************************************************

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

# CONTINUE WORK HERE ************************************************************************

# Variables related to Rotation Relaxed in Year y and Month m for botanical family b
# Pick only month family combinations that have planting going on!
# varRotationRelaxedFieldFamilyYearMonth <- varCropsPlantingMonthYearFieldRotation %>% 
#   select(Family, Year, PlantingMonth) 
# varRotationRelaxedFieldFamilyYearMonth <- distinct(varRotationRelaxedFieldFamilyYearMonth)
# 
# varRotationRelaxedFieldFamilyYearMonth <- merge(f, varRotationRelaxedFieldFamilyYearMonth,  by = NULL)
# 
# varRotationRelaxedFieldFamilyYearMonth <- varRotationRelaxedFieldFamilyYearMonth %>%
#   rename(Field = x, Month = PlantingMonth) %>%
#   mutate(varID = paste('rr', as.character(row_number()), sep = '_'))
varRotationRelaxedFieldFamilyYearMonth <- varCropsPlantingMonthYearFieldRotation %>%
  select(Crop, Family, Field, Year, PlantingMonth, varID) %>%
  rename(Month = PlantingMonth, cropVarID = varID) %>%
  mutate(varID = paste('rr', as.character(row_number()), sep = '_'))


# Variables related to big M botanical family rotation constraints also added here
# varRotationDeltaFieldFamilyYearMonth <- varRotationRelaxedFieldFamilyYearMonth %>%
#   select(! contains("varID")) %>%
#   mutate(varID = paste('dr', as.character(row_number()), sep = '_'))
varRotationDeltaFieldFamilyYearMonth <- varCropsPlantingMonthYearFieldRotation %>%
  select(Crop, Family, Field, Year, PlantingMonth, varID) %>%
  rename(Month = PlantingMonth, cropVarID = varID) %>%
  mutate(varID = paste('dr', as.character(row_number()), sep = '_'))

# Save variables to File #######################################################
cropVars <- varCropsPlantingMonthYearFieldRotation %>%
  select(varID, Crop, Field, PlantingMonth, Year)
fileName <- paste(baseDir, "/data/cplexData/cropVars.csv", sep = "")
write.csv(cropVars, fileName, row.names = FALSE)

# Timing the code
endTime <- Sys.time()
lapsedTime <- difftime(startTime,  endTime, units = "secs")
print(paste("Extracting Data from Spreadsheet: ", as.character(lapsedTime), sep=''))
startTime <- Sys.time()


