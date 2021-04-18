library(dplyr)
# library(glue)
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
Zrow <- paste(' + ', as.character(penaltyForUnmetDemand), ' ', varUnmetDemandProductYear$varID, sep = '')
ZrowSingle <- paste(Zrow, collapse = "")
objective <- c(objective, ZrowSingle)

# BUILD CONSTRAINTS
# Land transfers: field, year and month
# Crops Planted this period
cp <-
  varCropsPlantingMonthYearFieldFull %>% 
  mutate(YearMonth = paste(varCropsPlantingMonthYearFieldFull$Year, 
                           "-", 
                           varCropsPlantingMonthYearFieldFull$PlantingMonth, 
                           sep = "")) %>%
  select(YearMonth, varID) %>%
  mutate(cp_varID = varID) %>%
  select(YearMonth, cp_varID)
# Fields Unused this period
fu <-
  varUnusedFieldYearMonth %>% mutate(YearMonth = paste(
    varUnusedFieldYearMonth$Y,
    "-",
    varUnusedFieldYearMonth$Month,
    sep = ""
  )) %>%
  select(YearMonth, varID) %>%
  mutate(fu_varID = varID) %>%
  select(YearMonth, fu_varID)

#left_join(fu, cp, by = "YearMonth")

# Fields unused last period (will be available this period)
fulp <- varUnusedFieldYearMonth %>%
  mutate(AvailableInMonthNumeric = match(varUnusedFieldYearMonth$Month, month.abb)+1
         ) %>%
  mutate(AvailableInYear =
           if_else(AvailableInMonthNumeric != 13, Year, as.character(as.numeric(Year) + 1))) %>%
  mutate(AvailableInMonthNumeric2 = if_else(AvailableInMonthNumeric==13,1,AvailableInMonthNumeric)
         ) %>%
  mutate(AvailableInMonth = month.abb[AvailableInMonthNumeric2]) %>%
  mutate(AvailableInYearMonth = paste(AvailableInYear,"-",AvailableInMonth,sep = "")) %>%
  mutate(YearMonth = if_else(AvailableInYear == "5", NA_character_, paste(AvailableInYear, "-", Month, sep = "")
                             )
         ) %>%
  filter(AvailableInYear!="5") %>%
  select(YearMonth, AvailableInYearMonth, varID)

# Fields Freed by crops this period
ff <- varCropsPlantingMonthYearFieldFull %>%
  mutate(
    YearMonth = paste(
      varCropsPlantingMonthYearFieldFull$Year,
      "-",
      varCropsPlantingMonthYearFieldFull$PlantingMonth,
      sep = ''
    )
  ) %>%
  mutate(ReleaseYear =
           if_else(SameYearRelease, Year,
                   as.character(as.numeric(Year) + 1))) %>%
  mutate(
    ReleaseYearMonth =
      paste(
        ReleaseYear,
        '-',
        varCropsPlantingMonthYearFieldFull$`Release Field Month`
        ,
        sep = ''
      )
  ) %>%
  select(YearMonth, ReleaseYearMonth, varID) %>%
  mutate(ff_varID = varID) %>%
  select(YearMonth, ReleaseYearMonth, ff_varID)

# left_join(fu, ff, by = "YearMonth")




# WRITE TO FILE
# Write the title with overwrite
write(title, file = filePath, ncolumns = 1, append = FALSE)

# Append the Objective function
write(objective, file = filePath, ncolumns = 1, append = TRUE)

# Append Binary Variables
write(binary, file = filePath, ncolumns = 1, append = TRUE)

# Append Continuous/general Variables
write(general, file = filePath, ncolumns = 1, append = TRUE)
