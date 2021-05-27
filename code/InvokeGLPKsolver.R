library(dplyr)
library(Rglpk)
# library(glue)
# library(fs)

# In this script we create the data in the CPLEX LP format, we store the data in a file for review
# via lpsolve and visually and then invoke the solver (after reading the lp file again - a bit kludgy)

# CPLEX LP File Storage location
baseDir <- getwd()
dataDir <- paste(baseDir, '/data/cplexData', sep = '')
fileName <- paste('model-', as.character(Sys.Date()), sep = '')
filePath <- file.path(paste(dataDir, '/', fileName, '.lpt', sep = ''))

# Create the stub for the problem sections
title = paste('\\* Crop Rotation ', as.character(lubridate::year(Sys.Date())), ' *\\', sep = '')
objective = c('', 'minimize', 'Z:')
constraints = c('', 'Subject to')
constraintsLand = ''
constraintsDemand = ''
bounds = ''
general = c('', 'general')
integer = c('', 'integer')
binary = c('', 'binary')

# BUILD CONSTRAINTS ############################################################
# Land transfers: field, year and month


# Build empty dataframe for matrix #############################################
dfColNames <- c(varCropsPlantingMonthYearFieldFull$varID, varUnusedFieldYearMonth$varID)
dfRowNames <- paste("c_", varUnusedFieldYearMonth$varID, sep = "")
# Add column for the RHS
dfMatrix <- data.frame(matrix(0, nrow = length(dfRowNames), ncol = 1 + length(dfColNames)), stringsAsFactors = FALSE)
colnames(dfMatrix) <- c(dfColNames, "RHS")
row.names(dfMatrix) <- dfRowNames
# Add column for the Sense of the constraint
dfMatrix$Sense <- rep("=", length(dfRowNames))

# Set unused fields in Year 1 Month 1 ##########################################
irows <- varUnusedFieldYearMonth %>% filter(Year == "1" & Month == "Jan") %>% select(varID)
for (i in 1:nrow(irows)) {
  j <- paste("c_", irows[i, "varID"], sep = "")
  dfMatrix[j, irows[i, "varID"]] <- '+ 1'
  dfMatrix[j, "Sense"] <- "<="
  dfMatrix[j, "RHS"] <- '1'
}

# Set unused fields for Year <> 1 and Month <> 1 ###############################
irows <- varUnusedFieldYearMonth %>% filter(!(Year == "1" & Month == "Jan")) %>% select(varID)
dfPredecessors <- left_join(irows, varUnusedFieldYearMonth, by = "varID") %>% select(c("varID", "Field", "Year", "Month"))

for (i in 1:nrow(dfPredecessors)) {
  if (dfPredecessors[i, "Month"] != "Jan") {
    dfPredecessors[i, "previousYear"] <- dfPredecessors[i, "Year"]
    dfPredecessors[i, "previousMonth"] <-
      month.abb[match(dfPredecessors[i,"Month"], month.abb) - 1]
  } else {
    dfPredecessors[i, "previousYear"] <-
      as.character(as.numeric(dfPredecessors[i, "Year"]) - 1)
    dfPredecessors[i, "previousMonth"] <- "Dec"
  }
}
dfPredecessors <-
  left_join(
    dfPredecessors,
    varUnusedFieldYearMonth,
    by = c(
      "Field",
      "previousYear" = "Year",
      "previousMonth" = "Month"
    )
  ) %>% select("varID.x", "varID.y") %>% mutate(constraint = paste("c_", varID.x, sep = ""))

for (i in 1:nrow(dfPredecessors)) {
  dfMatrix[dfPredecessors[i, "constraint"], dfPredecessors[i, "varID.x"]] <- "+ 1"
  dfMatrix[dfPredecessors[i, "constraint"], dfPredecessors[i, "varID.y"]] <- "- 1"
  dfMatrix[dfPredecessors[i, "constraint"], "RHS"] <- '0'
  dfMatrix[dfPredecessors[i, "constraint"], "Sense"] <- "<="
}



# Set Field Use for Planted Crops ##############################################
# Since the unused field settings are already taken care of, the only variables we
# need to worry about here are the "Crops Planted" variables (cpmy_i) both as they
# use a field and as they release the field for the next crop.
# Field occupation/planting
irows <- left_join(varCropsPlantingMonthYearFieldFull, varUnusedFieldYearMonth, 
                   by = c("Field", "Year", "PlantingMonth" = "Month")) %>%
  select(varID.x, varID.y) %>% mutate(constraint = paste("c_", varID.y, sep = ""))
for (i in 1:nrow(irows)) {
  dfMatrix[irows[i, "constraint"], irows[i,"varID.x"]] <- "+ 1"
  # dfMatrix[irows[i, "constraint"], "RHS"] <- '0'
  # dfMatrix[irows[i, "constraint"], "Sense"] <- "="
}
# Field harvesting/release
irows <- left_join(varCropsPlantingMonthYearFieldFull, varUnusedFieldYearMonth,
                   by = c("FieldYearMonthRelease" = "FieldYearMonth")) %>%
  select(varID.x, varID.y) %>% mutate(constraint = paste("c_", varID.y, sep = ""))
for (i in 1:nrow(irows)) {
  dfMatrix[irows[i, "constraint"], irows[i,"varID.x"]] <- "- 1"
  # dfMatrix[irows[i, "constraint"], "RHS"] <- '0'
  # dfMatrix[irows[i, "constraint"], "Sense"] <- "="
}
# 




# BUILD DEMAND CONSTRAINTS #####################################################
# Build empty dataframe for matrix #############################################
dfColNames2 <- c(varCropsPlantingMonthYearFieldFull$varID, varUnmetDemandProductYear$varID)
dfRowNames2 <- paste("c_", varUnmetDemandProductYear$varID, sep = "")
# Add column for the RHS
dfMatrix2 <- data.frame(matrix(0, nrow = length(dfRowNames2), ncol = 1 + length(dfColNames2)), stringsAsFactors = FALSE)
colnames(dfMatrix2) <- c(dfColNames2, "RHS")
row.names(dfMatrix2) <- dfRowNames2
# Add column for the Sense of the constraint
dfMatrix2$Sense <- rep(">=", length(dfRowNames2))

# Set the unmet demand columns to -1
for (i in varUnmetDemandProductYear$varID) {
  thisRow <- paste('c_', i, sep = "")
  dfMatrix2[thisRow, i] <- "+ 1"
}

# Set the RHS  to the demand for product and year
for (i in varUnmetDemandProductYear$varID) {
  thisRow <- paste('c_', i, sep = "")
  dfMatrix2[thisRow, "Sense"] <- ">="
  dfMatrix2[thisRow, "RHS"] <- filter(varUnmetDemandProductYear, varID == i) %>% select('Yearly Demand')
}

# Set the crop production values to + YieldPerUnitOfField * Available for the appropriate field and year
for (i in 1:nrow(varCropsPlantingMonthYearFieldFull)) {
  thisColumn <- varCropsPlantingMonthYearFieldFull[i, 'varID']
  thisCrop <- varCropsPlantingMonthYearFieldFull[i, 'Crop']
  thisCoefficient <- varCropsPlantingMonthYearFieldFull[i, 'YieldPerUnitOfField'] *
    varCropsPlantingMonthYearFieldFull[i, 'Available']
  thisCoefficient <- paste("+ ", thisCoefficient, sep = "")
  if (varCropsPlantingMonthYearFieldFull[i, 'SameYearRelease']) {
    thisYear <- varCropsPlantingMonthYearFieldFull[i, 'Year']
  } else {
    thisYear <- as.character(as.numeric(varCropsPlantingMonthYearFieldFull[i, 'Year']) + 1)
  }
  if (thisYear %in% c("1", "2", "3", "4")) {
    thisRow <- filter(varUnmetDemandProductYear, Year == thisYear & Crop == thisCrop) %>%
      select(varID)
    thisRow <- paste("c_", thisRow, sep = "")
    if(substr(thisRow, 1,4) != "c_ud") {
      print(sprintf("Bad Row: Year = %s, Crop = %s, Column = %s, Row = %s", thisYear, thisCrop, thisColumn, thisRow))
    }
    dfMatrix2[thisRow, thisColumn] <- thisCoefficient
  } else {
    next
  }
}

# BUILD ROTATION CONSTRAINTS ###################################################
# Build empty dataframe for matrix #############################################
dfColNames3 <- c(varCropsPlantingMonthYearFieldRotation$varID, varRotationRelaxedFieldFamilyYearMonth$varID)
# dfRN3 <- select(varCropsPlantingMonthYearFieldRotation, Crop, varID, MonthsBetweenPlantings, MonthsInField) %>%
#   mutate(Counter = MonthsBetweenPlantings + MonthsInField) %>%
#   select(varID, Counter)
# dfRowNames3 <- NULL
# for (i_rn3 in dfRN3$varID) {
#   for (i_counter in select(filter(dfRN3, varID == i_rn3), Counter)) {
#     for (i in 1: i_counter) {
#     dfRowNames3 <- c(dfRowNames3, paste("c_", i_rn3, "_", as.character(i), sep = ""))
#     }
#   }
# }
# rm(dfRN3)

# Add column for the RHS
dfMatrix3 <- data.frame(matrix(0, nrow = 0, ncol = 1 + length(dfColNames3)), stringsAsFactors = FALSE)
colnames(dfMatrix3) <- c(dfColNames3, "RHS")
# row.names(dfMatrix3) <- dfRowNames3
# Add column for the Sense of the constraint
# dfMatrix3$Sense <- rep("<=", length(dfRowNames3))

relaxedRotationcounter <- 0
for (i_f in fieldsIncluded$Field) {
  thisField <- i_f
  for (i_y in y) {
    thisYear <- i_y
    for (i_m in m) {
      thisMonth <- i_m
      
      # Deal with crops other than cover crops
      for (i_c in cropsIncluded$Crop) {
        thisCrop <- i_c
        if (cropsIncluded[cropsIncluded$Crop == thisCrop, 'Is Cover Crop?'] == "Y") {
          # continue
        } else {
          thisCropIDrotation <-
            filter(
              varCropsPlantingMonthYearFieldRotation,
              Crop == thisCrop &
                Field == thisField & Year == thisYear & PlantingMonth == thisMonth
            )
          
          if (nrow(thisCropIDrotation) != 0) {
            thisCropIDrotation <- filter(
              varCropsPlantingMonthYearFieldRotation,
              Crop == thisCrop &
                Field == thisField &
                Year == thisYear &
                PlantingMonth == thisMonth
            ) %>%
              select(
                varID,
                Crop,
                Family,
                Year,
                PlantingMonth,
                MonthsInField,
                MonthsBetweenPlantings
              ) %>%
              mutate(Counter = MonthsInField + MonthsBetweenPlantings)
            thisBotanicalFamily <- thisCropIDrotation$Family
            
            # Are there any other plants in the same family that can be planted in the same month and year?
            thisCropFamilyRotation <-
              filter(
                varCropsPlantingMonthYearFieldRotation,
                Family == thisBotanicalFamily &
                  Year == thisYear &
                  PlantingMonth == thisMonth
              ) %>%
              select(
                varID,
                Crop,
                Family,
                Year,
                PlantingMonth,
                Field,
                MonthsInField,
                MonthsBetweenPlantings
              ) %>%
              mutate(Counter = MonthsInField + MonthsBetweenPlantings) 
            # Calculate the FamilyCounter as the interval between plantings for the crop
            # with the longest interval in this family
            thisFamilyCounter <- thisCropFamilyRotation %>% 
              group_by(Family, Year, PlantingMonth) %>% 
              summarise(FamilyCounter = max(Counter))
            # thisCropFamilyRotation <- 
            #   left_join(thisCropFamilyRotation, thisFamilyCounter, by = c("Family", "Year", "PlantingMonth"))
            
            for (i in seq(1,thisFamilyCounter$FamilyCounter)) {
              
              relaxedRotationcounter <- relaxedRotationcounter + 1
              
              relaxedRotationVarID <-
                paste("rr_",
                      as.character(relaxedRotationcounter),
                      sep = "")
              
              for (i in 1:thisCropIDrotation$Counter) {
                thisRow <-
                  paste("c_",
                        thisCropIDrotation$varID,
                        "_",
                        as.character(i),
                        sep = "")
                
                dfMatrix3[thisRow, thisCropIDrotation$varID] <-
                  "- 1"
                dfMatrix3[thisRow, relaxedRotationVarID] <- "+ 1"
                
                theseColumns <-
                  filter(
                    varCropsPlantingMonthYearFieldRotation,
                    Field == thisField &
                      Family == thisBotanicalFamily &
                      Crop != thisCrop &
                      Year == thisYear &
                      PlantingMonth == thisMonth
                  ) %>%
                  select(varID)
                
                for (j in 1:othersInFamily) {
                  dfMatrix3[thisRow, theseColumns[j, "varID"]] <- "+ 1"
                }
              }
              
            }
            
            
            
            # print(paste("Field: ", thisField,
            #             "; Botanical Family: ", thisBotanicalFamily,
            #             "; Crop: ", thisCrop,
            #             "; Year: ", as.character(thisYear),
            #             "; Month: ", thisMonth,
            #             sep = ""))
          }
          
        }
      }
    }
  }
  
}

# Write it temporarily here ####################################################
write.csv(dfMatrix, file = paste(dataDir, "/dfMatrix.csv", sep = ""))
write.csv(dfMatrix2, file = paste(dataDir, "/dfMatrix2.csv", sep = ""))
write.csv(dfMatrix3, file = paste(dataDir, "/dfMatrix3.csv", sep = ""))


# BUILD CPLEX-LP FORMATTED MILP FILE ###########################################
# BUILD VARIABLES ##############################################################
# Add Variables related to Crops Planted: varCropsPlantingMonthYearFieldFull$varID
binary = c(binary, paste(' ', varCropsPlantingMonthYearFieldFull$varID, sep = ''))

# Add variables related to fields not planted: varUnusedFieldYearMonth$varID
binary = c(binary, paste(" ", varUnusedFieldYearMonth$varID, sep = ""))

# Add variables related to unmet demand: varUnmetDemandProductYear$varID
general <- c(general, paste(" ", varUnmetDemandProductYear$varID, sep = ""))

# BUILD OBJECTIVE FUNCTION
Zrow <- paste(' + ', as.character(penaltyForUnmetDemand), ' ', varUnmetDemandProductYear$varID, sep = '')
ZrowSingle <- paste(Zrow, collapse = "")
objective <- c(objective, ZrowSingle)


# BUILD LAND TRANSFERS CONSTRAINTS #############################################
for (i in 1:nrow(dfMatrix)) {
  thisConstraint <- paste(rownames(dfMatrix)[i], ": ", sep = "")
  thiConstraintSingle <- ""
  for (j in 1:ncol(dfMatrix)) {
    thisColumn <- colnames(dfMatrix)[j]
    if (thisColumn == "RHS") {
      thisRHS <- dfMatrix[i, j]
    } else if (thisColumn == "Sense") {
      thisSense <- dfMatrix[i, j]
    } else {
      if (dfMatrix[i, j] != "0") {
        thisConstraint <-
          paste(thisConstraint, dfMatrix[i, j], " ", thisColumn, " ", sep = "")
      }
    }
  }
  thisConstraint <-
    paste(" ", thisConstraint, " ", thisSense, " ", thisRHS, sep = "")
  thisConstraintSingle <- paste(thisConstraint, collapse = "")
  constraintsLand <- c(constraintsLand, thisConstraintSingle)
}


# BUILD DEMAND CONSTRAINTS #####################################################
for (i in 1:nrow(dfMatrix2)) {
  thisConstraint <- paste(rownames(dfMatrix2)[i], ": ", sep = "")
  thiConstraintSingle <- ""
  for (j in 1:ncol(dfMatrix2)) {
    thisColumn <- colnames(dfMatrix2)[j]
    if (thisColumn == "RHS") {
      thisRHS <- dfMatrix2[i, j]
    } else if (thisColumn == "Sense") {
      thisSense <- dfMatrix2[i, j]
    } else {
      if (dfMatrix2[i, j] != "0") {
        thisConstraint <-
          paste(thisConstraint, dfMatrix2[i, j], " ", thisColumn, " ", sep = "")
      }
    }
  }
  thisConstraint <-
    paste(" ", thisConstraint, " ", thisSense, " ", thisRHS, sep = "")
  thisConstraintSingle <- paste(thisConstraint, collapse = "")
  constraintsDemand <- c(constraintsDemand, thisConstraintSingle)
}


# WRITE TO FILE ################################################################
# Write the title with overwrite
write(title, file = filePath, ncolumns = 1, append = FALSE)

# Append the Objective function
write(objective, file = filePath, ncolumns = 1, append = TRUE)

# Append constraints header
constraints <- c(constraints, constraintsLand, constraintsDemand)
write(constraints, file = filePath, ncolumns = 1, append = TRUE)

# Append Binary Variables
write(binary, file = filePath, ncolumns = 1, append = TRUE)

# Append Continuous/general Variables
write(general, file = filePath, ncolumns = 1, append = TRUE)

# Append terminator
write(c("", "end", ""), file = filePath, ncolumns = 1, append = TRUE)

# SOLVER INVOCATION ############################################################
glpkData <- Rglpk_read_file(filePath, type = "CPLEX_LP")
glpkResults <- Rglpk_solve_LP(glpkData$objective, 
                              glpkData$constraints[[1]], 
                              glpkData$constraints[[2]], 
                              glpkData$constraints[[3]],
                              bounds = NULL,
                              types = glpkData$types,
                              max = glpkData$maximum)
# Examine the results
if (glpkResults$status != 0) {
  print("Optimal solution NOT FOUND")
} else {
  glpkResults$optimum
}

# attr(x = glpkData, which = 'names')
vars <- attributes(x = glpkData)$objective_vars_names
values <- glpkResults$solution
results <- data.frame(vars, values)
resultsNonZero <- results %>% filter(values != 0)
# print(resultsNonZero)
resultsCrops <- 
filter(resultsNonZero, substr(vars,1,4) == "cpmy") %>%
  left_join(varCropsPlantingMonthYearFieldFull, by = c("vars" = "varID")) %>%
  select(Year, Field, Crop, PlantingMonth, "Release Field Month", vars) %>%
  arrange(Year, match(PlantingMonth, month.abb), Field, Crop) %>%
  rename('Plant On' = PlantingMonth, 'Harvest On' = 'Release Field Month')
resultsField <- 
  filter(resultsNonZero, substr(vars,1,4) == "cpmy") %>%
  left_join(varCropsPlantingMonthYearFieldFull, by = c("vars" = "varID")) %>%
  select(Year, Field, Crop, PlantingMonth, "Release Field Month", vars) %>%
  arrange(Year, Field, match(PlantingMonth, month.abb), Crop) %>%
  rename('Plant On' = PlantingMonth, 'Harvest On' = 'Release Field Month')


