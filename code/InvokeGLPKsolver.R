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
filePath <- file.path(paste(dataDir, '/', fileName, '.lp', sep = ''))

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

# Timing the code
endTime <- Sys.time()
lapsedTime <- difftime(startTime,  endTime, units = "secs")
print(paste("Build Land Transfer Constraints Data: ", as.character(lapsedTime), sep=''))
startTime <- Sys.time()

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
nonCoverCrops <- varCropsPlantingMonthYearFieldRotation %>%
  filter('Is Cover Crop?' != 'Y')
for (i in 1:nrow(nonCoverCrops)) {
  thisColumn <- nonCoverCrops[i, 'varID']
  thisCrop <- nonCoverCrops[i, 'Crop']
  thisCoefficient <- nonCoverCrops[i, 'YieldPerUnitOfField'] *
    nonCoverCrops[i, 'Available']
  thisCoefficient <- paste("+ ", thisCoefficient, sep = "")
  if (nonCoverCrops[i, 'SameYearRelease']) {
    thisYear <- nonCoverCrops[i, 'Year']
  } else {
    thisYear <- as.character(as.numeric(nonCoverCrops[i, 'Year']) + 1)
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

# Timing the code
endTime <- Sys.time()
lapsedTime <- difftime(startTime,  endTime, units = "secs")
print(paste("Build Unmet Demand Constraints Data: ", as.character(lapsedTime), sep=''))
startTime <- Sys.time()

# BUILD ROTATION CONSTRAINTS ###################################################
# Build empty dataframe for matrix #############################################
bigM <- 1
dfColNames3 <- c(varCropsPlantingMonthYearFieldRotation$varID,
                 varRotationDeltaFieldFamilyYearMonth$varID, 
                 varRotationRelaxedFieldFamilyYearMonth$varID)

# Add column for the RHS
dfMatrix3 <- data.frame(matrix(0, nrow = 0, ncol = 1 + length(dfColNames3)), stringsAsFactors = FALSE)
colnames(dfMatrix3) <- c(dfColNames3, "RHS")

# Write console log to file
# logFilePath <- file.path(paste(codeDir, '/consoleInvokeGLPKsolver.txt', sep = ''))
# write("Console log starts...", file = logFilePath, ncolumns = 1, append = FALSE)


# RECODING FROM HERE TO SIMPLIFY THE LOOPING AND THE THINKING ##################
thisField <-
  thisYear <-
  thisMonth <-
  thisCrop <- thisCoefficient <- thisColumn <- thisRow <- NULL
for (i_f in fieldsIncluded$Field) {
  thisField <- i_f
  # write(paste("In row 177, thisField = ", thisField, sep=""), file = logFilePath, ncolumns = 1, append = TRUE)
  #print(paste("In row 173, thisField = ", thisField, sep=""))
  for (i_y in y) {
    thisYear <- i_y
    for (i_m in m) {
      thisMonth <- i_m
      
      # Deal with crops other than cover crops
      for (i_c in cropsIncluded$Crop) {
        thisCrop <- i_c
        # write(paste("In row 189, thisCrop = ", thisCrop, "; thisField = ", thisField, "; thisYear = ", as.character(thisYear), "; thisMonth = ",
        #             as.character(thisMonth), sep=""), file = logFilePath, ncolumns = 1, append = TRUE)
        # print(paste("In row 184, thisCrop = ", thisCrop, " thisField = ", thisField, "thisYear = ", as.character(thisYear), "thisMonth = ",
        #          as.character(thisMonth), sep=""))
        if (cropsIncluded[cropsIncluded$Crop == thisCrop, 'Is Cover Crop?'] == "Y") {
          # continue; cover crops do not have rotational constraints
        } else {
          thisCropIDrotation <-
            filter(
              varCropsPlantingMonthYearFieldRotation,
              Crop == thisCrop &
                Field == thisField &
                Year == thisYear & 
                PlantingMonth == thisMonth
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
            
            thisVarID <- thisCropIDrotation$varID
            thisBotanicalFamily <- thisCropIDrotation$Family
            #  print("In row 230")
           
              #print("in row 259")
            deltaRotationVarID <- varRotationDeltaFieldFamilyYearMonth %>%
              filter(cropVarID == thisVarID) %>%
              select(varID)
              # deltaRotationVarID <- 
              #   varRotationDeltaFieldFamilyYearMonth %>%
              #   filter(
              #     Field == thisField &
              #       Family == thisBotanicalFamily &
              #       Year == thisYear &
              #       Month == thisMonth
              #   ) %>%
              #   select(varID)
              
              # relaxedRotationVarID <-
              #   varRotationRelaxedFieldFamilyYearMonth %>%
              #   filter(
              #     Field == thisField &
              #       Family == thisBotanicalFamily &
              #       Year == thisYear &
              #       Month == thisMonth
              #   ) %>%
              #   select(varID)
              
              # Big M constraint
              thisRow <-
                paste(
                  "c_",
                  thisCropIDrotation$varID,
                  "_",
                  thisCropIDrotation$Year,
                  "_",
                  thisCropIDrotation$PlantingMonth,
                  "_1",
                  sep = ""
                )
              thisColumn <- thisVarID
              # write(paste("In row 317, thisRow = ", thisRow, "; thisColumn = ", thisColumn, " ir = ", as.character(ir), " jc = ",
              #             as.character(jC),sep=""), file = logFilePath, ncolumns = 1, append = TRUE)
              # print(paste("In row 307, thisRow = ", thisRow, " thisColumn = ", thisColumn, " ir = ", as.character(ir), " jc = ",
              #         as.character(jC),sep=""))
              dfMatrix3[thisRow, thisColumn] <- " +1"
              dfMatrix3[thisRow, deltaRotationVarID$varID] <-
                paste(" -", as.character(bigM), sep = "")
              # dfMatrix3[thisRow, relaxedRotationVarID$varID] <-
              #   " +1"
              dfMatrix3[thisRow, "Sense"] <- "<="
              dfMatrix3[thisRow, "RHS"] <- 0
              
              # Are there any other crops in the same family to plant in this field?
              thisCropFamilyRotation <-
                filter(
                  varCropsPlantingMonthYearFieldRotation,
                  Family == thisBotanicalFamily &
                    # Year == thisYear &
                    # PlantingMonth == thisMonth &
                    Field == thisField &
                    varID != thisVarID
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
              
              if (nrow(thisCropFamilyRotation) != 0) {
              # Rows counter for current crop thisVarID
              counterMonth <- 0
              counterYear <- 0
              for (ir in seq(2, thisCropIDrotation$Counter)) {
                thisRow <-
                  paste(
                    "c_",
                    thisCropIDrotation[1, "varID"],
                    "_",
                    thisCropIDrotation[1, "Year"],
                    "_",
                    thisCropIDrotation[1, "PlantingMonth"],
                    "_",
                    as.character(ir),
                    sep = ""
                  )
                # print(thisRow)
                # if (thisRow == "c_cpmy_8_1_May_14") {
                #   print("Error about to happen")
                #   }
                # write(paste("In row 292, thisRow = ", thisRow, "; ir = ", as.character(ir), sep=""), file = logFilePath, ncolumns = 1, append = TRUE)
                # print(paste("In row 284, thisRow = ", thisRow, " ir = ", as.character(ir), sep=""))
                if (counterMonth == 0) {
                  counterMonth <-
                    match(thisCropIDrotation$PlantingMonth,
                          month.abb)
                  counterYear <-
                    as.numeric(thisCropIDrotation$Year)
                } else {
                  counterMonth <- counterMonth + 1
                  if (counterMonth == 13) {
                    counterMonth <- 1
                    counterYear <- counterYear + 1
                  }
                }
                counterMonthAlpha <- month.abb[counterMonth]
                counterYearAlpha <- as.character(counterYear)
                
                #print("in row 300")
                # Remaining rows of the big M set
                for (jC in 1:nrow(thisCropFamilyRotation)) {
                  plantable <- filter(
                    varCropsPlantingMonthYearFieldRotation,
                    Family == thisBotanicalFamily,
                    # Crop == thisCropFamilyRotation[jC, "Crop"],
                      Field == thisField &
                      Year == counterYearAlpha &
                      PlantingMonth == counterMonthAlpha &
                      varID != thisVarID
                  )
                  if (nrow(plantable) != 0) {
                    for (jp in 1:nrow(plantable)) {
                    # thisColumn <- thisCropFamilyRotation[jC, "varID"]
                    thisColumn <- plantable[jp, "varID"]
                    # write(paste("In row 346, thisRow = ", thisRow, "; thisColumn = ", thisColumn, " ir = ", as.character(ir), " jc = ", as.character(jC), sep=""), file = logFilePath, ncolumns = 1, append = TRUE)
                    # print(paste("In row 336, thisRow = ", thisRow, " thisColumn = ", thisColumn, " ir = ", as.character(ir), " jc = ", as.character(jC), sep=""))
                    dfMatrix3[thisRow, thisColumn] <- " +1"
                    dfMatrix3[thisRow, deltaRotationVarID$varID] <-
                      paste(" +", as.character(bigM), sep = "")
                    # dfMatrix3[thisRow, relaxedRotationVarID$varID] <-
                    #   " +1"
                    dfMatrix3[thisRow, "Sense"] <- "<="
                    dfMatrix3[thisRow, "RHS"] <-
                      as.character(bigM)
                    }
                    }
                }
              }
            }
          }
        }
      }
    }
  }
}

# Timing the code
endTime <- Sys.time()
lapsedTime <- difftime(startTime,  endTime, units = "secs")
print(paste("Build Rotation Constraints Data: ", as.character(lapsedTime), sep=''))
startTime <- Sys.time()

# # Write it temporarily here ####################################################
# write.csv(dfMatrix, file = paste(dataDir, "/dfMatrix.csv", sep = ""))
# write.csv(dfMatrix2, file = paste(dataDir, "/dfMatrix2.csv", sep = ""))
# write.csv(dfMatrix3, file = paste(dataDir, "/dfMatrix3.csv", sep = ""))

# Timing the code
endTime <- Sys.time()
lapsedTime <- difftime(startTime,  endTime, units = "secs")
print(paste("Writing the DataFrames to Disk: ", as.character(lapsedTime), sep=''))
startTime <- Sys.time()

# BUILD CPLEX-LP FORMATTED MILP FILE ###########################################

# Create the stub for the problem sections
title = paste('\\* Crop Rotation: ', myTitle, ' ', as.character(lubridate::year(Sys.Date())), ' *\\', sep = '')
objective = c('', 'minimize', 'Z:')
constraints = c('', 'Subject to')
constraintsLand = ''
constraintsDemand = ''
constraintsRotation <- ""
bounds = ''
general = c('', 'general')
integer = c('', 'integer')
binary = c('', 'binary')


# BUILD VARIABLES ##############################################################
# Add Variables related to Crops Planted: varCropsPlantingMonthYearFieldFull$varID
binary = c(binary, paste(' ', varCropsPlantingMonthYearFieldFull$varID, sep = ''))

# Add variables related to fields not planted: varUnusedFieldYearMonth$varID
binary = c(binary, paste(" ", varUnusedFieldYearMonth$varID, sep = ""))

# Add delta variables related to crop rotation: varRotationDeltaFieldFamilyYearMonth$varID
binary = c(binary, paste(" ", varRotationDeltaFieldFamilyYearMonth$varID, sep = ""))

# Add variables related to unmet demand: varUnmetDemandProductYear$varID
general <- c(general, paste(" ", varUnmetDemandProductYear$varID, sep = ""))

# Add variables related to relaxed crop rotation: varRotationRelaxedFieldFamilyYearMonth$varID
# general <- c(general, paste(" ", varRotationRelaxedFieldFamilyYearMonth$varID, sep = ""))

# BUILD OBJECTIVE FUNCTION
Zrow1 <- paste(' + ', as.character(penaltyForUnmetDemand), ' ', varUnmetDemandProductYear$varID,
              sep = '')
# Zrow2 <- paste(' + ', as.character(penaltyForRelaxedRotation), ' ', varRotationRelaxedFieldFamilyYearMonth$varID,
#     sep = '')
Zrow2 <- ""
coverCrops <- select(varCropsPlantingMonthYearFieldRotation, "Is Cover Crop?", "varID") %>%
  rename("IsCover"  = "Is Cover Crop?") %>% filter(IsCover == "Y")
if (nrow(coverCrops) != 0) {
  Zrow3 <- paste(" - ", as.character(valueOfCoverCrop), " ", coverCrops$varID, sep = '')
} else {
  Zrow3 <- ""
}
Zrow <- c(Zrow1, Zrow2, Zrow3)
ZrowSingle <- paste(Zrow, collapse = "")
objective <- c(objective, ZrowSingle)

# Timing the code
endTime <- Sys.time()
lapsedTime <- difftime(startTime,  endTime, units = "secs")
print(paste("Build CPLEX Core Constraints: ", as.character(lapsedTime), sep=''))
startTime <- Sys.time()

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

# Timing the code
endTime <- Sys.time()
lapsedTime <- difftime(startTime,  endTime, units = "secs")
print(paste("Build Land Transfer Constraints: ", as.character(lapsedTime), sep=''))
startTime <- Sys.time()


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

# Timing the code
endTime <- Sys.time()
lapsedTime <- difftime(startTime,  endTime, units = "secs")
print(paste("Build Unmet Demand Constraints: ", as.character(lapsedTime), sep=''))
startTime <- Sys.time()

# BUILD ROTATION CONSTRAINTS ###################################################
for (i in 1:nrow(dfMatrix3)) {
  thisConstraint <- paste(rownames(dfMatrix3)[i], ": ", sep = "")
  thiConstraintSingle <- ""
  for (j in 1:ncol(dfMatrix3)) {
    thisColumn <- colnames(dfMatrix3)[j]
    if (thisColumn == "RHS") {
      thisRHS <- dfMatrix3[i, j]
    } else if (thisColumn == "Sense") {
      thisSense <- dfMatrix3[i, j]
    } else {
      if ( (!is.na(dfMatrix3[i, j])) & (dfMatrix3[i, j] != "0") ) {
        thisConstraint <-
          paste(thisConstraint, dfMatrix3[i, j], " ", thisColumn, " ", sep = "")
      }
    }
  }
  thisConstraint <-
    paste(" ", thisConstraint, " ", thisSense, " ", thisRHS, sep = "")
  thisConstraintSingle <- paste(thisConstraint, collapse = "")
  constraintsRotation <- c(constraintsRotation, thisConstraintSingle)
}

# Timing the code
endTime <- Sys.time()
lapsedTime <- difftime(startTime,  endTime, units = "secs")
print(paste("Build Rotation Constraints: ", as.character(lapsedTime), sep=''))
startTime <- Sys.time()


# WRITE TO FILE ################################################################
# Write the title with overwrite
write(title, file = filePath, ncolumns = 1, append = FALSE)

# Append the Objective function
write(objective, file = filePath, ncolumns = 1, append = TRUE)

# Append constraints header
constraints <- c(constraints, constraintsLand, constraintsDemand, constraintsRotation)
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


