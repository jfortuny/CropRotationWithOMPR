library(dplyr)
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
# need to worry about here are the "Crops Planted" variables (cmpy_i) both as they
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
    dfMatrix2[thisRow, thisColumn] <- thisCoefficient
  } else {
    next
  }
}

# Write it temporarily here ####################################################
write.csv(dfMatrix, file = paste(dataDir, "/dfMatrix.csv", sep = ""))
write.csv(dfMatrix2, file = paste(dataDir, "/dfMatrix2.csv", sep = ""))

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
write(c("END", ""), file = filePath, ncolumns = 1, append = TRUE)
