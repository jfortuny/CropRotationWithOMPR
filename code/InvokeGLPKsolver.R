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
constraints = c('', 'Subject to')
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
binary = c(binary, paste(" ", varUnusedFieldYearMonth$varID, sep = ""))

# Add variables related to unmet demand: varUnmetDemandProductYear$varID
general <- c(general, paste(" ", varUnmetDemandProductYear$varID, sep = ""))

# BUILD OBJECTIVE FUNCTION
Zrow <- paste(' + ', as.character(penaltyForUnmetDemand), ' ', varUnmetDemandProductYear$varID, sep = '')
ZrowSingle <- paste(Zrow, collapse = "")
objective <- c(objective, ZrowSingle)

# BUILD CONSTRAINTS
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
  dfMatrix[j, "Sense"] <- "="
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
  dfMatrix[dfPredecessors[i, "constraint"], dfPredecessors[i, "varID.x"]] <- "- 1"
  dfMatrix[dfPredecessors[i, "constraint"], dfPredecessors[i, "varID.y"]] <- "+ 1"
  dfMatrix[dfPredecessors[i, "constraint"], "RHS"] <- '0'
  dfMatrix[dfPredecessors[i, "constraint"], "Sense"] <- "="
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
  dfMatrix[dfPredecessors[i, "constraint"], dfPredecessors[i, "varID.x"]] <- "- 1"
  dfMatrix[dfPredecessors[i, "constraint"], dfPredecessors[i, "varID.y"]] <- "+ 1"
  dfMatrix[dfPredecessors[i, "constraint"], "RHS"] <- '0'
  dfMatrix[dfPredecessors[i, "constraint"], "Sense"] <- "="
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

# Write it temporarily here ####################################################
# write.csv(dfMatrix, file = paste(dataDir, "/dfMatrix.csv", sep = ""))


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
# CODE HERE!


# ##############################################################################################################################
# # Crops Planted this period
# cp <-
#   varCropsPlantingMonthYearFieldFull %>% 
#   mutate(YearMonth = paste(varCropsPlantingMonthYearFieldFull$Year, 
#                            "-", 
#                            varCropsPlantingMonthYearFieldFull$PlantingMonth, 
#                            sep = "")) %>%
#   select(YearMonth, varID) %>%
#   mutate(cp_varID = varID) %>%
#   select(YearMonth, cp_varID)
# # Fields Unused this period
# fu <-
#   varUnusedFieldYearMonth %>% mutate(YearMonth = paste(
#     varUnusedFieldYearMonth$Y,
#     "-",
#     varUnusedFieldYearMonth$Month,
#     sep = ""
#   )) %>%
#   select(YearMonth, FieldYearMonth, varID) %>%
#   mutate(fu_varID = varID) %>%
#   select(YearMonth, FieldYearMonth, fu_varID)
# 
# #left_join(fu, cp, by = "YearMonth")
# 
# # Fields unused last period (will be available this period)
# fulp <- varUnusedFieldYearMonth %>%
#   mutate(AvailableInMonthNumeric = match(varUnusedFieldYearMonth$Month, month.abb)+1
#          ) %>%
#   mutate(AvailableInYear =
#            if_else(AvailableInMonthNumeric != 13, Year, as.character(as.numeric(Year) + 1))) %>%
#   mutate(AvailableInMonthNumeric2 = if_else(AvailableInMonthNumeric==13,1,AvailableInMonthNumeric)
#          ) %>%
#   mutate(AvailableInMonth = month.abb[AvailableInMonthNumeric2]) %>%
#   mutate(AvailableInYearMonth = paste(AvailableInYear,"-",AvailableInMonth,sep = "")) %>%
#   mutate(YearMonth = paste(Year, "-", Month, sep = "")) %>%
#   filter(AvailableInYear!="5") %>%
#   mutate(fulp_varID = varID) %>% 
#   select(YearMonth, AvailableInYearMonth, fulp_varID)
# 
# # left_join(fu, fulp, by = "YearMonth")
# 
# # Fields Freed by crops this period
# ff <- varCropsPlantingMonthYearFieldFull %>%
#   mutate(
#     YearMonth = paste(
#       varCropsPlantingMonthYearFieldFull$Year,
#       "-",
#       varCropsPlantingMonthYearFieldFull$PlantingMonth,
#       sep = ''
#     )
#   ) %>%
#   mutate(ReleaseYear =
#            if_else(SameYearRelease, Year,
#                    as.character(as.numeric(Year) + 1))) %>%
#   mutate(
#         '-',
#     ReleaseYearMonth =
#       paste(
#         ReleaseYear,
#         varCropsPlantingMonthYearFieldFull$`Release Field Month`
#         ,
#         sep = ''
#       )
#   ) %>%
#   select(YearMonth, ReleaseYearMonth, varID) %>%
#   mutate(ff_varID = varID) %>%
#   select(YearMonth, ReleaseYearMonth, ff_varID)
# 
# # left_join(fu, ff, by = "YearMonth")
# landTransfers <- left_join(fu, fulp, by = c("YearMonth", "fu_varID" = "fulp_varID"))
# landTransfers <-left_join(landTransfers, cp, by = "YearMonth")
# 
#   left_join(ff, by = "YearMonth")
#

##############################################################################################################################
# WRITE TO FILE
# Write the title with overwrite
write(title, file = filePath, ncolumns = 1, append = FALSE)

# Append the Objective function
write(objective, file = filePath, ncolumns = 1, append = TRUE)

# Append constraints header
constraints <- c(constraints, constraintsLand)
write(constraints, file = filePath, ncolumns = 1, append = TRUE)

# Append Binary Variables
write(binary, file = filePath, ncolumns = 1, append = TRUE)

# Append Continuous/general Variables
write(general, file = filePath, ncolumns = 1, append = TRUE)


