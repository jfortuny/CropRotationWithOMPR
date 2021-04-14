library(ompr)
library(ompr.roi)

# Start MILP definition (this will move to another source file)
m = MILPModel() %>%
  # Add Variables related to Crops Planted: varCropsPlantingMonthYearFieldFull$CropMonthYearField
  add_variable(
    crop[i],
    type = 'binary',
    i = 1:nrow(varCropsPlantingMonthYearFieldFull)
  ) %>%
  # Add variables related to fields not planted: varUnusedFieldYearMonth$FieldYearMonth
  add_variable(
    varUnusedFieldYearMonth[i],
    type = 'binary',
    i = 1:nrow(varUnusedFieldYearMonth)
  ) %>%
  # Add variables related to unmet demand: varUnmetDemandProductYear$UnmetDemandProductYear
  add_variable(
    unmetDemand[i],
    type = 'continuous',
    i = 1:nrow(varUnmetDemandProductYear)
  ) %>%
  # Set objective function
  set_objective(sum_expr(
    penaltyForUnmetDemand * unmetDemand[i],
    i = 1:nrow(varUnmetDemandProductYear)
  ),
  sense = 'min') %>%
  ###################################################################################################################
# Constraints related to field transfers and allocations to crops for year 1 month 1:
# varCropsPlantedInFirstMonthFirstYear$CropMonthYearField and varFieldsUnusedInFirstMonthFirstYear$FieldYearMonth
add_constraint(sum_expr(1 * crop[p],  )
               
)
# Constraints related to field transfers and allocations to crops for year 1 month 2 to year 4 month 12
add_constraint()