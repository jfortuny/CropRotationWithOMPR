library(dplyr)
# In this script we create the data in the CPLEX LP format, we store the data in a file for review
# via lpsolve and visually and then invoke the solver (after reading the lp file again - a bit kludgy)

# Create the stub for the problem sections
title = c('\\* Crop Rotation', as.character(lubridate::year(Sys.Date())), '*\\')
objective = 'minimize \nZ: '  # use cat to print the newline character
constraintsLand = ''
constraintsDemand = ''
bounds = ''
general = ''
integer = ''
binary = ''

cat(title)
