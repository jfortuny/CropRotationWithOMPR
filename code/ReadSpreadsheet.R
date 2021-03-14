library (readxl)

# Define path to sample template spreadsheet
baseDir <- getwd()
dataDir <- paste(baseDir, '/data', sep = '')
filePath <- file.path(paste(dataDir, '/Crop Rotation Sample Template.xlsx', sep = ''))

# List the sheets/tabs in the sample template spreadsheet
sheets <- excel_sheets(filePath)

# Read Farmland sheet; will throw error it the sheet doesn't exist
farmland <- read_excel(filePath, sheet = 'Farmland')
# Keep only included fields
farmlandIncluded <- subset(as.data.frame(farmland), farmland$'Include?' == 'Y')

