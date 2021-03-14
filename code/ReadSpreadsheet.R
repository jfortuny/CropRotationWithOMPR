library (readxl)

# Define path to sample template spreadsheet
baseDir <- getwd()
dataDir <- paste(baseDir, '/data', sep = '')
filePath <- file.path(paste(dataDir, '/Crop Rotation Sample Template.xlsx', sep = ''))

# List the sheets/tabs in the sample template spreadsheet
tryCatch(sheets <- excel_sheets(filePath),
         error = function(e){
           message('The spreadsheet does not exist')
           quit(save = 'no')
         },
         warning = function(w){
           message(paste('Warning:', w))
           quit(save = 'no')
         }
)

# Read Farmland sheet; will throw error it the sheet doesn't exist
tryCatch(farmland <- read_excel(filePath, sheet = 'Farmland'),
         error = function(e){
           message('There is no Farmland tab in the spreadsheet')
           quit(save = 'no')
         },
         warning = function(w){
           message(paste('Warning:', w))
           quit(save = 'no')
         }
         )
# Keep only included fields
farmlandIncluded <- subset(as.data.frame(farmland), farmland$'Include?' == 'Y')

