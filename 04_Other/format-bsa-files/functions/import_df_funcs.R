# ----
# import sheet
# ----
import_sheet <- function(wkbk, sheetName){
  # set up list of column types for extracting data
  dfTypes <-c('guess','skip','skip','skip','skip','skip','skip','skip','numeric','skip','numeric')
  
  # import left side of data
  dfOne <- suppressMessages(
    read_excel(
      wkbk,
      sheet = sheetName,
      range = 'A20:K99',
      col_types = dfTypes,
      col_names = F
    )
  )
  
  # import right side of data
  dfTwo <- suppressMessages(
    read_excel(
      wkbk
      ,sheet = sheetName
      ,range= 'Q20:AA99'
      ,col_types = dfTypes
      ,col_names = F
    )
  )
  
  # combine the two data sets
  dfAll <- rbind(dfOne,dfTwo)
  
  # Clean Up Data
  # rename columns
  names(dfAll) <- c('taxon','count','subsample')
  
  # skip sheet if no data
  if (sum(dfAll$count, na.rm = TRUE) == 0) {
    next
  }
  
  # remove blank rows
  dfAbund <- janitor::remove_empty(dfAll, which = 'rows')
  
  return(dfAbund)
}

# ----
# populate category col
# ----
pop_cat_col <- function(dfAbund) {
  # create a list of the categories
  catList <- subset(dfAbund$taxon, dfAbund$taxon == toupper(dfAbund$taxon))
  
  # set starting values for the for loop
  catCount <- 1
  start <- 1
  catVal <- 2
  catVec <- c()
  
  # populate category vector
  for (x in seq(start, length(dfAbund$taxon))) {
    catCount <- catCount + 1
    if (catVal <= length(catList)-1) {
      if (dfAbund$taxon[x] != catList[catVal]) {
        catVec <- c(catVec,catList[catVal-1])
        
      } else {
        catVec <- c(catVec,catList[catVal])
        start <- catCount
        catVal = catVal + 1
      }
    } else {
      catVec <- c(catVec,catList[catVal])
    }
  }
  
  # append to df
  dfAbund$category <- catVec
  
  # change Harpacticoids to lowercase
  for (x in seq(1, length(dfAbund$taxon))) {
    if (dfAbund$taxon[x] == 'HARPACTICOIDS') {
      dfAbund$taxon[x] = 'Harpacticoids'
    }
  }
  
  # remove category rows
  upperCols <- dfAbund$taxon == toupper(dfAbund$taxon)
  dfAbund <- dfAbund[!upperCols,]
  
  # change NAs to 0s
  dfAbund$count[is.na(dfAbund$count)] <- 0
  dfAbund$subsample[is.na(dfAbund$subsample)] <- 0
  
  return(dfAbund)
}