# DO Daily Average Calculation
# calculates DO average for previous day(s) from continuous sonde and appends to output text file
# questions: seperry83@gmail.com

# import packages
suppressMessages(library(R.utils))

# --- Read in Data ---
# filepath for DO data (ie. txt file to write data to)
# assumes connection to S: drive
fp_DOdata <- 'S:/M & A BRANCH/Discrete EMP/Code/DO-average-calc/DO_Check.txt'

# define sonde fp (shouldn't change)
fp_sonde <- 'S:/OMBD/RRI.TXT'

# determine how far back to read file (1 day = ~100 lines) 
keepLines <- 150000 # lines
numLines <- R.utils::countLines(fp_sonde)

# import sonde data
sondeData <- read.csv(fp_sonde,
                      header = FALSE,
                      skip = numLines - keepLines)

# import DO Data (to determine which dates avg needs to be calc'd for)
doData <- read.csv(fp_DOdata,
                   header = FALSE,
                   sep = '\t',
                   skip = R.utils::countLines(fp_DOdata) - 1)

# --- Define date range ---
# define yesterday's date
yesterday <- as.Date(Sys.Date()-1)

# define last day avg was calc'd
lastDay <- as.Date(tail(doData,1)$V1)

# create vector consisting of dates to calc avg for
if(yesterday == lastDay) {
  {stop('DO values are up to date')}
} else {
  allDates = as.character(seq(lastDay+1, yesterday, 'days'))
}

# --- Populate Avg df ---
# create empty lists for relevant values
dayVals <- vector('list',length(allDates))
doLvl_1m <- vector('list',length(allDates))
criteria_1m <- vector('list',length(allDates))
doLvl_3m <- vector('list',length(allDates))
criteria_3m <- vector('list',length(allDates))
doLvl_6m <- vector('list',length(allDates))
criteria_6m <- vector('list',length(allDates))

df_list <- list()

# looping over the days
for (iDay in 1:length(allDates)) {
  
  # extract relevant values and append to dayDates 
  for(r in 1:nrow(sondeData)) {
    # convert NaN to NA  
    dayVals[[iDay]][is.nan(dayVals[[iDay]])] <- NA
    dayVals[[iDay]][R.utils::isZero(dayVals[[iDay]])] <- NA
  }S
  
  # set threshold
  month <- lubridate::month(allDates[[iDay]])
  high_val_months <- c(9, 10, 11) # months
  threshold <- ifelse(month %in% high_val_months, 6.5, 5.5) # mg/L
  
  # calculate average of dayDates vector (ignoring NA)
  doLvl_1m[[iDay]] = round(mean(sondeData$V10[sondeData$V2 == allDates[iDay]], na.rm=TRUE), 2) #avg of 1m
  doLvl_3m[[iDay]] = round(mean(sondeData$V11[sondeData$V2 == allDates[iDay]], na.rm=TRUE), 2) #avg of 3m
  doLvl_6m[[iDay]] = round(mean(sondeData$V12[sondeData$V2 == allDates[iDay]], na.rm=TRUE), 2) #avg of 6m
  
  # determine pass/fail for DO check
  criteria_1m[[iDay]] = ifelse(doLvl_1m[[iDay]] >= threshold, 'Passed', 'Failed')
  criteria_3m[[iDay]] = ifelse(doLvl_3m[[iDay]] >= threshold, 'Passed', 'Failed')
  criteria_6m[[iDay]] = ifelse(doLvl_6m[[iDay]] >= threshold, 'Passed', 'Failed')
  
  # create the df
  df_list[[iDay]] <- data.frame(date = allDates[[iDay]],
                         DO_1m = doLvl_1m[[iDay]],
                         thres_1m = threshold,
                         passed_1m = criteria_1m[[iDay]],
                         DO_3m = doLvl_3m[[iDay]],
                         thres_3m = threshold,
                         passed_3m = criteria_3m[[iDay]],
                         DO_6m = doLvl_6m[[iDay]],
                         thres_6m = threshold,
                         passed_6m = criteria_6m[[iDay]])
}

outputDf <- do.call(rbind, df_list)

# --- Export Data ---
# #append value to output text file
write.table(outputDf,
            fp_DOdata,
            append = TRUE,
            sep = '\t',
            col.names = FALSE,
            row.names = FALSE,
            quote = FALSE)