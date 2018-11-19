## ProcessData.R
## Updates physical indicator data for OCN GAM ensemble forecasts.
## This file is run from the script DowloadPhysicalVars.R
##   Author:  Tom Wainwright, thomas.wainwright@noaa.gov
##   Created:  13 July 2010
##   Last Modified: 22 Apr 2011
##                  23 Oct 2012 - Converted from Sweave to plain R
##                  24 Jan 2014 - Revised for OCN forecast data sets
##     After raw data files are downloaded by the 'UpdateData' script,
##     this script reads them individually into R, then uses them to create
##     standard monthly and/or annual indicator data series.
##     

##     DATA SET DEFINITIONS
    
##     First, define the datasets. Each dataset is defined by a structure 
##     with several named elements, some of which are optional:
##     
##          Name:  (char) Short name (used for constructing file names, etc.).
##          Title:  (char) Long name (used for labeling graphs and tables).
##          URL:  (char) URL for web-based data (omit for local files).
##          SrcName:  (char) Name of local source data structure.
##          Units:  (char/expression) Units for data, used for axis labels 
##            (may be an R expression).
##          Freq:  (char) Data summary frequency: 'Annual','Monthly','Irregular'
##          SourceTag: (char) Data source acknowledgement label for graphs 
##            and tables.
    
dataSetDefs <- list(PDO=list(Name='PDO',
                      Title='Pacific Decadal Oscillation',
                      Units='Std. Dev.',
                      Freq='Monthly',
                      SourceTag='Nathan Mantua, JISAO, U. Washington',
                      SrcName='PDO'),
                    NPGO=list(Name='NPGO',
                      Title='North Pacific Gyre Oscillation',
                      Units='Std. Dev.',
                      Freq='Monthly',
                      SourceTag='www.o3d.org/npgo',
                      SrcName='NPGO'),
                    # NPI=list(Name='NPI',
                    #   Title='North Pacific Index',
                    #   Units='hPa - 1000',
                    #   Freq='Monthly',
                    #   SourceTag='Jim Hurrell, NCAR',
                    #   SrcName='NPI'),
                    # MEI=list(Name='MEI',
                    #   Title='Multivariate ENSO Index',
                    #   Units='Std. Dev.',
                    #   Freq='Monthly',
                    #   SourceTag='Klaus Wolter, NOAA/ESRL',
                    #   SrcName='MEI'),
                    ONI=list(Name='ONI',
                      #Title=expression(Oceanic ~~ Ni * tilde(n) * o ~~ Index),
                      Title='Oceanic Niño Index',
                      Units=expression(degree * C),
                      Freq='Monthly',
                      SourceTag='NOAA Climate Prediction Center',
                      SrcName='ONI'),
                    # SLP.45N=list(Name='SLP.45N',
                    #   Title='Sea Level Pressure at 45N, 125W',
                    #   Units='mb',
                    #   Freq='Monthly',
                    #   SourceTag='NOAA NCEP/NCAR Reanalysis I',
                    #   SrcName='SLP'),
                    # ASL.45N=list(Name='ASL.45N',
                    #   Title='Adjusted Sea Level Height at South Beach',
                    #   Units='mm',
                    #   Freq='Monthly',
                    #   SourceTag='GLOSS/CLIVAR, Univ. of Hawaii',
                    #   SrcName='SLHraw'),
                    SST.46050=list(Name='SST.46050',
                      Title='Sea Surface Temperature, Buoy 46050',
                      Units=expression(degree * C),
                      Freq='Monthly',
                      SourceTag='NOAA National Data Buoy Center',
                      SrcName='SST.46050'),
                    CWT=list(Name='CWT',
                      Title='Coastal Water Temperature, Charleston',
                      Units=expression(degree * C),
                      Freq='Monthly',
                      SourceTag='NOAA National Ocean Service',
                      SrcName='CWT'),
                    # BFL=list(Name='BFL',
                    #   Title='Columbia River Discharge at Bonneville',
                    #   Units='Kcfs',
                    #   Freq='Monthly',
                    #   SourceTag='USACE Columbia Basin Water Management Div.',
                    #   SrcName='BFL'),
                    UWI=list(Name='UWI',
                      Title='Coastal Upwelling Index',
                      Units=expression(m^3 * s^{-1} * (100*m)^{-1}),
                      Freq='Monthly',
                      SourceTag='NOAA/SWFSC Environmental Research Division',
                      SrcName='UWI'),
                    SPT.LGR=list(Name='SPT.LGR',
                      Title='Spring Transition (Logerwell)',
                      Units='Day of Year',
                      Freq='Annual',
                      SourceTag='www.cbr.washington.edu',
                      SrcName='SPR.TRANS'))

##     PROCESS DATASETS

##     Finally, process the raw data to form standard monthly or annual 
##     tables, and write them to local files for future use.  Also create a 
##     flat date & data structure for time series plots:
    
##          PDO (Pacific Decadal Oscillation)
##            is already a monthly table, so not much to do:

cat(' * Processing PDO *\n')
.dsd <- dataSetDefs$PDO
.raw <- get(paste(.dsd$SrcName, '.raw', sep=''))
PDO.mon <- .raw
.data <- as.vector(t(.raw[ , -1]))
.decyr <- min(.raw$Year) + (1:length(.data) - 0.5)/12
PDO.ts <- data.frame(DecYr=.decyr, Data=.data) 

##          NPGO (North Pacific Gyre Oscillation)
##            file has three numeric columns:  Year, month, and 
##            the index:

cat(' * Processing NPGO *\n')
.dsd <- dataSetDefs$NPGO
.raw <- get(paste(.dsd$SrcName, '.raw', sep=''))
NPGO.mon <- cbind(sort(unique(.raw$Year)), 
                  tapply(.raw$NPGO, 
                        list(Year=.raw$Year, Mon=.raw$Mon), 
                        mean, na.rm=T))
colnames(NPGO.mon) <- c('Year', month.abb)
.data <- .raw$NPGO
.decyr <- min(.raw$Year) + (1:length(.data) - 0.5)/12
NPGO.ts <-  data.frame(DecYr=.decyr, Data=.data)

##          NPI (North Pacific Index)
##            file has two numeric columns:  Year/Month (YYYYMM) , and 
##            the index:

# cat(' * Processing NPI *\n')
# .dsd <- dataSetDefs$NPI
# .raw <- get(paste(.dsd$SrcName, '.raw', sep=''))
# # Recode missing values (-99)
# .raw$NPI[.raw$NPI < -90] <- NA
# .yr <- floor(.raw$YYYYMM/100)
# .mo <- round(.raw$YYYYMM - .yr*100)
# NPI.mon <- cbind(sort(unique(.yr)), 
#                   tapply(.raw$NPI, 
#                         list(Year=.yr, Mon=.mo), 
#                         mean, na.rm=T))
# colnames(NPI.mon) <- c('Year', month.abb)
# .data <- .raw$NPI
# .decyr <- min(.yr) + (1:length(.data) - 0.5)/12
# NPI.ts <-  data.frame(DecYr=.decyr, Data=.data)
# rm(.yr, .mo)

##          MEI (Multivariate ENSO Index)
##            is already a monthly table, so not much to do:

# cat(' * Processing MEI *\n')
# .dsd <- dataSetDefs$MEI
# .raw <- get(paste(.dsd$SrcName, '.raw', sep=''))
# MEI.mon <- .raw
# .data <- as.vector(t(MEI.mon[ , -1]))
# .decyr <- min(MEI.mon$Year) + (1:length(.data) - 0.5)/12
# MEI.ts <- data.frame(DecYr=.decyr, Data=.data) 

##          SLP.45 (Sea Level Pressure @ 45 N)
##            file has four columns:  first is Date (YYYY-MM-DD),
##            rest are Sea-Level Pressure (mb) for latitudes
##            42.5, 45, 47.5 North:

# cat(' * Processing SLP.45 *\n')
# .dsd <- dataSetDefs$SLP.45N
# .raw <- get(paste(.dsd$SrcName, '.raw', sep=''))
# .yr <- as.numeric(format(as.Date(.raw$Date),'%Y'))
# .mon <- as.numeric(format(as.Date(.raw$Date),'%m'))
# SLP.45N.mon <- tapply(.raw[['lat45.0N']], list(Year=.yr, Mon=.mon), 
#                       mean, na.rm=T)
# SLP.45N.mon <- cbind(sort(unique(.yr)), SLP.45N.mon)
# dimnames(SLP.45N.mon) <- list(sort(unique(.yr)), c('Year',month.abb))
# .data <- .raw[['lat45.0N']]
# .decyr <- min(.yr) + (1:length(.data) - 0.5)/12
# SLP.45N.ts <-  data.frame(DecYr=.decyr, Data=.data)

##          ASL.45 (Adjusted Sea Level Height at South Beach)
##            is computed from raw SLH at South Beach with missing
##            data filled from Crescent City and Neah Bay data, and
##            adjusted based on sea level pressure. There are three
##            raw data tables, one for each location. Call a special
##            script to do this one.
# cat(' * Processing ASL.45 *\n')
# .dsd <- dataSetDefs$ASL.45N
# source('ProcessAdjustedSLH.R', echo=TRUE)

##          ONI (Oceanic Niño Index)
##            file has four columns:  first is three-character 
##            abbreviation for three-month moving average period (e.g. DJF 
##            for Dec-Jan-Feb average), second is 4-digit year, third is
##            total index, fourth is the anomaly:

cat(' * Processing ONI *\n')
.dsd <- dataSetDefs$ONI
.raw <- get(paste(.dsd$SrcName, '.raw', sep=''))
print(summary(.raw))  ###DEBUG###
ONI.mon <- cbind(sort(unique(as.numeric(.raw$Year))), 
                 matrix(c(as.numeric(.raw$ONI),
                          rep(NA, 12-length(.raw$ONI)%%12)), 
                        ncol=12, byrow=T))
colnames(ONI.mon) <- c('Year', month.abb)
.data <- as.numeric(.raw$ONI)
.decyr <- min(as.numeric(.raw$Year), na.rm=T) + (1:length(.data) - 0.5)/12
ONI.ts <-  data.frame(DecYr=.decyr, Data=.data)

##          SST.46050 (Sea Surface Temperature @ Stonewall Bank)
##            has a complex structure, with only one column of 
##            interest.  The number of data columns and the date format 
##            changed over time, missing value codes vary by column, 
##            and there is one bad data point that needs to be removed.
    
cat(' * Processing SST.46050 *\n')
.dsd <- dataSetDefs$SST.46050
# This takes a lot of computation, so only update it if the output
# monthly file is more than 5 days old.
# This saves a lot of wait time while debugging these scripts.
.monFile <- file.path(dd.dir, 'SST.46050.mon.csv')
if(!file.exists(.monFile) ||
   difftime(Sys.time(), file.info(.monFile)$mtime, units='days') > maxFileAge) {
  .raw <- get(paste(.dsd$SrcName, '.raw', sep=''))
  # Fix misc. data problems:  
  #   2-digit year before 1999
  .cnames <- colnames(.raw)
  .sel <- .raw$YY < 100
  .raw$YY[.sel] <- .raw$YY[.sel] + 1900
  #   minute column added 2005 as fifth column
  .sel <- .raw$YY < 2005
  .raw[.sel,] <- cbind(.raw[.sel,1:4], 
                       NA, .raw[.sel,-c(1:4,18)])
  #   TIDE column added 2000.08.01 at end of record
  #     Nothing to do, auto-filled with NA by read.table() with fill
  #   Replace missing value codes:
  .recode <- function(x, mv) {as.vector(ifelse(is.na(x) | (x >= mv), NA, t(x)))}
  .mv <- c(Inf, Inf, Inf, Inf, Inf, 999, 99, 99, 99, 99, 
           99, 999, 9999, 999, 999, 999, 99, 99)
  .raw <- data.frame(t(apply(.raw, 1, .recode, mv=.mv)))
  colnames(.raw) <- .cnames
  rm(.sel, .mv)
  #  One remaining bad value, WTMP==0
  .raw$WTMP[.raw$WTMP < 5] <- NA
  # select Water Temperature (SST) column, build monthly table:
  .yrs <- sort(unique(.raw[,1]))
  SST.46050.mon <- tapply(.raw$WTMP, 
                          list(Year=.raw$YY, Mon=.raw$MM), 
                          mean, na.rm=T)
  # sweep out column means to get monthly anomaly:
  #.mns <- apply(SST.46050.mon, 2, mean, na.rm=T)
  #SST.46050.mon <- sweep(SST.46050.mon, 2, .mns)

  dimnames(SST.46050.mon) <- list(Year=.yrs, Month=month.abb)
  SST.46050.mon <- data.frame(Year=.yrs, SST.46050.mon)
} else {
  SST.46050.mon <- read.csv(.monFile)
} # if (!file.exists...
  .data <- as.vector(t(SST.46050.mon[ , 2:13]))
  .decyr <- min(SST.46050.mon$Year) + (1:length(.data) - 0.5)/12
  SST.46050.ts <- data.frame(DecYr=.decyr, Data=.data)

##          CWT (Coastal Water Temperature
##             is already a monthly table, so not much to do:
      
cat(' * Processing CWT *\n')
.dsd <- dataSetDefs$CWT
.raw <- get(paste(.dsd$SrcName, '.raw', sep=''))
CWT.mon <- .raw
# sweep out column means to get monthly anomaly:
#.mns <- apply(CWT.mon[, -1], 2, mean, na.rm=T)
#CWT.mon[ , -1] <- sweep(CWT.mon[ , -1], 2, .mns)
.data <- as.vector(t(CWT.mon[ , -1]))
.decyr <- min(CWT.mon$Year) + (1:length(.data) - 0.5)/12
CWT.ts <- data.frame(DecYr=.decyr, Data=.data) 

##          BFL (Columbia River Discharge at Bonneville)
##            is already a monthly table, so not much to do:

# cat(' * Processing BFL *\n')
# .dsd <- dataSetDefs$BFL
# .raw <- get(paste(.dsd$SrcName, '.raw', sep=''))
# BFL.mon <- .raw
# .data <- as.vector(t(.raw[ , -1]))
# .decyr <- min(.raw$Year) + (1:length(.data) - 0.5)/12
# BFL.ts <- data.frame(DecYr=.decyr, Data=.data) 

cat(' * Processing UWI *\n')
##          UWI (Upwelling Index)
##            is daily data with four columns:  Year, Mon, Day, and 
##            UWI, the daily upwelling index.  Compute monthly means:

.dsd <- dataSetDefs$UWI
.raw <- get(paste(.dsd$SrcName, '.raw', sep=''))
# Get year and month from date column, convert number to text to POSIX date
.date <- strptime(format(.raw$Date), '%Y%m%d') 
.raw <- data.frame(Year=.date$year+1900, Mon=.date$mon+1, Day=.date$mday,
                   UWI=.raw$UWI)
# Replace missing value code (-9999) with NA:
.raw$UWI[.raw$UWI < -9000] <- NA
# Compute monthly means:
UWI.mon <- tapply(.raw$UWI, 
                        list(Year=.raw$Year, Mon=.raw$Mon), 
                        mean, na.rm=T)
UWI.mon <- data.frame(Year=as.numeric(rownames(UWI.mon)), UWI.mon)
names(UWI.mon) <- c('Year', month.abb)
# sweep out column means to get monthly anomaly:
#.mns <- apply(UWI.mon[, -1], 2, mean, na.rm=T)
#UWI.mon[ ,-1] <- sweep(UWI.mon[ ,-1], 2, .mns)
.data <- as.vector(t(UWI.mon[ , -1]))
.decyr <- min(UWI.mon$Year) + (1:length(.data) - 0.5)/12
UWI.ts <- data.frame(DecYr=.decyr, Data=.data) 
# Store revised raw structure for later use:
UWI.raw <- .raw

##          SPT.LGR (Logerwell-method Physical Spring Transition)
           
cat(' * Processing SPT.LGR *\n')
.dsd <- dataSetDefs$SPT.LGR
.raw <- get(paste(.dsd$SrcName, '.raw', sep=''))
SPT.LGR.ann <- data.frame(DecYr=.raw$Year, Data=.raw$Logerwell)
# Sort by year:
SPT.LGR.ann <- SPT.LGR.ann[order(SPT.LGR.ann$DecYr), ]
# Truncate NA's before 1969:
##SPT.LGR.ann <- SPT.LGR.ann[SPT.LGR.ann$DecYr >= 1969, ]
SPT.LGR.ts <- SPT.LGR.ann

##     SAVE TABLES TO LOCAL DATA DIRECTORY

##     All the monthly and annual data frames are saved as 
##     comma-separated value (.csv) files for future processing.
##     
    
for (dsd in dataSetDefs) {
  # Save Monthly Tables (if they exist)
  obj.name <- paste(dsd$Name, '.mon', sep='')
  if (exists(obj.name)) {
    cat('Writing ', obj.name, '\n')
    write.csv(get(obj.name), 
              file.path(dd.dir, paste(obj.name, '.csv', sep='')), 
              row.names=F)
  } # if (exists...
  # Save Annual Tables (if they exist)
  obj.name <- paste(dsd$Name, '.ann', sep='')
  if (exists(obj.name)) {
    cat('Writing ', obj.name, '\n')
    write.csv(get(obj.name), 
              file.path(dd.dir, paste(obj.name, '.csv', sep='')), 
              row.names=F)
  } # if (exists...
} # for (dsd)
