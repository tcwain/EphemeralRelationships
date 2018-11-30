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
                    ONI=list(Name='ONI',
                      #Title=expression(Oceanic ~~ Ni * tilde(n) * o ~~ Index),
                      Title='Oceanic Niño Index',
                      Units=expression(degree * C),
                      Freq='Monthly',
                      SourceTag='NOAA Climate Prediction Center',
                      SrcName='ONI'),
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
                      SourceTag='PFMC PreSeason Report I',
                      SrcName='SPR.TRANS'),
                    OCN.RCR.ann=list(Name='OCN.RCR',
                      Title='OCNR Coho Recruits (lagged to ocean entry, y-2)',
                      Units='Thousands',
                      Freq='Annual',
                      SourceTag='PFMC PreSeason Report I',
                      SrcName='OCN.RIV'),
                    OCN.SPN=list(Name='OCN.SPN',
                      Title='OCNR Coho Spawners (lagged to ocean entry, y+1)',
                      Units='Thousands',
                      Freq='Annual',
                      SourceTag='PFMC PreSeason Report I',
                      SrcName='OCN.RIV'),
                    COP.RCH=list(Name='COP.RCH',
                                 Title='Copepod Species Richness',
                                 Units='Number of Species',
                                 Freq='Annual',
                                 SourceTag='NOAA/NWFSC Fish Ecology Division',
                                 SrcName='NHL'),
                    COP.NAN=list(Name='COP.NAN',
                                 Title='Northern Copepod Anomaly',
                                 Units='mg C m^-3',
                                 Freq='Annual',
                                 SourceTag='NOAA/NWFSC Fish Ecology Division',
                                 SrcName='NHL'),
                    COP.SAN=list(Name='COP.SAN',
                                 Title='Southern Copepod Anomaly',
                                 Units='mg C m^-3',
                                 Freq='Annual',
                                 SourceTag='NOAA/NWFSC Fish Ecology Division',
                                 SrcName='NHL'),
                    SPT.BIO=list(Name='SPT.BIO',
                                 Title='Biological Transition',
                                 Units='Day of Year',
                                 Freq='Annual',
                                 SourceTag='NOAA/NWFSC Fish Ecology Division',
                                 SrcName='NHL'),
                    TMP.DP=list(Name='TMP.DP',
                                 Title='Deep Temperature',
                                 Units='Degrees C',
                                 Freq='Annual',
                                 SourceTag='NOAA/NWFSC Fish Ecology Division',
                                 SrcName='NHL'),
                    SAL.DP=list(Name='SAL.DP',
                                 Title='Deep Salinity',
                                 Units='PSU',
                                 Freq='Annual',
                                 SourceTag='NOAA/NWFSC Fish Ecology Division',
                                 SrcName='NHL'),
                    ICH.BIO=list(Name='ICH.BIO',
                                 Title='Ichthyoplankton Biomass',
                                 Units='mg C (1000 m^3)^-1',
                                 Freq='Annual',
                                 SourceTag='NOAA/NWFSC Fish Ecology Division',
                                 SrcName='NHL'),
                    ICH.COM=list(Name='ICH.COM',
                                 Title='Ichthyoplankton Community Index',
                                 Units='unitless',
                                 Freq='Annual',
                                 SourceTag='NOAA/NWFSC Fish Ecology Division',
                                 SrcName='NHL')
)

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

##          ONI (Oceanic Niño Index)
##            file has four columns:  first is three-character 
##            abbreviation for three-month moving average period (e.g. DJF 
##            for Dec-Jan-Feb average), second is 4-digit year, third is
##            total index, fourth is the anomaly:

cat(' * Processing ONI *\n')
.dsd <- dataSetDefs$ONI
.raw <- get(paste(.dsd$SrcName, '.raw', sep=''))
# print(summary(.raw))  ###DEBUG###
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
.data <- as.vector(t(CWT.mon[ , -1]))
.decyr <- min(CWT.mon$Year) + (1:length(.data) - 0.5)/12
CWT.ts <- data.frame(DecYr=.decyr, Data=.data) 

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
SPT.LGR.ts <- SPT.LGR.ann

##          OCN.RCR (OCNR Recruits)

cat(' * Processing OCN.RCR *\n')
.dsd <- dataSetDefs$OCN.RCR
.raw <- get(paste(.dsd$SrcName, '.raw', sep=''))
# Lag data to ocean entry year:
OCN.RCR.ann <- data.frame(DecYr=.raw$YEAR-2, Data=.raw$ADULTS)
OCN.RCR.ts <- OCN.RCR.ann

##          OCN.SPN (OCNR Spawners)

cat(' * Processing OCN.SPN *\n')
.dsd <- dataSetDefs$OCN.SPN
.raw <- get(paste(.dsd$SrcName, '.raw', sep=''))
# Lag data to ocean entry year:
OCN.SPN.ann <- data.frame(DecYr=.raw$YEAR-2, Data=.raw$SPAWNERS)
OCN.SPN.ts <- OCN.SPN.ann

##     NEWPORT LINE DATA:

##     COP.RCH (Copepod richness anom. (no. species; May-Sept))

cat(' * Processing COP.RCH *\n')
.dsd <- dataSetDefs$COP.RCH
.raw <- get(paste(.dsd$SrcName, '.raw', sep=''))
COP.RCH.ann <- data.frame(DecYr=.raw$Year, Data=.raw$CopRch)

##     COP.NAN (N. copepod biomass anom. (mg C m-3; May-Sept))

cat(' * Processing COP.NAN *\n')
.dsd <- dataSetDefs$COP.NAN
.raw <- get(paste(.dsd$SrcName, '.raw', sep=''))
COP.NAN.ann <- data.frame(DecYr=.raw$Year, Data=.raw$CopNan)

##     COP.SAN (S. copepod biomass anom. (mg C m-3; May-Sept))

cat(' * Processing COP.SAN *\n')
.dsd <- dataSetDefs$COP.SAN
.raw <- get(paste(.dsd$SrcName, '.raw', sep=''))
COP.SAN.ann <- data.frame(DecYr=.raw$Year, Data=.raw$CopSan)

##     SPT.BIO (Biological spring transition(day of year))

cat(' * Processing SPT.BIO *\n')
.dsd <- dataSetDefs$SPT.BIO
.raw <- get(paste(.dsd$SrcName, '.raw', sep=''))
SPT.BIO.ann <- data.frame(DecYr=.raw$Year, Data=.raw$BioTrn)

##     TMP.DP (Deep temperature (deg C, May-Sept))

cat(' * Processing TMP.DP *\n')
.dsd <- dataSetDefs$TMP.DP
.raw <- get(paste(.dsd$SrcName, '.raw', sep=''))
TMP.DP.ann <- data.frame(DecYr=.raw$Year, Data=.raw$TmpDp)

##     SAL.DP (Deep salinity (May-Sep))

cat(' * Processing SAL.DP *\n')
.dsd <- dataSetDefs$SAL.DP
.raw <- get(paste(.dsd$SrcName, '.raw', sep=''))
SAL.DP.ann <- data.frame(DecYr=.raw$Year, Data=.raw$SalDp)

##     ICH.BIO (Ichthyoplankton biomass (mg C 1000 m-3; Jan-Mar))

cat(' * Processing ICH.BIO *\n')
.dsd <- dataSetDefs$ICH.BIO
.raw <- get(paste(.dsd$SrcName, '.raw', sep=''))
ICH.BIO.ann <- data.frame(DecYr=.raw$Year, Data=.raw$IchBio)

##     ICH.COM (Ichthyoplankton community index (Jan-Mar))

cat(' * Processing ICH.COM *\n')
.dsd <- dataSetDefs$ICH.COM
.raw <- get(paste(.dsd$SrcName, '.raw', sep=''))
ICH.COM.ann <- data.frame(DecYr=.raw$Year, Data=.raw$IchCom)


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
