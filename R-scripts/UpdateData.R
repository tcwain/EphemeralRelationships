## UpdateData.R
## Updates physical indicator data for Oregon Coastal coho forecasts.
## This file is run from the script Ephemera.Rmd
##   Author:  Tom Wainwright, tcwainw@gmail.com
##   Created:  13 July 2010
##   Modified: 18 Apr 2011 - Fixed some URLs
##             23 Oct 2012 - Converted from Sweave to plain R
##             24 Jan 2014 - Revised for OCN forecast data sets
##             12 Nov 2018 - Removed unused data series code
##             17 Jan 2019 - Minor fixes to comments
##             24 Jan 2019 - Added OPIH adult & smolt data
##
##     This script performs updates of long-term climate data 
##     from various web sources and stores them in local files in the 
##     "Data" directory.  Before downloading data, it checks the timestamp of
##     the most recent version on disk, and only downloads new data if
##     the disk version is out of date.

## Run parameters:
# maxFileAge gives the maximum allowable age of a local data file (in days)
#    This is set > 0 to prevent repeated time-consuming downloads of datasets
#    that have already been successfully downloaded within the recent past.
#    Can be set to 0 to ensure that everything is up-to-date as of today.
#    NOTE: this is overridden by some of the specialized download scripts.

maxFileAge <- 30 # Maximum age (days) of a local data file

##     DATA SOURCE DEFINITIONS
    
##     First, define the data source files. Each source is defined by a 
##     structure with several named elements, some of which are optional:
##     
##       Name:  (char) Short name (used for constructing file names, etc.).
##       URL:  (char) URL for web-based data (omit for local files).
##       LocFile:  (char) Name of local source data file.
##       FileFormat: (char) the file format ('csv', 'tbl', 'fwf', 'nc').  
##         File extensions indicate the local file type: '.tbl' for tabular
##         data with columns separated by white space, '.csv' for 
##         comma-separated values, '.fwf' for fixed-width columns, '.nc'
##         for NetCDF binary format.
##       NumericRows: (logical) read only lines begining with character 0-9
##       SkipRows: (numeric) vector containing row indices to skip, e.g.,
##         c(1:10,101:200) might skip both head and tail of a file
##       Scrub: (char) characters to be scrubbed from input file
##       ColNames: (char) vector of column names
##       ColWidths: (integer) widths of columns for 'fwf' format files
    
dataSrcDefs <- list(PDO=list(Name='PDO',
                      URL='http://jisao.washington.edu/pdo/PDO.latest',
                      LocFile=file.path(dd.dir,'PDO.tbl'),
                      FileFormat='tbl',
                      NumericRows=TRUE,
                      Scrub='\\*',
                      ColNames=c('Year',month.abb)), 
                    NPGO=list(Name='NPGO',
                      URL='http://o3d.org/npgo/data/NPGO.txt',
                      LocFile=file.path(dd.dir,'NPGO.tbl'),
                      FileFormat='tbl',
                      SkipRows=1:15,
                      ColNames=c('Year','Mon','NPGO')),
                    ONI=list(Name='ONI',
                      URL='http://www.cpc.ncep.noaa.gov/data/indices/oni.ascii.txt',
                      LocFile=file.path(dd.dir,'ONI.tbl'),
                      FileFormat='tbl',
                      SkipRows=1,
                      ColNames=c('MMM','Year','Tot', 'ONI')),
                    SST.46050=list(Name='SST.46050',
                      #No URL here, processed by separate script
                      LocFile=file.path(dd.dir,'SST.46050.tbl'),
                      FileFormat='tbl',
                      SkipRows=1,
                      ColNames=c('YY', 'MM', 'DD', 'hh', 'mm', 
                        'WDIR', 'WSPD', 'GST', 'WVHT', 'DPD', 
                        'APD', 'MWD', 'PRES', 'ATMP', 'WTMP', 
                        'DEWP', 'VIS', 'TIDE')),
                    CWT=list(Name='CWT',
                      #No URL here, processed by separate script
                      LocFile=file.path(dd.dir,'CWT.tbl'),
                      FileFormat='tbl',
                      SkipRows=1,
                      ColNames=c('Year',month.abb)),
                    # UWI=list(Name='UWI',
                    #   URL='https://www.pfeg.noaa.gov/products/PFELData/upwell/daily/p07dayac.all',
                    #   LocFile=file.path(dd.dir,'UWI.tbl'),
                    #   FileFormat='tbl',
                    #   SkipRows=1:6,
                    #   ColNames=c('Date', 'UWI')),
                    UWI=list(Name='UWI',
                             URL='https://www.pfeg.noaa.gov/products/PFELData/upwell/monthly/upindex.mon',
                             LocFile=file.path(dd.dir,'UWI.tbl'),
                             FileFormat='tbl',
                             SkipRows=1:4,
                             ColNames=c('Lat', 'Lon', 'Year', 'Jan', 'Feb', 'Mar',
                                        'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep',
                                        'Oct', 'Nov', 'Dec')),
                    SPR.TRANS=list(Name='SPR.TRANS',
                      #No URL here, hand-edit
                      LocFile=file.path(hand.dir,'SpringTrans.csv'),
                      FileFormat='csv',
                      SkipRows=1:2,
                      ColNames=c('Year','OSCURS','Logerwell','Peterson','CBR')),
                    OCN.RIV=list(Name='OCN.RIV',
                      #No URL here, hand-edit
                      LocFile=file.path(hand.dir,'OCN_Rivers.dat'),
                      FileFormat='tbl',
                      SkipRows=1:5,
                      ColNames=c('YEAR','ADULTS','SPAWNERS')),
                    OPIH=list(Name='OPIH',
                      #No URL here, hand-edit
                      LocFile=file.path(hand.dir,'OPIHAdSm.csv'),
                      FileFormat='csv',
                      SkipRows=1:5,
                      ColNames=c('AdYEAR','SmYEAR','ADULTS','SMOLTS')),
                    NHL=list(Name='NHL',
                      #No URL here, needs special processing, below
                      LocFile=file.path(dd.dir,'StopLight.csv'),
                      FileFormat='csv',
                      SkipRows=1,
                      ColNames=c('Year','CopRch','CopNan','CopSan','BioTrn',
                                 'TmpDp','SalDp','IchBio','IchCom'))
) # dataSrcDefs

##     UPDATE LOCAL DATA SOURCE FILES

##     Next, update local versions of any out-of-date (older than 20 days) 
##     data sets that are web-based.
##     
##     To do this, we define a function to do all the web stuff.
##     This function checks if the local file doesn't exist or is older than
##     'maxAge' days (20 days by default).  If it is out-of-date and a URL
##     is available, itreads a dataset from the URL (parameter 'url'), 
##     then writes it to a local file ('datafile'). If the file is
##     out-of-date and no URL is available, a warning is printed that file
##     may need a hand update.

update.dataset <- function(url, dataFile, maxAge=20) {
  # if local file doesn't exist or is older than maxAge:
  if ( !file.exists(dataFile) ||
  difftime(Sys.time(), file.info(dataFile)$mtime, units='days') > maxAge ) {
    # out of date:
    warning('File ', dataFile, ' may be out of date.')
    if ( !is.null(url) ) {
      cat('Updating ', dataFile, ' from ', url, '.', '\n')
      .tmp <- try(download.file(url, dataFile, quiet=T))
      if (class(.tmp)=='try-error') {
        warning('URL download error.  File ', dataFile, ' not written!')
      } # if (try-error)
    } else { # is.null(url)
      # no url
      warning('No web source.  Make sure local file ', dataFile, ' is updated.')
    } # if (!is.null(url))
  } else { # file.exists...
    cat('File ', dataFile, ' is OK.\n')
  } # if (!file.exists ...
  
} # update.dataset()

##     Each out-of-date dataset is downloaded as a text file.  
##     Progress and error messages should be printed below.

for (srcdef in dataSrcDefs) {
    update.dataset(url=srcdef$URL, 
                   dataFile=srcdef$LocFile,
                   maxAge=maxFileAge)
}

##      SST.46050 (Sea Surface Temperature at Stonewall Banks)
##      requires a complex download.  
##      Historical data (up to prior year) has to be downloaded 
##      one year at a time from the 
##      'historical_data.shtml' web page.  Current year data has to be 
##      downloaded one month at a time from the web site 
##      'http://www.ndbc.noaa.gov/data/stdmet/xxx/...' where xxx is the 
##      month abbreviation.  And, current month data is yet another 
##      web page.  See script UpdateNDBCData.R for
##      getting data from past years. WHAT A PAIN!

.srcdef <- dataSrcDefs$SST.46050
.dataFile <- .srcdef$LocFile
if(!file.exists(.dataFile) ||
    difftime(Sys.time(), file.info(.dataFile)$mtime, units='days') > maxFileAge) {
  cat('Updating ', .dataFile, ' with UpdateNBDCData.R script.\n')
  source('UpdateNBDCData.R', echo=T)
  updateNBDCdata(buoyID='46050', startYr=1991, startMon=1, 
                 localfile=.dataFile)
} # if(!file.exists)

##     CWT (Coastal Water Temperature) is also complicated,
##     with recent data from the Charleston, OR, tide station
##     requiring two files -- one for times of high and low tides, and 
##     one for the temperature (collected at 6-minute intervals).
##     Older data is from the Oregon Institute of Marine Biology (OIMB)
##     pier, collected at higher high tide from 1966 to 1997.  Missing
##     values in the later years are filled in via a calibration of 
##     the Buoy 46050 SST data (above).  If data file is missing, older data 
##     is downloaded and filled via the GetCharlestonData.R script.
##     Otherwise, tide station data is updated.

.srcdef <- dataSrcDefs$CWT
.dataFile <- .srcdef$LocFile
# Check if data file exists, if not generate from scratch:
if ( !file.exists(.dataFile) ||
     difftime(Sys.time(), file.info(.dataFile)$mtime, units='days') > maxFileAge) {
  cat('Running script GetCharlestonData.R\n')
  source('GetCharlestonData.R', echo=T)
} # if(!file.exists)

##     NHL (Newport Hydrographic Line) data
##     These are found in the NWFSC "Salmon Forecasting" website "Stoplight Chart". 
##     The data table is very messy with blank rows and text notes mixed in. Data series 
##     are rows, years are columns. First column is long data series names mixed with notes. 
##     Needs to be extracted row-by-row. First data row is year and Biological indicators 
##     are in subsequent rows. Variables we want are (row numbers as of November 2018):
##     
##     * Year   - Row 1 (after skipping first empty row)
##     * CopRch - Copepod richness anom. (no. species; May-Sept), row 12
##     * CopNan - N. copepod biomass anom. (mg C m-3; May-Sept), row 13
##     * CopSan - S. copepod biomass anom. (mg C m-3; May-Sept), row 14
##     * BioTrn - Biological transition(day of year), row 15
##     * TmpDp  - Deep temperature (deg C, May-Sept), row 9
##     * SalDp  - Deep salinity (May-Sep), row 10
##     * IchBio - Ichthyoplankton biomass (mg C 1000 m-3; Jan-Mar), row 16
##     * IchCom - Ichthyoplankton community index (Jan-Mar), row 17

.srcdef <- dataSrcDefs$NHL
.dataFile <- .srcdef$LocFile
.raw <- read.csv(file.path(hand.dir,'StopLight-Table.csv'), skip=1, 
                 header=FALSE, as.is=TRUE)
.dat <- data.frame(t(.raw[c(1, 12, 13, 14, 15, 9, 10, 16, 17), 2:ncol(.raw)]))
rownames(.dat) <- NULL
names(.dat) <- .srcdef$ColNames
##print(head(.dat))  ### DEBUG ###
write.csv(.dat, .srcdef$LocFile, row.names=FALSE)

##     LOAD AND RESTRUCTURE DATA

##     All the raw data should now be up-to-date in local files.
##     These files are then processed to extract the data, then put into 
##     standard format monthly or annual R data.frames, and saved as 
##     comma-separated value (.csv) files for future processing. 
##     
##     First, we define the custom read function that reads and processes
##     the local file for a dataset and produces a simple data frame 
##     time series objects.
##
##     NOTE: ASSUMES text files are small enough to fit in memory.
##     
##      This function proceeds as follows:
##           Skip the specified rows, if any ...
##          ... read numeric data only, if any ...
##          ...scrub out special characters, if any.
##          Then, read the data according to it's format into a
##            plain data frame.
##          From these, create monthly time series at the raw data interval.
  
readRawDataFile <- function(srcdef) {
  .name <- srcdef$Name
  .raw.name <- paste(.name, '.raw', sep='')
  .fn <- srcdef$LocFile
  if (file.exists(.fn)) {
    if (!is.null(srcdef$SkipRows)) {
      #Skip specific rows in file:
      .txt <- readLines(.fn)
      .txt <- .txt[-srcdef$SkipRows]
      .tmpf0 <- tempfile()
      writeLines(.txt, .tmpf0)
      .fn <- .tmpf0  #point to revised file
    } # if (SkipRows)
    if (!is.null(srcdef$NumericRows) && srcdef$NumericRows) {
      .txt <- readLines(.fn)
      .tmpf1 <- tempfile()
      writeLines(.txt[grepl('^[[:digit:]].*', .txt)], .tmpf1)
      .fn <- .tmpf1  #point to revised file
    } # if (NumericRows)
    if (!is.null(srcdef$Scrub)) {
      .txt <- readLines(.fn)
      .txt <- gsub(srcdef$Scrub, '', .txt, fixed=FALSE)
      .tmpf2 <- tempfile()
      writeLines(.txt, .tmpf2)
      .fn <- .tmpf2  #point to revised file
    } # if (Scrub)
    if (charmatch(srcdef$FileFormat, 'csv', nomatch=0)) {
      .tmp <- read.csv(.fn, fill=T, header=F, as.is=T, 
                       col.names=srcdef$ColNames)
      assign(.raw.name, .tmp, pos=.GlobalEnv)
    } # if 'csv'
    if (charmatch(srcdef$FileFormat, 'tbl', nomatch=0)) {
      .tmp <- read.table(.fn, fill=T, header=F, as.is=T, 
                         col.names=srcdef$ColNames)
      assign(.raw.name, .tmp, pos=.GlobalEnv)
    } # if 'tbl'
    if (charmatch(srcdef$FileFormat, 'fwf', nomatch=0)) {
      .tmp <- read.fwf(.fn, widths=srcdef$ColWidths, fill=T, header=F, as.is=T, 
                       col.names=srcdef$ColNames)
      assign(.raw.name, .tmp, pos=.GlobalEnv)
    } # if 'fwf'
    cat(.name, ': ', nrow(get(.raw.name)), ' rows & ', ncol(get(.raw.name)),
               ' columns\n')
  } else {
    warning('File not found for ', .name, ': Check data set definitions')
  }# if (file.exists(.fn))  
  # if the datasets haven't been created yet, set them to NULL:
  if (!exists(.raw.name)) assign(.raw.name, NULL, pos=.GlobalEnv)
} # readDataFile()

##     Read the local files and keep the data sets for next script

for (srcdef in dataSrcDefs) {
  cat('Reading local file for', srcdef$Name, '\n')
  readRawDataFile(srcdef)
} # for (srcdef)
