#UpdateNBDCData.R
# Download NOAA Buoy meteorological data from NDBC.
#
#  NOTES ON DATA:
#  * 1991-1999, year was coded as 2 digits rather than 4
#  * 1991 - 2004, time was as "YY(YY) MM DD hh", while in
#    2005 - , time was "YYYY MM DD hh mm", thus shifting other columns.
#  NOTE: above problems ignored here, fixed in "ProcessData.R"
#  * In 2007, several changes were made:
#     From: https://www.ndbc.noaa.gov/mods.shtml
#     1. A "#" sign starts header or other metadata lines (April 10th, 2007).
# 
#       This will facilitate having a second header line for units of 
#       measurement (see below) and other lines interspersed in the data for 
#       significant changes in metadata, such as a station position change.
#
#     2. Column headings were standardized (April 10th, 2007).
# 
#       Some columns for the same measurement had different headings, depending 
#       on which data group they were in. For example, the column heading for 
#       air pressure was PRES in one list and BARO in another. PRES will be the 
#       standard for air pressure, APD will be the standard for Average Wave 
#       Period, WVHT will be the standard for Significant Wave Height, GST will 
#       be the standard for wind Gust speed.
#
#     3. A second header line was added to specify units of measurement 
#       (April 10th, 2007).
# 
#       The 5-day and 45-day realtime data files will be modified to include 
#       a second header line that specifies the unit of measure for each column. 
#       Generally, the units in the data files are different than the units on 
#       the station pages, which has caused misinterpretation. Note! The units 
#       are not changing - we are just adding information about the units. Raw 
#       spectral wave data files will not have the second header, as these raw 
#       data are unitless. See Measurement Descriptions and Units for more 
#       information on the units of measure used on the NDBC web site.
# 
#     NOTE: These 2007 changes don't seem to affect this script. No changes.
#
updateNBDCdata <- function(buoyID='46050',
                           startYr=1991, startMon=1,
                           localfile='buoy.txt') {
  currYr <- as.numeric(format(Sys.time(),'%Y'))
  currMon <- as.numeric(format(Sys.time(),'%m'))
  lastYr <- currYr - 1  #Historical data goes through last calendar year
  lastMon <- currMon - 1  #Recent data goes through end of last month
  if(lastMon==0) {  # Special case for January; historical files not updated
    lastMon <- 12
    lastYr <- lastYr - 1
    currYr <- currYr - 1
  } # if(lastMon)
  hist.url.path <-'http://www.ndbc.noaa.gov/data/historical/stdmet/'
  recent.url.path <-'http://www.ndbc.noaa.gov/data/stdmet/'
  
  if (file.exists(localfile)) {
    #Check end date of localfile, reset start date accordingly
    tmp <- read.table(localfile, header=T, as.is=T, fill=T)
    PrevDate <- tail(tmp,1)[c('YY', 'MM', 'DD')]
    # Here assume we never downloaded partial months, so each month is complete
    startYr <- PrevDate$YY + ifelse(PrevDate$MM==12, 1, 0)
    startMon <- (PrevDate$MM+1) %% 12
    print(c(startYr, startMon))
  } else {  # File doesn't exist, start from given start date
    # Put header row at top of file
    writeLines('YY MM DD hh mm WDIR WSPD GST  WVHT   DPD   APD MWD   PRES  ATMP  WTMP  DEWP  VIS  TIDE', localfile)
  } # if file.exists(localfile)
  
  tfname <- tempfile()
  tmpf0 <- file(tfname, open='w')
  print(file.info(tfname)[c('size', 'mtime')])
  # (1) get historical data by year, copying it all to one temporary file:
  if (startYr <= lastYr) {
    cat('processing annual data from ', startYr, ' to ', lastYr, '\n')
    for (yr in startYr:lastYr) {
      file.name <- paste(buoyID, 'h', yr, '.txt.gz', sep='')
      hist.url <- paste(hist.url.path, file.name, sep='')
      print(hist.url)
      z <- gzcon(url(hist.url))                     #open compressed connection
      tmp <- try(readLines(z))          #read text from url
      if (class(.Last.value) == 'try-error') {
        warning('URL not available.  File', tmpf0, 'not written!')
      } else {
        print(length(tmp))
        # Append only lines with digits in first column:
        writeLines(tmp[grepl('^[[:digit:]].*', tmp)], tmpf0)
      }
      close(z)
    } # for (yr)
    startYr <- currYr
    startMon <- 1           # re-start at January of cur year
  } # if (startYr <= lastYr)
  
  # (2) get current year data, month by month
  if (startMon < lastMon) {
    cat('processing monthly data from ', startMon, ' to ', lastMon, '\n')
    for (mon in startMon:lastMon) {
      ## file.name <- paste(buoyID, mon, currYr, '.txt.gz', sep='')
      ## file name changed Feb 2011: no longer has MMYYYY; no longer gz
      ## file.name <- paste(buoyID, '.txt', sep='')
      ## 7/28/2011 -- back to previous file names.
      ## 1/28/2013 -- another change: hYYYY, h=lowercase hex month code
      file.name <- paste(buoyID, as.hexmode(mon), currYr, '.txt.gz', sep='')
      recent.url <- paste(recent.url.path, month.abb[mon], '/', file.name,
                          sep='')
      print(recent.url)
      z <- gzcon(url(recent.url))       #open compressed connection
##        z <- url(recent.url)
      ## No longer compressed, try plain text download
##      .tst <- try(open(z))
      .tst <- try(open(z))
##      print(.tst)
      if (class(.tst) == 'try-error') {
        warning('URL ', recent.url, ' not available')
      } else {
        cat('Reading from ', recent.url, '\n')
        tmp <- readLines(z)          #read text from url
        print(length(tmp))
        # Append only lines with digits in first column:
        writeLines(tmp[grepl('^[[:digit:]].*', tmp)], tmpf0)
        close(z)
      }
##      close(z)
    } # for (mon)
  } # if (startMon < lastMon)
  close(tmpf0)
  print(file.info(tfname)[c('size', 'mtime')])
  file.append(localfile, tfname)
  
} # updateNBDCdata()

## # example:
## .buoyID <- '46050'
## .startYr <- 2008
## .startMon <- 1
## .savefile <- file.path('..', 'Data', paste('NB', .buoyID, 'met.txt', sep=''))
## updateNBDCdata(buoyID=.buoyID, startYr=.startYr, startMon=.startMon,
##                localfile=.savefile)
