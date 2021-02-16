#Download older data (1993 up to last calendar year) data from Charleston, OR
# tide station (sta. ID 9432780)

## Note: dd.dir and hand.dir are defined by calling script

##pdf('CharlestonDataFigs.pdf')

if (!exists('maxFileAge')) maxFileAge <- 5 # Maximum age of a local data file, only if undefined

#Local file for times of higher high tides at tide station
hh.file <- file.path(dd.dir, 'HiHiTides_9432780.csv')
#Local file for tide station monthly flood-tide water temp means
tswt.file <- file.path(dd.dir, 'WT.ChTS.mon.csv')
#Local file for OIMB pier monthly flood-tide water temp means
oimbwt.file <- file.path(hand.dir, 'WT.OIMB.mon.csv')
#Local file for OIMB pier daily flood-tide water temp means
oimbwt.daily.file <- file.path(hand.dir, 'WT.OIMB.daily.csv')
#Local file for final blended Charleston coastal water temperature:
wt.file <- file.path(dd.dir, 'CWT.tbl')

### 1. Download Charleston tide station data ###

# Only regenerate if water temp file is missing or more than maxFileAge days old:
if(!file.exists(tswt.file) ||
   difftime(Sys.time(), file.info(tswt.file)$mtime, units='days') > maxFileAge) {

  #Make sure files are empty:
  if (file.exists(hh.file)) { file.remove(hh.file) }
  if (file.exists(tswt.file)) { file.remove(tswt.file) }

  #Write column headers:
  writeLines('Year, Mon, Day, Hour, Min', hh.file)
  writeLines(paste(c('Year', month.abb), collapse=', '), tswt.file)

  # First year of temperature data at station:
  st.yr <- 1993
  #st.yr <- 2008  ###Testing###
  # Last calendar year:
  # end.yr <- as.POSIXlt(Sys.time())$year + 1900 - 1  #POSIXlt year base is 1900
  # Current calendar year:
  end.yr <- as.POSIXlt(Sys.time())$year + 1900     #POSIXlt year base is 1900
  #end.yr <- 1994  ###Testing###
  for (yr in st.yr:end.yr) {
    cat('Reading data for ', yr, '\n')
    start.date <- paste(yr, '0101', sep='')
    cat("\t start.date: ", start.date,'\n')
    end.date <- paste(yr, '1231', sep='')
    cat("\t end.date: ", end.date,'\n')
##    url.wt <- paste('http://opendap.co-ops.nos.noaa.gov/dods/IOOS/Water_Temperature.ascii?WATER_TEMPERATURE_PX.DATE_TIME,WATER_TEMPERATURE_PX.WaterTemp&WATER_TEMPERATURE_PX._STATION_ID="9432780"&WATER_TEMPERATURE_PX._BEGIN_DATE="', start.date, '"&WATER_TEMPERATURE_PX._END_DATE="', end.date, '"', sep='')
##    url.hilo <- paste('http://opendap.co-ops.nos.noaa.gov/dods/IOOS/High_Low_Verified_Water_Level.ascii?WATERLEVEL_HIGHLOW_VFD_PX.DATE_TIME,WATERLEVEL_HIGHLOW_VFD_PX.WL_VALUE,WATERLEVEL_HIGHLOW_VFD_PX.TY&WATERLEVEL_HIGHLOW_VFD_PX._STATION_ID="9432780"&WATERLEVEL_HIGHLOW_VFD_PX._DATUM="MLLW"&WATERLEVEL_HIGHLOW_VFD_PX._BEGIN_DATE="', start.date, '"&WATERLEVEL_HIGHLOW_VFD_PX._END_DATE="', end.date, '"', sep='')
    # Switch to NOAA tides & Currents server:
    ## New formats, water temp only available hourly, except 6 min. limited to 31 day download intervals
    url.wt <- paste('https://tidesandcurrents.noaa.gov/cgi-bin/newdata.cgi?type=phys&id=9432780&begin=', start.date, '&end=', end.date, '&units=metric&timezone=GMT&mode=csv&interval=h', sep='')
    url.hilo <- paste('https://tidesandcurrents.noaa.gov/api/datagetter?product=high_low&application=NOS.COOPS.TAC.WL&station=9432780&begin_date=', start.date, '&end_date=', end.date, '&datum=MLLW&units=english&time_zone=GMT&format=csv', sep='')
    #Process times of higher high tide:
    .tmp.hilo <- readLines(url.hilo)
    #Select data lines beginning with a date ('1' or '2'):
    .tmp.hilo <- .tmp.hilo[grepl('^[12]', .tmp.hilo)]
    #Select only higher high-tide lines:
    .tmp.hilo <- .tmp.hilo[grepl('HH', .tmp.hilo)]
    cat('\tHiHi records: ', (length(.tmp.hilo)), '\n')
    if (length(.tmp.hilo) > 0) {
      #Read the data into a data frame:
      hilo.data <- read.csv(textConnection(.tmp.hilo), as.is=T, header=F)
      colnames(hilo.data) <- c('Date', 'Height', 'Type', 'I', 'L')
      # Extract year, month, day, hour, min from date
      .times <- strptime(hilo.data$Date, format='%Y-%m-%d %H:%M', tz='GMT')
      # cat('\thead(.times): \n')    ### DEBUG ###
      # print(head(.times))
      hh.time <- data.frame(Yr=.times$year+1900, Mon=.times$mon+1,
                            Day=.times$mday, Hour=.times$hour, Min=.times$min)
      hh.key <- apply(hh.time[,1:4],1,paste, collapse='-')  #key for matching hours
      write.table(hh.time, file=hh.file, append=T, sep=', ',
                  col.names=F, row.names=F)
    } # if(length(.tmp.hilo)...
    #Get temperatures during hour of higher high tide:
    .tmp.wt <- readLines(url.wt)
    #Select data lines beginning with a date (starting with month, so 0 or 1 as 1st digit):
    .tmp.wt <- .tmp.wt[grepl('^[01]', .tmp.wt)]
    cat('\tWtemp records: ', (length(.tmp.wt)), '\n')
    if ((length(.tmp.wt) > 0) && (length(.tmp.hilo) > 0)) {
      tswt.data <- read.csv(textConnection(.tmp.wt), as.is=T, header=F)
      colnames(tswt.data) <- c('Date', 'WTemp', 'Cond')
      # Extract year, month, day, hour from date
      .times <- strptime(tswt.data$Date, format='%m/%d/%Y %H:%M', tz='GMT')
      tswt.time <- data.frame(Yr=.times$year+1900, Mon=.times$mon+1,
                              Day=.times$mday, Hour=.times$hour)
      tswt.key <- apply(tswt.time,1,paste, collapse='-')  #key for matching hours
      # Compute hourly mean temperatures:
      tswt.hrly <- tapply(tswt.data$WTemp, tswt.key, mean, na.rm=T)
      .dates <- strptime(names(tswt.hrly), format="%Y-%m-%d-%H")
      # index of hourly mean values matching high tide hour:
      tswt.index <- match(hh.key, names(tswt.hrly))
      tswt.index <- tswt.index[!is.na(tswt.index)] #drop NA's
      # Table of monthly mean flood-tide water temperatures:
      #  but first, fudge up a table padded with na's to be sure
      #  all months are included in the indexing
      .tmp <- data.frame(Yr=.dates$year[tswt.index]+1900,
                         Mon=.dates$mon[tswt.index]+1,
                         WTemp=tswt.hrly[tswt.index])
      .tmp <- rbind(.tmp, data.frame(Yr=yr, Mon=1:12, WTemp=NA))
      tswt.mon <- tapply(.tmp$WTemp, list(.tmp$Yr, .tmp$Mon), mean, na.rm=T)
      write.table(cbind(as.numeric(row.names(tswt.mon)), tswt.mon),
                  file=tswt.file, append=T, sep=', ',
                  col.names=F, row.names=F)
    } # if (length(.tmp.wt) ...
  } # for (yr)

} # if (!file.exists(tswt.file) ...
# Re-read data and filter bad values:

hh.time <- read.csv(hh.file, header=T, colClasses='numeric')
cat('\nSummary of higher high tide times,\n',
    '   with counts of high tides per month:\n')
print(summary(hh.time))
print(table(hh.time$Year, hh.time$Mon))

tswt.mon <- read.csv(tswt.file, header=T, colClasses='numeric')
# get rid of low bad values (June/July 2003)
tswt.mon[tswt.mon<5] <- NA
# get rid of questionable values before/after missing data (Nov 2002, Nov 2003)
tswt.mon$Nov[tswt.mon$Year %in% 2002:2003] <- NA
# get rid of initial month (Sept 1993) with only 4 data points:
tswt.mon$Sep[tswt.mon$Year==1993] <- NA

# Compute anomalies before regressing series:
.mns <- apply(tswt.mon[ , 2:13], 2, mean, na.rm=T)
tswt.anom <- cbind(Year=tswt.mon[ , 1], sweep(tswt.mon[ , 2:13], 2, .mns))
cat('\nSummary of monthly flood-tide water temperature means:\n')
print(tswt.mon)

### 2. Extend back with OIMB pier records ###
# OIMB data is flood tide daily measurements, May 1966 - Aug 1997 (but
# May 1966 is only a few days, so we discard it).

# First, check if data file exists; if not, download and process the data
if (!file.exists(oimbwt.file)) {
  # Data comes from Scripps 'Shore Stations Project' ftp site:
  .url <- 'ftp://ftp.iod.ucsd.edu/shore/historical_data/combined_years/tempseriestotal.txt'
  .tmp <- read.table(.url, sep='\t', na.strings='NaN', header=T, fill=T)
  oimb.daily <- data.frame(Year=.tmp$X.year, Month=.tmp$month, Day=.tmp$day,
                           WTemp=.tmp$charleston)
  oimb.daily <- oimb.daily[oimb.daily$Year %in% 1966:1997, ]
  write.csv(oimb.daily, file=oimbwt.daily.file, row.names=F)
  oimb.mon <- tapply(oimb.daily$WTemp,
                     list(oimb.daily$Year, oimb.daily$Month),
                     mean, na.rm=T)
  oimb.num <- tapply(!is.na(oimb.daily$WTemp),
                     list(oimb.daily$Year, oimb.daily$Month),
                     sum, na.rm=T)
  print(oimb.num) 
  oimb.mon[oimb.num <= 5] <- NA  #Filter out data-poor months
  oimb.mon <- data.frame(as.numeric(rownames(oimb.mon)), oimb.mon)
  names(oimb.mon) <- c('Year', month.abb)
  write.csv(oimb.mon, file=oimbwt.file, row.names=F)
}

oimb.mon <- read.csv(oimbwt.file)
print(oimb.mon)

#compute anomalies for regression:
.mns1 <- apply(tswt.mon[ , 2:13], 2, mean, na.rm=T)
tswt.anom <- cbind(Year=tswt.mon[ , 1], sweep(tswt.mon[ , 2:13], 2, .mns1))
.mns2 <- apply(oimb.mon[ , 2:13], 2, mean, na.rm=T)
oimb.anom <- cbind(Year=oimb.mon[ , 1], sweep(oimb.mon[ , 2:13], 2, .mns2))

# 
# Calibrate overlapping years (1993-1997)
.tmp1 <- as.vector(t(oimb.anom[oimb.anom$Year %in% 1993:1997, 2:13]))
.tmp2 <- as.vector(t(tswt.anom[tswt.anom$Year %in% 1993:1997, 2:13]))
.reg2 <- lm(.tmp2 ~ .tmp1 - 1)
print(summary(.reg2))
##matplot(oimb.anom[oimb.anom$Year %in% 1993:1997, 2:13],
##        tswt.anom[tswt.anom$Year %in% 1993:1997, 2:13])
##abline(c(0, .reg2$coef))  #add regression line to plot
oimb.adj <- cbind(Year=oimb.anom$Year, oimb.anom[,2:13]*.reg2$coef)
oimb.adj[,2:13] <- sweep(oimb.adj[,2:13], 2, -.mns2) #add monthly means back in

# add years up to 1992 to wt.mon (final output table)
wt.mon <- tswt.mon
wt.mon <- rbind(oimb.adj[oimb.adj$Year %in% 1966:1992, ], wt.mon)
rownames(wt.mon) <-wt.mon$Year
print(wt.mon)

# fill missing values in 1993:
.t93a <- wt.mon[wt.mon$Year==1993, ]
.t93b <- oimb.adj[oimb.adj$Year==1993, ]
.t93a[is.na(.t93a)] <- .t93b[is.na(.t93a)]
wt.mon[wt.mon$Year==1993, ] <- .t93a

### 4.  Fill in remaining missing values by linear interpolation:
#  At this writing, there are only 4 remaining internal missing values:
#  1968:Aug&Nov, 1979:May, and 1983:Aug.  Fix by hand:
wt.mon['1968','Aug'] <- (wt.mon['1968','Jul']+wt.mon['1968','Sep'])/2
wt.mon['1968','Nov'] <- (wt.mon['1968','Oct']+wt.mon['1968','Dec'])/2
wt.mon['1979','May'] <- (wt.mon['1979','Apr']+wt.mon['1979','Jun'])/2
wt.mon['1983','Aug'] <- (wt.mon['1983','Jul']+wt.mon['1983','Sep'])/2
print(wt.mon)

### 5.  Store final series and Summary plots ###

write.table(wt.mon, file=wt.file, row.names=F)

## matplot(wt.mon$Year, wt.mon[,2:13], main='Water Temperature by Year',
##         xlab='Year', ylab=expression(degree * C))
## matplot(1:12, t(wt.mon[,2:13]), main='Water Temperature by Month',
##         xlab='Month', ylab=expression(degree * C))
## .tmp1 <- unlist(t(wt.mon[,2:13])) # Blended time series
## plot(min(wt.mon$Year)+(1:length(.tmp1))/12, .tmp1, type='l', col='black',
##      lty=1, lwd=2,  
##      main="Monthly Mean Water Temperature", xlab='Year',
##      ylab=expression(degree * C))
## .tmp2 <- unlist(t(tswt.mon[,2:13])) # Tide station time series
## lines(min(tswt.mon$Year)+(1:length(.tmp2))/12, .tmp2, col='cyan', lty=2)
## .tmp3 <- unlist(t(sweep(buoy.mon[,2:13], 2, -.mns1))) # Buoy time series
## lines(min(buoy.mon$Year)+(1:length(.tmp3))/12, .tmp3, col='red', lty=2)
## .tmp4 <- unlist(t(oimb.mon[,2:13])) # Buoy time series
## lines(min(oimb.mon$Year)+(1:length(.tmp4))/12, .tmp4, col='blue', lty=2)
## legend('topleft', legend=c('Blended', 'Tide Sta', 'Stwl Buoy', 'OIMB Pier'),
##        lty=c(1,2,2,2), lwd=c(2,1,1,1), col=c('black','cyan','red', 'blue'))

