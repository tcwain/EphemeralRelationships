#DownloadPhysicalVars.R             -*-R-*-
#  Run scripts to update physical indicators for OCNR forecasts
#  Author:  Tom Wainwright, thomas.wainwright@noaa.gov
#  Created:  15 Jan 2014
#

options(warn=1)  # Print all messages

d.dir <- file.path('..', 'Data') # Data directory
dd.dir <- file.path(d.dir, 'Download') #Downloaded data directory
s.dir <- file.path('..', 'R-scripts')  # Script directory

cat('\nUpdating source datasets.\n')
source(file.path(s.dir, 'UpdateData.R'), echo=T)

cat('\nComputing derived datasets.\n')
source(file.path(s.dir, 'ProcessData.R'), echo=T)

cat('\nWriting Physical_3mo.dat analysis file.\n')
source(file.path(s.dir, 'WritePhysical3mo.R'), echo=T)
