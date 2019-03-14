## TEST A FILE BEFORE RUNNING FULL CODE
# USES PAMGUIDE TO CALIBRATE AND CONVERT AUDIO FILES INTO NVSPL FORMAT

# created on 2/1/2019

rm(list=ls(all=TRUE)) 

# -------------------------------------------------------------------------
# PACKAGES AND FUNCTIONS
## -------------------------------------------------------------------------
library(svDialogs)

## -------------------------------------------------------------------------
# READ IN PARAMERS
## -------------------------------------------------------------------------
params = dlgOpen(title = "choose parameters file: paramsFile_project_instrum")$res
load(params)

## -------------------------------------------------------------------------
# PROCESS ONE FILE AND CHECK OUTPUT
## -------------------------------------------------------------------------
# get a test file
WAVFiles = list.files(WAVDirs[1], pattern = filpat, full.names = TRUE) 


# sets the filenames
site   = unlist (strsplit( basename(WAVFiles[1]), '[.]') ) [1] 
filename2 = paste(site, filext, sep="")

# set directory to correct code
setwd(PAMdir) 
source('PAMGuide.R')

# run the calibration 
PAMGuide(chunksize = 500, atype = 'TOL', timestring = filename2,
         r=0, outwrite=1, plottype = "None", 
         calib=1, envi=enviset, ctype="TS", Mh=mhset, G=Gset, vADC=vADCset)

# Evaluate the file created
testFile <- list.files(WAVDirs[1], pattern = 'TOL', recursive=T, full.names=T)
basename(testFile)

# combine data
conk <- as.matrix( read.csv(testFile[1], colClasses="numeric",header=FALSE) )
dimc <- dim(conk)  	

# what is the dB range?
a <- conk[2:dimc[1],2:dimc[2]] 
hist(a,main = "Check to see if calibration is accurate", xlab="SPL dB")
cat("dB range: ", min(a), ":", max(a) )

# time stamp- start and continious
t <- conk[2:dimc[1],1]
t <- as.POSIXct(t,origin="1970-01-01")
if ( is.na(as.character(t[1])) ) {
  cat('ERROR: TIME IS NOT CORRECT FORMAT- need to fix the Input parameters!') 
}else { 
  plot(conk[2:dimc[1],1],main = 'Is time continous?') }

# delete files, if okay, re-run if not
file.remove(testFile)
rm(conk,a,dimc,t, testFile, PAMGuide,site, filename2)

