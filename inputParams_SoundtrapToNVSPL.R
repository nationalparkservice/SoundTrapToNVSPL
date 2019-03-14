## GENERATES A PARAMETERS INPUT FILE FOR SongMeterToNVSPL_modPAMGUIDE.R
# created on 2/1/2019

rm(list=ls(all=TRUE)) 

#--------------------------------------------------------------------------------
# INSTALL PACKAGES
#--------------------------------------------------------------------------------
library(svDialogs)

## UPDATE THE FOLLOWING PARAMETERS before running SongMeterToNVSPL scripts
#--------------------------------------------------------------------------------

## (1) SET DIRECTORY WHERE ALL AUDIO FILES TO PROCESS ARE
#if you want to process multiple directories (site) with same recording parameters, choose the highest directory
WAVDirsDIR =  "F:\\NPS_DATA\\SITKA_underwater\\AUDIO\\SITK001d01"

## (2) LIST OF DIRECTORIES TO PROCESS
WAVDirs = list.dirs(WAVDirsDIR)
# ARE YOUR FILES IN "Data" subdirectories? (standard format from song meters)
DataFolders = "No" # "Yes"
if (DataFolders == "Yes") {WAVDirs = WAVDirs[grep("Data", WAVDirs)]}

## (3) FILE NAMES FORMAT
filext = ".%y%m%d%H%M%S.wav"
filpat = ".+\\d{12}.wav"

## (4) VERSION OF PAMGUIDE, see the most recent code
vers = "PAMGUIDE_V9"

## (5) SET DIRECTORY WITH PAMGUIDE CODE
PAMdir = "F:\\CODE\\GitHub\\SongMeterToNVSPL\\PAMGuideCode"

## (6) SET CALIBRATION PARAMS FOR THE INSTRUMENT
instrum = "Soundtrap"
project = "SITKA" #give your file a project name
mhset   = -184.5   #http://oceaninstruments.azurewebsites.net/App/# need to know serial number and gain setting
Gset    = 0     #gain setting
vADCset = 1     #zero-peak
enviset = "Wat" #"Air" or "Wat" 
envir   = 1     #1= water, 2=air measurements
rescWat = 0     #1= if you want to re-scale underwater values to be able to plot using AMT?
calTone = 1     #0= off, 1=on (need to skip first )
timezone = "GMT" #what time zone are the times in? so you can convert later if necessary

# SM2: mh=-36, G=xx, vADC=1.414
# SM3: mh=-36, G=XX, vADC=1
# SM4: mh=-35, G=XX, vADC=1

#--------------------------------------------------------------------------------
## WRITE OUT THE FILE- do not change!
#--------------------------------------------------------------------------------
setwd(WAVDirsDIR)
save.image(file=paste("paramsFile_",project,"_",instrum,sep=""))
