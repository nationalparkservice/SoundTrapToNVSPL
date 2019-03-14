## USES PAMGUIDE TO CALIBRATE AND CONVERT AUDIO FILES INTO NVSPL FORMAT

# created on 2/1/2019, and modifed batch_PAMGUIDE_NVSPL_V9.R

# V9: most recent version that was converted to more user friendly code

rm(list=ls(all=TRUE)) 

## -------------------------------------------------------------------------
# PACKAGES AND FUNCTIONS
## -------------------------------------------------------------------------
library(svDialogs)
split_path <- function(x) if (dirname(x)==x) x else c(basename(x),split_path(dirname(x)))

## -------------------------------------------------------------------------
# READ IN PARAMERS
## -------------------------------------------------------------------------
params = dlgOpen(title = "choose parameters file: paramsFile_project_instrum")$res
load(params)

## -------------------------------------------------------------------------
# BATCH PROCEDSS ALL AUDIO FILES IN DIRECTORies SELECTION
# can process multiple sites as long as they have the same calibration parameters
## -------------------------------------------------------------------------
for (ff in 1:length(WAVDirs)) 
{
  ## NEED to LOOP through wav files of the same day to make NVSPLs
  
  ## find unique days
  WAVfiles = list.files(WAVDirs[ff], pattern = filpat ) 
  
  # find unique days dys =  ( unlist( strsplit(basename(WAVfiles),"[.]" ) )[2] )
  d1  = sapply(strsplit(WAVfiles, "[.]"), "[", 2)
  dys = substr(d1,1,6)
  
  ## sets the file names for the directory- new one for each direcory
  site   = unlist (strsplit( basename(WAVfiles[1]), '[.]') ) [1] 
  filename = paste(site, filext, sep="")
  
  ## create NVSPL OUTPUT directory
  NVSPLdir = paste(WAVDirs[ff], "NVSPL",sep="\\") 
  dir.create(NVSPLdir,showWarnings=F,recursive=T)
  
  
  ## NOTE: uncomment and edit next line if code breaks partway through, use this to start loop on next file
  #dys = dys[74:90] 
  
  ## LOOP through the unique days- calibrate then convert to NVSPL format
  cnt = 0
  for(d in (dys) )  # d = dys[1] # for testing
  {
    
    cnt = cnt + 1
    cat('##################################################')
    cat('Processing DIRECTORY ', ff, " of ", length(WAVDirs), " for DAY", cnt, ' of ', length(unique(dys)), '\n' )
    cat('##################################################')
    
    ## (1) GET a list files for each day-------------------------------------------
    udaylist = grep(d, WAVfiles, value=T)
    filenms = paste(WAVDirs[ff], "\\", udaylist, sep="")
    
    ## (2) RUN PAMGUIDE-------------------------------------------------------------
    setwd(PAMdir)
    source('Meta.R')
    
    Meta(chunksize = 500, type = 'TOL', timestring = filename,
         r=0, outwrite=1, plottype = "None", calib=1, 
         envi=enviset, ctype="TS", Mh=mhset, G=Gset, vADC=vADCset)
    
    ## (3) READ IN file created by PAMGUIDE------------------------------------------
    PAMfiles = list.files(WAVDirs[ff], pattern = "Conk.*.csv", 
                          recursive=T, full.names=T)
    PAMfiles2 = list.files(WAVDirs[ff], pattern = "*.csv", 
                           recursive=T, full.names=T)
    PAMdirFiles = dirname(PAMfiles2[2])
    
    # read in 1st PAM file
    conk <- as.matrix( read.csv(PAMfiles[1], colClasses="numeric",header=FALSE) )
    
    # remove the folder with files for each WAV file
    unlink(PAMfiles2)
    unlink(PAMdirFiles, recursive = TRUE)
    
    ## (4) EXTRACT PARAMS--------------------------------------------------------------
    aid <- conk[1,1]  
    tstampid <- substr(aid,1,1)		#extract time stamp identifier
    enviid <- substr(aid,2,2)			#extract in-air/underwater identifier
    calibid <- substr(aid,3,3)		#extract calibrated/uncalibrated identifier
    atypeid <- substr(aid,4,4)
    
    # assign PAMGuide variables envi, calib, atype from metadata
    if (tstampid == 1){tstamp = 1} else {tstamp = ""}
    if (enviid == 1){
      envi = 'Air'  ; pref <- 20			
    } else {envi = 'Wat' ; pref <- 1}
    if (calibid == 1){calib = 1
    } else {calib = 0}
    if (atypeid == 1){atype = 'PSD'
    } else if (atypeid == 2) {atype = 'PowerSpec'
    } else if (atypeid == 3) {atype = 'TOLf'
    } else if (atypeid == 4) {atype = 'Broadband'
    } else if (atypeid == 5) {atype = 'Waveform'}
    
    # extract DATA SPL DATA and TIMESTAMP.....
    dimc <- dim(conk)  	
    t <- conk[2:dimc[1],1]
    t <- as.POSIXct(t,origin="1970-01-01")
    tString <- as.character(t)
    a <- conk[2:dimc[1],2:dimc[2]] 
    f <- conk[1,2:dimc[2]]
    # hist(a)   max(a)   min(a)
    rm(conk)
    
    ## (5) FORMAT myOutput as NVSPL-----------------------------------------------------
    # (note: PAMguide starts at 25 Hz, so lower bands (12.5, 15.8, and 20 are always NaNs)
    NVSPLhead = c("SiteID","STime", "H12p5", "H15p8", "H20", "H25", "H31p5","H40","H50","H63","H80","H100","H125","H160","H200","H250","H315","H400","H500",
                  "H630","H800","H1000","H1250","H1600","H2000","H2500","H3150","H4000","H5000","H6300","H8000","H10000","H12500","H16000","H20000",
                  "dbA","dbC","dbF","Voltage","WindSpeed","WindDir","TempIns","TempOut","Humidity",
                  "INVID","INSID","GChar1","GChar2","GChar3", "AdjustmentsApplied","CalibrationAdjustment","GPSTimeAdjustment","GainAdjustment","Status")
    
    # check to see of more 1/3 OCB than 33, if so truncate data
    if(dim(a)[2] > 30) a <- a[,1:30]
    
    # check to see if less than 33 octave
    endA = ((33-4)-dim(a)[2])+1
    
    # calculate a dBA
    aweight <- c(-63.4,-56.7,-50.5,-44.7, -39.4, -34.6, -30.2, -26.2, -22.5, - 19.1, -16.1,
                 -13.4, -10.9, -8.6, -6.6, -4.2, -3.2, -1.9, -0.8, 0, 0.6, 1, 1.2,
                 1.3, 1.2, 1.0, 0.5, -0.1, -1.1, -2.5, -4.3, -6.6, -9.3)
    # only use a-weights for the available data
    #aA <- a + aweight[4:(33-endA)]
    #a[1,] + aweight[4:(33-endA)]
    aA = t( t(a) + aweight[4:(33-endA)] )
    
    # convert to pressure
    press <- rowMeans(10^(aA/10))
    dBA = 10*log10(press) #hist(dBA)
    
    # if underwater, rescales the values to the AMT scale, using a normalization formula
    if (rescWat == 1) {
      if (envir == 1)   
      {
        cat("rescaling db levels to work with AMT")
        a2 = ((a -  (-8)) / (87 - (-8))) * a 
      # hist(a2) a2 = a - 62 # accounts for offset of water/ air
      a=a2
      rm(a2)
      }
    }
    
    ## determine how many blank columns in NVSPL, assumes you add the first 5 columns
    nBlankCols <- length(NVSPLhead) - (dim(a)[2] + 5)
    
    ## find unique day hours
    unqHrs <- substr(tString,1,13)
    
    ## create matrix with all the data combined and add headers
    tempOutput <- cbind(site, tString, 0, 0, 0, round(a, 1), 
                        matrix(rep(0,dim(a)[1] * nBlankCols), 
                               nrow=dim(a)[1], ncol=nBlankCols))
    tempOutput[,36] = dBA
    tempOutput[,54] = vers
    tempOutput[,52] = timezone
    tempOutput[,50] = rescWat # if 1, underwater values rescaled to, otherwise 0 is not rescaled
    
    colnames(tempOutput) <- NVSPLhead
    
    ## separate tempOutput by unique day hours
    tempOutput <- cbind(unqHrs, tempOutput) #add a column to sort by
    unqHrData <- split(tempOutput, tempOutput[,1]) #find where to split the data
    
    ## write out data to separate files, breaks into hours
    for(hr in 1:length(unqHrData))
    {
      dataToWrite <- matrix(unqHrData[[hr]],ncol=dim(tempOutput)[2])[,-1]
      colnames(dataToWrite) <- NVSPLhead
      outFileName <- paste(NVSPLdir,"\\", "NVSPL_", site, "_", 
                           gsub(" ","_",gsub("-","_",names(unqHrData[hr]))), 
                           ".txt", sep="")
      write.csv(dataToWrite, file=outFileName, na="", quote=F, row.names=F)
    }
    
  } ## END OF DAY LOOP (d)
  
} ## END OF DIRECTORY LOOP (ff)

