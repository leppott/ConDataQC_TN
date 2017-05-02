# Sourced Routine
##################
# Quality Control (auto)
#########################
# make user script smaller and easier to understand
# not a true function, needs defined variables in calling script
# if change variable names in either file have to update the other
##################
# Erik.Leppo@tetratech.com (EWL)
# 20150921
##################
# assumes use of CSV.  If using TXT have to modify list.files(pattern), read.csv(), and write.csv()
#
# Basic Operations:
# load all files in data directory
# perform QC
# write QC report
# save QCed data file

fun.QCauto <- function() {##FUN.fun.QCauto.START
  #
  # Verify input dates, if blank, NA, or null use all data
  # if DateRange.Start is null or "" then assign it 1900-01-01
  if (is.na(myData.DateRange.Start)==TRUE||myData.DateRange.Start==""){myData.DateRange.Start<-DateRange.Start.Default}
  # if DateRange.End is null or "" then assign it today
  if (is.na(myData.DateRange.End)==TRUE||myData.DateRange.End==""){myData.DateRange.End<-DateRange.End.Default}
  #
  # Read in list of files to work on, uses all files matching pattern ("\\.csv$")
  # ## if change formats will have to make modifications (pattern, import, export)
  files2process = list.files(path=myDir.data.import, pattern=" *.csv")
  head(files2process)
  #
  #
  # Define Counters for the Loop
  intCounter <- 0
  intCounter.Stop <- length(files2process)
  intItems.Total <- intCounter.Stop
  print(paste("Total files to process = ",intItems.Total,sep=""))
    flush.console()
  myItems.Complete  <- 0
  myItems.Skipped   <- 0
  myFileTypeNum.Air <- 0
  myFileTypeNum.Water <- 0
  #
  # Create Log file
  ##  List of all items (files)
  myItems.ALL <- as.vector(unique(files2process))
  # create log file for processing results of items
  #myItems.Log <- data.frame(cbind(myItems.ALL,NA),stringsAsFactors=FALSE)
  myItems.Log <- data.frame(ItemID=1:intItems.Total,Status=NA,ItemName=myItems.ALL)
  #


  # Error if no files to process or no files in dir




  # Start Time (used to determine run time at end)
  myTime.Start <- Sys.time()
  #
  # Perform a data manipulation on the data as a new file
  # Could use for (n in files2process) but prefer the control of a counter
  while (intCounter < intCounter.Stop)
  {##while.START
    #
    # 0. Increase the Counter
    intCounter <- intCounter+1
    #
    # 1.0. File Name, Define
    strFile = files2process[intCounter]
    # 1.1. File Name, Parse
    strFile.Base <- substr(strFile,1,nchar(strFile)-nchar(".csv"))
    strFile.parts <- strsplit(strFile.Base,"_")
    strFile.SiteID     <- strFile.parts[[1]][1]
    strFile.DataType   <- strFile.parts[[1]][2]
    # Convert Data Type to proper case
    strFile.DataType <- paste(toupper(substring(strFile.DataType,1,1)),tolower(substring(strFile.DataType,2,nchar(strFile.DataType))),sep="")
    strFile.Date.Start <- as.Date(strFile.parts[[1]][3],"%Y%m%d")
    strFile.Date.End   <- as.Date(strFile.parts[[1]][4],"%Y%m%d")
    #
    # 2. Check File and skip if doesn't match user defined parameters
    # 2.1. Check File Size
    if(file.info(paste(myDir.data.import,"/",strFile,sep=""))$size==0){
      # inform user of progress and update LOG
      myMsg <- "SKIPPED (file blank)"
      myItems.Skipped <- myItems.Skipped + 1
      myItems.Log[intCounter,2] <- myMsg
      fun.write.log(myItems.Log,myDate,myTime)
      fun.Msg.Status(myMsg, intCounter, intItems.Total, strFile)
        flush.console()
      # go to next Item
      next
    }
    # 2.2. Check SiteID
    # if not in provided site list then skip
    if(strFile.SiteID %in% myData.SiteID == FALSE) {
      # inform user of progress and update LOG
      myMsg <- "SKIPPED (Non-Match, SiteID)"
      myItems.Skipped <- myItems.Skipped + 1
      myItems.Log[intCounter,2] <- myMsg
      fun.write.log(myItems.Log,myDate,myTime)
      fun.Msg.Status(myMsg, intCounter, intItems.Total, strFile)
        flush.console()
      # go to next Item
      next
    }
    # 2.3. Check DataType
    # if not equal go to next file (handles both Air and Water)
    if (strFile.DataType %in% myData.Type == FALSE){
      # inform user of progress and update LOG
      myMsg <- "SKIPPED (Non-Match, DataType)"
      myItems.Skipped <- myItems.Skipped + 1
      myItems.Log[intCounter,2] <- myMsg
      fun.write.log(myItems.Log,myDate,myTime)
      fun.Msg.Status(myMsg, intCounter, intItems.Total, strFile)
        flush.console()
      # go to next Item
      next
    }
    # 2.4. Check Dates
    # 2.4.2.1. Check File.Date.Start (if file end < my Start then next)
    if(strFile.Date.End<myData.DateRange.Start) {
      # inform user of progress and update LOG
      myMsg <- "SKIPPED (Non-Match, Start Date)"
      myItems.Skipped <- myItems.Skipped + 1
      myItems.Log[intCounter,2] <- myMsg
      fun.write.log(myItems.Log,myDate,myTime)
      fun.Msg.Status(myMsg, intCounter, intItems.Total, strFile)
        flush.console()
      # go to next Item
      next
    }
    # 2.4.2.2. Check File.Date.End (if file Start > my End then next)
    if(strFile.Date.Start>myData.DateRange.End) {
      # inform user of progress and update LOG
      myMsg <- "SKIPPED (Non-Match, End Date)"
      myItems.Skipped <- myItems.Skipped + 1
      myItems.Log[intCounter,2] <- myMsg
      fun.write.log(myItems.Log,myDate,myTime)
      fun.Msg.Status(myMsg, intCounter, intItems.Total, strFile)
        flush.console()
      # go to next Item
      next
    }
    #
    # 3.0. Import the data
    #data.import=read.table(strFile,header=F,varSep)
    #varSep = "\t" (use read.delim instead of read.table)
    # as.is = T so dates come in as text rather than factor
    #data.import <- read.delim(strFile,as.is=TRUE,na.strings="")
    data.import <- read.csv(paste(myDir.data.import,strFile,sep="/"),as.is=TRUE,na.strings="")
    #
    # 4.0. Columns
    # Check for and add any missing columns (but not for missing data fields)
    # 4.1. Date, Time, DateTime
    # list
    strCol.DT <- c(myName.Date,myName.Time,myName.DateTime)
    # check for missing
    strCol.DT.Missing <- strCol.DT[strCol.DT %in% colnames(data.import)==FALSE]
    # add to df
    data.import[,strCol.DT.Missing] <- NA
    # 4.2.  Check for columns present and reorder columns
    # check for columns present
    strCol.Present <- myNames.Order[myNames.Order %in% colnames(data.import)==TRUE]
    # reorder Columns
    data.import <- data.import[,strCol.Present]
    # 4.3. Add FLAGS
    strCol.Flags <- myNames.Flags[myNames.Cols4Flags %in% colnames(data.import)==TRUE]
    data.import[,strCol.Flags] <- ""
    #
    #
    # data columns for flags that are present (need for later)
    #myNames.Cols4Flags.Present <- myNames.Cols4Flags[myNames.Cols4Flags %in% colnames(data.import)==TRUE]
    #
    myNames.DataFields.Present <- myNames.DataFields[myNames.DataFields %in% colnames(data.import)==TRUE]
    #
    #
    #
    # 5.  QC Date and Time fields
    #
    ############
    # may have to tinker with for NA fields
    ##############
    # get format - if all data NA then get an error
    #
    # backfill first?
    #
    # may have to add date and time (data) from above when add the missing field.
    #if does not exists then add field and data.
    #
    # if entire field is NA then fill from other fields
    # Date
    myField   <- myName.Date
    data.import[,myField][all(is.na(data.import[,myField]))] <- data.import[,myName.DateTime]
    # Time
    myField   <- myName.Time
    data.import[,myField][all(is.na(data.import[,myField]))] <- data.import[,myName.DateTime]
    # DateTime
    #myField   <- myName.DateTime
    # can't fill fill from others without knowing the format
    #
    # get current file date/time records so can set format
    # Function below gets date or time format and returns R format
    # date_time is split and then pasted together.
    # if no AM/PM then 24hr time is assumed
    format.Date     <- fun.DateTimeFormat(data.import[,myName.Date],"Date")
    format.Time     <- fun.DateTimeFormat(data.import[,myName.Time],"Time")
    format.DateTime <- fun.DateTimeFormat(data.import[,myName.DateTime],"DateTime")
    # get error if field is NA, need to fix
    # same for section below
    #
    # QC
    #  # format.Date <- "%Y-%m-%d"
    #   format.Time <- "%H:%M:%S"
    #   format.DateTime <- "%Y-%m-%d %H:%M"
    #
    # 5. QC Date and Time
    # 5.1. Convert all Date_Time, Date, and Time formats to expected format (ISO 8601)
    # Should allow for users to use different time and date formats in original data
    # almost worked
    #data.import[!(is.na(data.import[,myName.DateTime])),][myName.DateTime] <- strftime(data.import[!(is.na(data.import[,myName.DateTime])),][myName.DateTime]
    #                                                                                   ,format="%Y-%m-%d")
    # have to do where is NOT NA because will fail if the first item is NA
    # assume all records have the same format.
    #
    # 5.1.1. Update Date to "%Y-%m-%d" (equivalent to %F)
    myField   <- myName.Date
    myFormat.In  <- format.Date #"%Y-%m-%d"
    myFormat.Out <- myFormat.Date #"%Y-%m-%d"
    data.import[,myField][!is.na(data.import[,myField])] <- format(strptime(data.import[,myField][!is.na(data.import[,myField])],format=myFormat.In)
                                                                   ,format=myFormat.Out)
    # 5.1.2. Update Time to "%H:%M:%S" (equivalent to %T) (uses different function)
    myField   <- myName.Time
    myFormat.In  <- format.Time #"%H:%M:%S"
    myFormat.Out <- myFormat.Time #"%H:%M:%S"
    data.import[,myField][!is.na(data.import[,myField])] <- format(as.POSIXct(data.import[,myField][!is.na(data.import[,myField])],format=myFormat.In)
                                                                   ,format=myFormat.Out)
    # 5.1.3. Update DateTime to "%Y-%m-%d %H:%M:%S" (equivalent to %F %T)
    myField   <- myName.DateTime
    myFormat.In  <- format.DateTime #"%Y-%m-%d %H:%M:%S"
    myFormat.Out <- myFormat.DateTime #"%Y-%m-%d %H:%M:%S"
    data.import[,myField][!is.na(data.import[,myField])] <- format(strptime(data.import[,myField][!is.na(data.import[,myField])],format=myFormat.In)
                                                                   ,format=myFormat.Out)
    #   # strptime adds the timezome but drops it when added back to data.import (using format)
    #   #######################################################
    #   # doesn't work anymore, worked when first line was NA
    #   #######################################################
    #   data.import <- y
    #   x<-data.import[,myField][!is.na(data.import[,myField])]
    #   (z<-x[2])
    #   (a <- strptime(z,format=myFormat.In))
    #   (b <- strptime(x,format=myFormat.In))
    #   # works on single record but fails on vector with strftime
    #   # strptime works but adds time zone (don't like but it works)
    #
    #
    # 5.2. Update DateTime, Date, and Time if NA based on other fields
    # 5.2.1. Update Date_Time if NA (use Date and Time)
    myField   <- myName.DateTime
    myFormat  <- myFormat.DateTime #"%Y-%m-%d %H:%M:%S"
    #   data.import[,myField][data.import[,myField]==""] <- strftime(paste(data.import[,myName.Date][data.import[,myField]==""]
    #                                                                       ,data.import[,myName.Time][data.import[,myField]==""],sep="")
    #                                                                 ,format=myFormat,usetz=FALSE)
    data.import[,myField][is.na(data.import[,myField])] <- strftime(paste(data.import[,myName.Date][is.na(data.import[,myField])]
                                                                          ,data.import[,myName.Time][is.na(data.import[,myField])]
                                                                          ,sep=" ")
                                                                    ,format=myFormat,usetz=FALSE)
    # 5.2.2. Update Date if NA (use Date_Time)
    myField   <- myName.Date
    myFormat  <- myFormat.Date #"%Y-%m-%d"
    #   data.import[,myField][data.import[,myField]==""] <- strftime(data.import[,myName.DateTime][data.import[,myName.Date]==""]
    #                                                               ,format=myFormat,usetz=FALSE)
    data.import[,myField][is.na(data.import[,myField])] <- strftime(data.import[,myName.DateTime][is.na(data.import[,myField])]
                                                                    ,format=myFormat,usetz=FALSE)
    # 5.2.3. Update Time if NA (use Date_Time)
    myField   <- myName.Time
    myFormat  <- myFormat.Time #"%H:%M:%S"
    #   data.import[,myField][data.import[,myField]==""] <- strftime(data.import[,myName.DateTime][data.import[,myName.Time]==""]
    #                                                               ,format=myFormat,usetz=FALSE)
    data.import[,myField][is.na(data.import[,myField])] <- as.POSIXct(data.import[,myName.DateTime][is.na(data.import[,myField])]
                                                                      ,format=myFormat,usetz=FALSE)
    #
    # old code just for reference
    # 5.5. Force Date and Time format
    #   data.import[,myName.Date] <- strftime(data.import[,myName.Date],format="%Y-%m-%d")
    #   data.import[,myName.Time] <- as.POSIXct(data.import[,myName.Time],format="%H:%M:%S")
    #   data.import[,myName.DateTime] <- strftime(data.import[,myName.DateTime],format="%Y-%m-%d %H:%M:%S")
    #
    #
    # Create Month and Day Fields
    # month
#     myField   <- "month"
#     data.import[,myField] <- data.import[,myName.Date]
#     myFormat  <- "%m"
#     data.import[,myField][!is.na(data.import[,myName.Date])] <- strftime(data.import[,myName.Date][!is.na(data.import[,myName.DateTime])]
#                                                                     ,format=myFormat,usetz=FALSE)
    data.import[,"month"] <- as.POSIXlt(data.import$Date)$mon+1
    # day
#     myField   <- "day"
#     data.import[,myField] <- data.import[,myName.Date]
#     myFormat.In  <- myFormat.Date #"%Y-%m-%d"
#     myFormat.Out <- "%d"
#     data.import[,myField][!is.na(data.import[,myField])] <- format(strptime(data.import[,myField][!is.na(data.import[,myField])],format=myFormat.In)
#                                                                    ,format=myFormat.Out)
    data.import[,"day"] <- as.POSIXlt(data.import$Date)$mday
    #
#     # example of classes for POSIXlt
#     Sys.time()
#     unclass(as.POSIXlt(Sys.time()))
#     ?DateTimeClasses
#
    # 6. QC for each Data Type present
    # sub routine adds QC Calcs, QC Test Flags, Assigns overall Flag, and removes QC Calc Fields
    # cycle each data type (manually code)
    #
    # 6.1. WaterTemp
    myMsg.data <- "WaterTemp"
    myMsg <- paste("WORKING (QC Tests and Flags - ",myMsg.data,")",sep="")
    myItems.Complete <- myItems.Complete + 1
    myItems.Log[intCounter,2] <- myMsg
    fun.Msg.Status(myMsg, intCounter, intItems.Total, strFile)
    flush.console()
    data.import <- fun.CalcQCStats(data.import
                                  ,myName.WaterTemp
                                  ,myThresh.Gross.Fail.Hi.WaterTemp
                                  ,myThresh.Gross.Fail.Lo.WaterTemp
                                  ,myThresh.Gross.Suspect.Hi.WaterTemp
                                  ,myThresh.Gross.Suspect.Lo.WaterTemp
                                  ,myThresh.Spike.Hi.WaterTemp
                                  ,myThresh.Spike.Lo.WaterTemp
                                  ,myThresh.Roc.SD.period.WaterTemp
                                  ,myThresh.RoC.SD.number.WaterTemp
                                  ,myThresh.Flat.Hi.WaterTemp
                                  ,myThresh.Flat.Lo.WaterTemp
                                  ,myThresh.Flat.Tolerance.WaterTemp)
    # 6.2. AirTemp
    myMsg.data <- "AirTemp"
    myMsg <- paste("WORKING (QC Tests and Flags - ",myMsg.data,")",sep="")
    myItems.Complete <- myItems.Complete + 1
    myItems.Log[intCounter,2] <- myMsg
    fun.Msg.Status(myMsg, intCounter, intItems.Total, strFile)
    flush.console()
    data.import <- fun.CalcQCStats(data.import
                                  ,myName.AirTemp
                                  ,myThresh.Gross.Fail.Hi.AirTemp
                                  ,myThresh.Gross.Fail.Lo.AirTemp
                                  ,myThresh.Gross.Suspect.Hi.AirTemp
                                  ,myThresh.Gross.Suspect.Lo.AirTemp
                                  ,myThresh.Spike.Hi.AirTemp
                                  ,myThresh.Spike.Lo.AirTemp
                                  ,myThresh.Roc.SD.period.AirTemp
                                  ,myThresh.RoC.SD.number.AirTemp
                                  ,myThresh.Flat.Hi.AirTemp
                                  ,myThresh.Flat.Lo.AirTemp
                                  ,myThresh.Flat.Tolerance.AirTemp)
    # 6.3. WaterBP
    myMsg.data <- "WaterBP"
    myMsg <- paste("WORKING (QC Tests and Flags - ",myMsg.data,")",sep="")
    myItems.Complete <- myItems.Complete + 1
    myItems.Log[intCounter,2] <- myMsg
    fun.Msg.Status(myMsg, intCounter, intItems.Total, strFile)
    flush.console()
    data.import <- fun.CalcQCStats(data.import
                                  ,myName.WaterBP
                                  ,myThresh.Gross.Fail.Hi.WaterBP
                                  ,myThresh.Gross.Fail.Lo.WaterBP
                                  ,myThresh.Gross.Suspect.Hi.WaterBP
                                  ,myThresh.Gross.Suspect.Lo.WaterBP
                                  ,myThresh.Spike.Hi.WaterBP
                                  ,myThresh.Spike.Lo.WaterBP
                                  ,myThresh.Roc.SD.period.WaterBP
                                  ,myThresh.RoC.SD.number.WaterBP
                                  ,myThresh.Flat.Hi.WaterBP
                                  ,myThresh.Flat.Lo.WaterBP
                                  ,myThresh.Flat.Tolerance.WaterBP)
    # 6.4. AirBP
    myMsg.data <- "AirBP"
    myMsg <- paste("WORKING (QC Tests and Flags - ",myMsg.data,")",sep="")
    myItems.Complete <- myItems.Complete + 1
    myItems.Log[intCounter,2] <- myMsg
    fun.Msg.Status(myMsg, intCounter, intItems.Total, strFile)
    flush.console()
    data.import <- fun.CalcQCStats(data.import
                                  ,myName.AirBP
                                  ,myThresh.Gross.Fail.Hi.AirBP
                                  ,myThresh.Gross.Fail.Lo.AirBP
                                  ,myThresh.Gross.Suspect.Hi.AirBP
                                  ,myThresh.Gross.Suspect.Lo.AirBP
                                  ,myThresh.Spike.Hi.AirBP
                                  ,myThresh.Spike.Lo.AirBP
                                  ,myThresh.Roc.SD.period.AirBP
                                  ,myThresh.RoC.SD.number.AirBP
                                  ,myThresh.Flat.Hi.AirBP
                                  ,myThresh.Flat.Lo.AirBP
                                  ,myThresh.Flat.Tolerance.AirBP)
    # 6.5. WaterLevel
    myMsg.data <- "WaterLevel"
    myMsg <- paste("WORKING (QC Tests and Flags - ",myMsg.data,")",sep="")
    myItems.Complete <- myItems.Complete + 1
    myItems.Log[intCounter,2] <- myMsg
    fun.Msg.Status(myMsg, intCounter, intItems.Total, strFile)
    flush.console()
    data.import <- fun.CalcQCStats(data.import
                                  ,myName.WaterLevel
                                  ,myThresh.Gross.Fail.Hi.WaterLevel
                                  ,myThresh.Gross.Fail.Lo.WaterLevel
                                  ,myThresh.Gross.Suspect.Hi.WaterLevel
                                  ,myThresh.Gross.Suspect.Lo.WaterLevel
                                  ,myThresh.Spike.Hi.WaterLevel
                                  ,myThresh.Spike.Lo.WaterLevel
                                  ,myThresh.Roc.SD.period.WaterLevel
                                  ,myThresh.RoC.SD.number.WaterLevel
                                  ,myThresh.Flat.Hi.WaterLevel
                                  ,myThresh.Flat.Lo.WaterLevel
                                  ,myThresh.Flat.Tolerance.WaterLevel)
    #
    #############################
    # Names of columns for QC Calculations and Tests with Flags for each data column present
    # combine so can check for and remove later.
    myNames.DataFields.Present.QCCalcs <- as.vector(t(outer(myNames.DataFields.Present,myNames.QCCalcs,paste,sep=".")))
    myNames.Flags.QCTests <- paste("Flag.",as.vector(t(outer(myNames.QCTests,myNames.DataFields.Present,paste,sep="."))),sep="")
    #################################
    # not sure if need this little bit anymore
    ################################
    #
    #
    # 7. QC Tests
    # incorporated into subroutine in step 6
    #
    # 8. Generate QC File
    # incorporated into subroutine in step 6
    #
    # 9. Generate Log File
    # incorporated into subroutine in step 6
    #
    ###########################
    # save file then run QC Report in a separate Script
    ###############
    # 10.0. Output file (only works if DataType is Air OR Water *not* both)
    # 10.1. Set Name
    #File.Date.Start <- format(as.Date(myData.DateRange.Start,myFormat.Date),"%Y%m%d")
    #File.Date.End   <- format(as.Date(myData.DateRange.End,myFormat.Date),"%Y%m%d")
    strFile.Out <- paste("QCauto",strFile,sep="_")
    # 10.2. Save to File the data (overwrites any existing file).
    #print(paste("Saving output of file ",intCounter," of ",intCounter.Stop," files complete.",sep=""))
    #flush.console()
    write.csv(data.import,file=paste(myDir.data.export,"/",strFile.Out,sep=""),quote=FALSE,row.names=FALSE)
    #
    # 11. Clean up
    # 11.1. Inform user of progress and update LOG
    myMsg <- "COMPLETE"
    myItems.Complete <- myItems.Complete + 1
    myItems.Log[intCounter,2] <- myMsg
    fun.write.log(myItems.Log,myDate,myTime)
    fun.Msg.Status(myMsg, intCounter, intItems.Total, strFile)
    flush.console()
    # 11.2. Remove data (import)
    #rm(data.import)
    #
  }##while.END
  #
  myTime.End <- Sys.time()
  print(paste("Task COMPLETE; ",round(difftime(myTime.End,myTime.Start,units="mins"),2)," min.",sep=""))
  flush.console()
  #
  # return data table
  return(data.import)
}##FUN.fun.QCauto.END
######################################################################



# # ######################################################################
# # # QC
fun.data.import                 <- data.import
fun.myField.Data                <- myName.WaterLevel
fun.myThresh.Gross.Fail.Hi      <- myThresh.Gross.Fail.Hi.WaterLevel
fun.myThresh.Gross.Fail.Lo      <- myThresh.Gross.Fail.Lo.WaterLevel
fun.myThresh.Gross.Suspect.Hi   <- myThresh.Gross.Suspect.Hi.WaterLevel
fun.myThresh.Gross.Suspect.Lo   <- myThresh.Gross.Suspect.Lo.WaterLevel
fun.myThresh.Spike.Hi           <- myThresh.Spike.Hi.WaterLevel
fun.myThresh.Spike.Lo           <- myThresh.Spike.Lo.WaterLevel
fun.myThresh.Roc.SD.period      <- myThresh.Roc.SD.period.WaterLevel
fun.myThresh.RoC.SD.number      <- myThresh.RoC.SD.number.WaterLevel
fun.myThresh.Flat.Hi            <- myThresh.Flat.Hi.WaterLevel
fun.myThresh.Flat.Lo            <- myThresh.Flat.Lo.WaterLevel
fun.myThresh.Flat.Tolerance     <- myThresh.Flat.Tolerance.WaterLevel
# # ####################################################################

########################
# FUNCTION
########################
# Generate QC Test Calculations, QC Test Flags, and Assign overall flags
# input is a single data field and the thresholds
# output is a data frame (assumes data.import)
# reuses items from this script and calling script.  Not a stand alone function
#
fun.CalcQCStats <- function(fun.data.import
                            ,fun.myField.Data
                            ,fun.myThresh.Gross.Fail.Hi
                            ,fun.myThresh.Gross.Fail.Lo
                            ,fun.myThresh.Gross.Suspect.Hi
                            ,fun.myThresh.Gross.Suspect.Lo
                            ,fun.myThresh.Spike.Hi
                            ,fun.myThresh.Spike.Lo
                            ,fun.myThresh.Roc.SD.period
                            ,fun.myThresh.RoC.SD.number
                            ,fun.myThresh.Flat.Hi
                            ,fun.myThresh.Flat.Lo
                            ,fun.myThresh.Flat.Tolerance) {
  #
  # A.1. Calc, SD Time Interval
  myCalc <- "SD.Time"
  myField <- paste(fun.myField.Data,myCalc,sep=".")
  # calculate as separate variable
  #http://stackoverflow.com/questions/8857287/how-to-add-subtract-time-from-a-posixlt-time-while-keeping-its-class-in-r
  myT <- strptime(fun.data.import[,myName.DateTime],format=myFormat.DateTime)
  myT$hour <- myT$hour - fun.myThresh.Roc.SD.period
  # add back to dataframe
  fun.data.import[,myField] <- as.character(myT)
  #
  # alternate calculation, not used
  #seq.POSIXt( from=Sys.time(), by="-25 hour", length.out=2 )[2]
  #
  # variable for following block
  myField.T1 <- myField
  #
  # A.2. Calc, SD, calc SD of last 25 hours
  myCalc <- "SD"
  myField <- paste(fun.myField.Data,myCalc,sep=".")
  #myField.T2 <- myName.DateTime
#   ################
#   # calc SD
#   #
#   #a <- fun.data.import[,myName.WaterTemp]
#   #b <- fun.data.import[,myName.DateTime]
#   #sd(a[b<="2014-04-22 10:00:00" & b>="2014-01-13 11:00:00"],na.rm=TRUE)
#   #sd(a,na.rm=TRUE)
#   #
#   fun.data.import[,myField] <- sd(fun.data.import[,fun.myField.Data][fun.data.import[,myName.DateTime]<="2014-04-22 10:00:00" & fun.data.import[,myName.DateTime]>="2014-01-13 11:00:00"],na.rm=TRUE)
  ################
  # slow but works (~10 seconds for 5k records)
  # *******need to change to sapply***********
  for (m in 1:nrow(fun.data.import)) {
    fun.data.import[m,myField] <- sd(fun.data.import[,fun.myField.Data][
        fun.data.import[,myName.DateTime]<=fun.data.import[m,myName.DateTime]
        & fun.data.import[,myName.DateTime]>=fun.data.import[m,myField.T1]
        ],na.rm=TRUE)
  }
  #
  # A.3. Calc, NxSD, SD * n.per
  myCalc.1 <- "SD"
  myCalc.2 <- "SDxN"
  myField.1 <- paste(fun.myField.Data,myCalc.1,sep=".")
  myField.2 <- paste(fun.myField.Data,myCalc.2,sep=".")
  fun.data.import[,myField.2] <- fun.data.import[,myField.1] * fun.myThresh.RoC.SD.number
  #
  # A.4. Calc, Diff (1:5) (5 is default but can be more)
  for (i in 1:myThresh.Flat.MaxComp) {##FOR.j.START
    #
    myCalc <- paste("n",i,sep=".")
    myField <- paste(fun.myField.Data,myCalc,sep=".")
    fun.data.import[-(1:i),myField] <- diff(fun.data.import[,fun.myField.Data],lag=i)
    #
  }##FOR.i.END
  #
  #http://stackoverflow.com/questions/18862114/r-count-number-of-columns-by-a-condition-for-each-row
  #
  # A.5. Calc, flat.Hi, count n.1 etc if less than toler
  myCalc <- "flat.Hi"
  myField <- paste(fun.myField.Data,myCalc,sep=".")
  myThresh <- fun.myThresh.Flat.Hi
  # Fields to check
  myFields.Match <- match(paste(fun.myField.Data,"n",1:myThresh,sep="."), names(fun.data.import))
  # use rowSums to count the fields
  fun.data.import[,myField] <- rowSums(abs(fun.data.import[,myFields.Match])<=fun.myThresh.Flat.Tolerance)
  #
  # A.6. Calc, flat.Lo, count if less than toler
  myCalc <- "flat.Lo"
  myField <- paste(fun.myField.Data,myCalc,sep=".")
  myThresh <- fun.myThresh.Flat.Lo
  # Fields to check
  myFields.Match <- match(paste(fun.myField.Data,"n",1:myThresh,sep="."), names(fun.data.import))
  # use rowSums to count the fields
  fun.data.import[,myField] <- rowSums(abs(fun.data.import[,myFields.Match])<=fun.myThresh.Flat.Tolerance)
  #
  ## B. Generate Flags based on Calculation Fields
  # B.1. Gross
  myQCTest <- "Gross"
  myField <- paste("Flag",myQCTest,fun.myField.Data,sep=".")
  # Assign Flags
  # default value
  fun.data.import[,myField] <- 2
  # data is NA then flag = 9 (missing data)
  fun.data.import[,myField][is.na(fun.data.import[,fun.myField.Data])==TRUE] <- 9
  # data >= Suspect.Hi then flag = 3 (suspect)
  fun.data.import[,myField][fun.data.import[,fun.myField.Data] >= fun.myThresh.Gross.Suspect.Hi] <- 3
  # data <= Suspect.Lo then flag = 3 (Suspect)
  fun.data.import[,myField][fun.data.import[,fun.myField.Data] <= fun.myThresh.Gross.Suspect.Lo] <- 3
  # data >= Fail.Hi then flag = 4 (fail)
  fun.data.import[,myField][fun.data.import[,fun.myField.Data] >= fun.myThresh.Gross.Fail.Hi] <- 4
  # data <= Fail.Lo then flag = 4 (fail)
  fun.data.import[,myField][fun.data.import[,fun.myField.Data] <= fun.myThresh.Gross.Fail.Lo] <- 4
  # otherwise flag = 1 (pass)
  fun.data.import[,myField][fun.data.import[,myField]==2] <- 1
  # QC
  #table(fun.data.import[,myField])
  #
  # B.2. Spike
  myQCTest <- "Spike"
  myField <- paste("Flag",myQCTest,fun.myField.Data,sep=".")
  myField.Calc.1 <- paste(fun.myField.Data,"n",1,sep=".")
  # Assign Flags
  # default value
  fun.data.import[,myField] <- 2
  # diff 1 is NA then flag = 9 (missing data)
  fun.data.import[,myField][is.na(fun.data.import[,myField.Calc.1])==TRUE] <- 9
  # abs(diff 1) >= spike Lo then flag = 3 (suspect)
  fun.data.import[,myField][abs(fun.data.import[,myField.Calc.1]) >= fun.myThresh.Spike.Lo] <- 3
  # abs(diff 1) >= spike Hi then flag = 4 (fail)
  fun.data.import[,myField][abs(fun.data.import[,myField.Calc.1]) >= fun.myThresh.Spike.Hi] <- 4
  # otherwise flag = 1 (pass)
  fun.data.import[,myField][fun.data.import[,myField]==2] <- 1
  # QC
  #table(fun.data.import[,myField])
  #
  # B.3. RoC
  myQCTest <- "RoC"
  myField <- paste("Flag",myQCTest,fun.myField.Data,sep=".")
  myField.Calc.1 <- paste(fun.myField.Data,"n",1,sep=".")
  myField.Calc.2 <- paste(fun.myField.Data,"SDxN",sep=".")
  # Assign Flags
  # default value
  fun.data.import[,myField] <- 2
  # data is NA then flag = 9 (missing data)
  fun.data.import[,myField][is.na(fun.data.import[,fun.myField.Data])==TRUE] <- 9
  # sd is NA then flag = 9 (missing data)
  fun.data.import[,myField][is.na(fun.data.import[,myField.Calc.1])==TRUE] <- 9
  # diff 1 > SD*N then flag = 3 (suspect)
  fun.data.import[,myField][abs(fun.data.import[,myField.Calc.1]) > fun.data.import[,myField.Calc.2]] <- 3
  # otherwise flag = 1 (pass) [no 4/Fail]
  fun.data.import[,myField][fun.data.import[,myField]==2] <- 1
  # QC
  #table(fun.data.import[,myField])
  #
  # B.4. Flat
  myQCTest <- "Flat"
  myField <- paste("Flag",myQCTest,fun.myField.Data,sep=".")
  myField.Calc.1 <- paste(fun.myField.Data,"flat.Hi",sep=".")
  myField.Calc.2 <- paste(fun.myField.Data,"flat.Lo",sep=".")
  # default value
  fun.data.import[,myField] <- 2
  # Lo >= Thresh.Lo = 3 (suspect)
  fun.data.import[,myField][fun.data.import[,myField.Calc.2] >= fun.myThresh.Flat.Lo] <- 3
  # Hi >= Thresh.Hi = 4 (fail)
  fun.data.import[,myField][fun.data.import[,myField.Calc.1] >= fun.myThresh.Flat.Hi] <- 4
  # otherwise flag = 1 (pass)
  fun.data.import[,myField][fun.data.import[,myField]==2] <- 1
  # QC
  #table(fun.data.import[,myField])
  #
  #
  # C. Assign Overall Data Flag
  myField <- paste("Flag",fun.myField.Data,sep=".")
  #myNames.QCTests
  # get column numbers (match) for QCTest Flags for this data
  myFields.Match <- match(paste("Flag",myNames.QCTests,fun.myField.Data,sep="."), names(fun.data.import))
  # Conditional rowSums for number of flag fields with specified flags
  myFlags.Num.Pass    <- rowSums(fun.data.import[,myFields.Match]==1)
  myFlags.Num.Suspect <- rowSums(fun.data.import[,myFields.Match]==3)
  myFlags.Num.Fail    <- rowSums(fun.data.import[,myFields.Match]==4)
  myFlags.Num.Missing <- rowSums(fun.data.import[,myFields.Match]==9)
  myFlags.Num.OK      <- rowSums(fun.data.import[,myFields.Match]==1 | fun.data.import[,myFields.Match]==9)
  # Assign
  # default value
  fun.data.import[,myField] <- 2
  # any QC Test flags = 3 then flag = 3 (suspect)
  fun.data.import[,myField][myFlags.Num.Suspect > 0] <- 3
  # any QC Test flags = 4 then flag = 4 (fail)
  fun.data.import[,myField][myFlags.Num.Fail > 0] <- 4
  # all QC Test flags = 1 then flag = 1 (pass)
  fun.data.import[,myField][myFlags.Num.Pass == length(myNames.QCTests)] <- 1
  # all QC Test flags = 1 or 9 then flag = 1 (pass)
  fun.data.import[,myField][myFlags.Num.OK == length(myNames.QCTests)] <- 1
    #fun.data.import[,myField][fun.data.import[,myField]==2] <- 1
  # data is NA then flag = 9 (missing data)
  fun.data.import[,myField][is.na(fun.data.import[,fun.myField.Data])==TRUE] <- 9
  # QC
  #table(fun.data.import[,myField])
  #
  # D. Remove QC Calc fields
  #myNames.QCCalcs <- c("SD.Time","SD","SDxN","n.1","n.2","n.3","n.4","n.5","flat.Lo","flat.Hi")
  # get field column numbers
  myFields.Match <- match(paste(fun.myField.Data,myNames.QCCalcs,sep="."), names(fun.data.import))
  # drop fields from data table
  fun.data.import <- fun.data.import[,-na.omit(myFields.Match)]

  #
  # function output
  return(fun.data.import)
}










