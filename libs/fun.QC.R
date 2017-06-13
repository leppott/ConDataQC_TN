#!/cygdrive/c/Program Files/R/R-3.3.2patched/bin/R

##################
#
# Quality Control
#
##################
# Erik.Leppo@tetratech.com (EWL)
#################
#
# assumes use of CSV.  If using TXT have to modify list.files(pattern), read.csv(), and write.csv()
#
# Basic Operations:
# load all files in data directory
# perform QC
# write QC report
# save QCed data file


# 20160208
# SensorDepth - Gross is only negative, Flat = remove
# 20160303
# Rolling SD.  Use "zoo" and rollapply.  Loop too slow for large/dense data sets.
# (will crash if less than 5 records so added "stop")

#
library(zoo, quietly = TRUE, warn.conflicts = FALSE)
library(futile.logger)

fun.QC_file <- function(input_file_name,
                        import_folder,
                        export_folder) {

  input_file_path = file.path(import_folder, input_file_name)
  flog.debug(paste0("Begin processing file\n\t",input_file_path))
  #
  # 2. Check File and skip if doesn't match user defined parameters
  #
  if(file.info( input_file_path )$size == 0)
  {
    flog.debug(paste0("File is empty (size 0 bytes).  Skipping...\n\t", input_file_path))
    next
  }


  flog.debug(paste0("Begin processing file\n\t",input_file_path))
  #
  # 3.0. Import the data
  data.import <- read.csv( input_file_path, as.is=TRUE, na.strings="", skip=0)

  fun.helper.CheckRequiredFields( names(data.import), input_file_path)    #
  # 4.0. Columns
  # Kick out if missing minimum of fields
  #
  # Check for and add any missing columns (but not for missing data fields)
  # 4.1. Date, Time, DateTime
  # list
  strCol.DT <- c(myName.Date, myName.Time, myName.DateTime)
  # check for missing
  strCol.DT.Missing <- strCol.DT[strCol.DT %in% colnames(data.import)==FALSE]
  # go to next item if no date, time, or date/time field

  if(length(strCol.DT.Missing) == 3)
  {
    myMsg <- "SKIPPED (Missing Fields, Date/Time)"
    myItems.Skipped <- myItems.Skipped + 1
    myItems.Log[intCounter,2] <- myMsg
    fun.write.log(myItems.Log,myDate,myTime)
    fun.Msg.Status(myMsg, intCounter, intItems.Total, strFile)

    flush.console()
    next
  }
  # go to next item if no (date or time) AND no date/time field  (i.e., only 1 of date or time)
  if(length(strCol.DT.Missing)==2 & myName.DateTime%in%strCol.DT.Missing==TRUE)
  {
    myMsg <- "SKIPPED (Missing Fields, 'Date.Time' and one of 'Date' or 'Time')"
    myItems.Skipped <- myItems.Skipped + 1
    myItems.Log[intCounter,2] <- myMsg
    fun.write.log(myItems.Log,myDate,myTime)
    fun.Msg.Status(myMsg, intCounter, intItems.Total, strFile)

    flush.console()
    next
  }
  #
  # add to df
  data.import[,strCol.DT.Missing] <- NA
  #
  # 4.2.  Check for columns present and reorder columns
  # check for columns present
  strCol.Present <- myNames.Order[myNames.Order %in% colnames(data.import)==TRUE]
  #
  myNames.DataFields.Present <- myNames.DataFields[myNames.DataFields %in% colnames(data.import)==TRUE]

  # kick out if no data fields
  if(length(myNames.DataFields.Present)==0){
    myMsg <- "SKIPPED (Missing Fields, DATA)"
    myItems.Skipped <- myItems.Skipped + 1
    myItems.Log[intCounter,2] <- myMsg
    fun.write.log(myItems.Log,myDate,myTime)
    fun.Msg.Status(myMsg, intCounter, intItems.Total, strFile)
    flush.console()
    # go to next Item
  }
  #
  # reorder Columns (and drop extra columns)
  data.import <- data.import[,strCol.Present]

  # add column that will show if there are any flags in that row
  data.import[,'FlagCount'] <- 0

  # 4.3. Add FLAGS
  strCol.Flags <- myNames.Flags[myNames.Cols4Flags %in% colnames(data.import)==TRUE]
  data.import[, strCol.Flags] <- ""
  
  
  # Add "User" data fields for TN QC step
  # 20170605
  myFlds.UserData <- paste0("User.",myNames.DataFields.Present)
  data.import[,myFlds.UserData] <- ""
  
  #
  # 5.  QC Date and Time fields
  #

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
  #format.DateTime <- fun.DateTimeFormat(data.import[,myName.DateTime],"DateTime")
  # get error if field is NA, need to fix
  # same for section below
  #
  # 20160322, new section, check for NA and fill if needed
  if (length(na.omit(data.import[,myName.DateTime]))==0){
    # move 5.2.1 up here
    myField   <- myName.DateTime
    myFormat  <- pbcc.Format.Date # myFormat.DateTime # "%Y-%m-%d %H:%M:%S"
    flog.debug(paste0('date format could be myFormat==', myFormat))

    data.import[,myField][is.na(data.import[,myField])] <- strftime(paste(data.import[,myName.Date][is.na(data.import[,myField])]
                                                                          ,data.import[,myName.Time][is.na(data.import[,myField])]
                                                                          ,sep=" ")
                                                                    ,format=myFormat,usetz=FALSE)
  }

  format.DateTime <- fun.DateTimeFormat(data.import[,myName.DateTime],"DateTime")

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

  #
  # 5.2. Update DateTime, Date, and Time if NA based on other fields
  # 5.2.1. Update Date_Time if NA (use Date and Time)
  myField   <- myName.DateTime
  myFormat  <- myFormat.DateTime #"%Y-%m-%d %H:%M:%S"

  data.import[,myField][is.na(data.import[,myField])] <- strftime(paste(data.import[,myName.Date][is.na(data.import[,myField])]
                                                                        ,data.import[,myName.Time][is.na(data.import[,myField])]
                                                                        ,sep=" ")
                                                                  ,format=myFormat,usetz=FALSE)
  # 5.2.2. Update Date if NA (use Date_Time)
  myField   <- myName.Date
  myFormat  <- myFormat.Date

  data.import[,myField][is.na(data.import[,myField])] <- strftime(data.import[,myName.DateTime][is.na(data.import[,myField])]
                                                                  ,format=myFormat,usetz=FALSE)
  # 5.2.3. Update Time if NA (use Date_Time)
  myField   <- myName.Time
  myFormat  <- myFormat.Time

  data.import[,myField][is.na(data.import[,myField])] <- as.POSIXct(data.import[,myName.DateTime][is.na(data.import[,myField])]
                                                                    ,format=myFormat,usetz=FALSE)

  # Create Month and Day Field
  #data.import[,"month"] <- as.POSIXlt(data.import$Date)$mon + 1
  #data.import[,"day"]   <- as.POSIXlt(data.import$Date)$mday
  data.import[,myName.Mo] <- as.POSIXlt(data.import$Date)$mon + 1
  data.import[,myName.Day]   <- as.POSIXlt(data.import$Date)$mday
  data.import[,myName.Yr] <- as.POSIXlt(data.import[,myName.Date])$year+1900

  # 6. QC for each Data Type present
  # sub routine adds QC Calcs, QC Test Flags, Assigns overall Flag, and removes QC Calc Fields
  # cycle each data type (manually code)

  #
  # skip if not present
  #
  # 6.1. WaterTemp
  myField <- myName.WaterTemp

  if(myField %in% myNames.DataFields.Present == TRUE)
  {
    flog.debug(paste0('Processing data for ', myField))

    data.import <- fun.CalcQCStats(data.import
                                   ,myField
                                   ,myThresh.Gross.Fail.Hi.WaterTemp
                                   ,myThresh.Gross.Fail.Lo.WaterTemp
                                   ,myThresh.Gross.Suspect.Hi.WaterTemp
                                   ,myThresh.Gross.Suspect.Lo.WaterTemp
                                   ,myThresh.Spike.Hi.WaterTemp
                                   ,myThresh.Spike.Lo.WaterTemp
                                   ,myThresh.RoC.SD.period.WaterTemp
                                   ,myThresh.RoC.SD.number.WaterTemp
                                   ,myThresh.Flat.Hi.WaterTemp
                                   ,myThresh.Flat.Lo.WaterTemp
                                   ,myThresh.Flat.Tolerance.WaterTemp)
  }

  #
  # 6.2. AirTemp
  myField <- myName.AirTemp
  if(myField %in% myNames.DataFields.Present==TRUE)
  {
    flog.debug(paste0('Processing data for ', myField))
    data.import <- fun.CalcQCStats(data.import
                                   ,myField
                                   ,myThresh.Gross.Fail.Hi.AirTemp
                                   ,myThresh.Gross.Fail.Lo.AirTemp
                                   ,myThresh.Gross.Suspect.Hi.AirTemp
                                   ,myThresh.Gross.Suspect.Lo.AirTemp
                                   ,myThresh.Spike.Hi.AirTemp
                                   ,myThresh.Spike.Lo.AirTemp
                                   ,myThresh.RoC.SD.period.AirTemp
                                   ,myThresh.RoC.SD.number.AirTemp
                                   ,myThresh.Flat.Hi.AirTemp
                                   ,myThresh.Flat.Lo.AirTemp
                                   ,myThresh.Flat.Tolerance.AirTemp)
  }

  #
  # 6.3. WaterP
  myField <- myName.WaterP

  if(myField %in% myNames.DataFields.Present==TRUE)
  {
    flog.debug(paste0('Processing data for ', myField))
    data.import <- fun.CalcQCStats(data.import
                                   ,myField
                                   ,myThresh.Gross.Fail.Hi.WaterP
                                   ,myThresh.Gross.Fail.Lo.WaterP
                                   ,myThresh.Gross.Suspect.Hi.WaterP
                                   ,myThresh.Gross.Suspect.Lo.WaterP
                                   ,myThresh.Spike.Hi.WaterP
                                   ,myThresh.Spike.Lo.WaterP
                                   ,myThresh.RoC.SD.period.WaterP
                                   ,myThresh.RoC.SD.number.WaterP
                                   ,myThresh.Flat.Hi.WaterP
                                   ,myThresh.Flat.Lo.WaterP
                                   ,myThresh.Flat.Tolerance.WaterP)
  }

  # 6.4. AirBP
  myField <- myName.AirBP

  if(myField %in% myNames.DataFields.Present==TRUE)
  {
    flog.debug(paste0('Processing data for ', myField))
    data.import <- fun.CalcQCStats(data.import
                                   ,myField
                                   ,myThresh.Gross.Fail.Hi.AirBP
                                   ,myThresh.Gross.Fail.Lo.AirBP
                                   ,myThresh.Gross.Suspect.Hi.AirBP
                                   ,myThresh.Gross.Suspect.Lo.AirBP
                                   ,myThresh.Spike.Hi.AirBP
                                   ,myThresh.Spike.Lo.AirBP
                                   ,myThresh.RoC.SD.period.AirBP
                                   ,myThresh.RoC.SD.number.AirBP
                                   ,myThresh.Flat.Hi.AirBP
                                   ,myThresh.Flat.Lo.AirBP
                                   ,myThresh.Flat.Tolerance.AirBP)
  }
  #
  # 6.5. SensorDepth
  myField <- myName.SensorDepth

  if(myField %in% myNames.DataFields.Present==TRUE)
  {
    flog.debug(paste0('Processing data for ', myField))
    data.import <- fun.CalcQCStats(data.import
                                   ,myField
                                   ,myThresh.Gross.Fail.Hi.SensorDepth
                                   ,myThresh.Gross.Fail.Lo.SensorDepth
                                   ,myThresh.Gross.Suspect.Hi.SensorDepth
                                   ,myThresh.Gross.Suspect.Lo.SensorDepth
                                   ,myThresh.Spike.Hi.SensorDepth
                                   ,myThresh.Spike.Lo.SensorDepth
                                   ,myThresh.RoC.SD.period.SensorDepth
                                   ,myThresh.RoC.SD.number.SensorDepth
                                   ,myThresh.Flat.Hi.SensorDepth
                                   ,myThresh.Flat.Lo.SensorDepth
                                   ,myThresh.Flat.Tolerance.SensorDepth)
    #
  }
  #
  # 6.5. Discharge
  myField <- myName.Discharge

  if(myField %in% myNames.DataFields.Present==TRUE)
  {
    flog.debug(paste0('Processing data for ', myField))

    data.import <- fun.CalcQCStats(data.import
                                   ,myField
                                   ,myThresh.Gross.Fail.Hi.Discharge
                                   ,myThresh.Gross.Fail.Lo.Discharge
                                   ,myThresh.Gross.Suspect.Hi.Discharge
                                   ,myThresh.Gross.Suspect.Lo.Discharge
                                   ,myThresh.Spike.Hi.Discharge
                                   ,myThresh.Spike.Lo.Discharge
                                   ,myThresh.RoC.SD.period.Discharge
                                   ,myThresh.RoC.SD.number.Discharge
                                   ,myThresh.Flat.Hi.Discharge
                                   ,myThresh.Flat.Lo.Discharge
                                   ,myThresh.Flat.Tolerance.Discharge)
  }
  # B.6.07. Conductivity
      myField <-myName.Cond
      if(myField %in% myNames.DataFields.Present==TRUE){##IF.myField.START
          #
            flog.debug(paste0('Processing data for ', myField))
          # myMsg.data <- "Cond"
            # myMsg <- paste("WORKING (QC Tests and Flags - ",myMsg.data,")",sep="")
            # myItems.Complete <- myItems.Complete + 1
            # myItems.Log[intCounter,2] <- myMsg
            # fun.Msg.Status(myMsg, intCounter, intItems.Total, strFile)
            # flush.console()
            #
            data.import <- fun.CalcQCStats(data.import
                                         ,myField
                                         ,myThresh.Gross.Fail.Hi.Cond
                                         ,myThresh.Gross.Fail.Lo.Cond
                                         ,myThresh.Gross.Suspect.Hi.Cond
                                         ,myThresh.Gross.Suspect.Lo.Cond
                                         ,myThresh.Spike.Hi.Cond
                                         ,myThresh.Spike.Lo.Cond
                                         ,myThresh.RoC.SD.period.Cond
                                         ,myThresh.RoC.SD.number.Cond
                                         ,myThresh.Flat.Hi.Cond
                                         ,myThresh.Flat.Lo.Cond
                                         ,myThresh.Flat.Tolerance.Cond)

            }##IF.myField.END
      #
        # B.6.08. Dissolved Oxygen
        myField <-myName.DO
      if(myField %in% myNames.DataFields.Present==TRUE){##IF.myField.START
          #
            flog.debug(paste0('Processing data for ', myField))
          # myMsg.data <- "DO"
            # myMsg <- paste("WORKING (QC Tests and Flags - ",myMsg.data,")",sep="")
            # myItems.Complete <- myItems.Complete + 1
            # myItems.Log[intCounter,2] <- myMsg
            # fun.Msg.Status(myMsg, intCounter, intItems.Total, strFile)
            # flush.console()
            #
            data.import <- fun.CalcQCStats(data.import
                                           ,myField
                                           ,myThresh.Gross.Fail.Hi.DO
                                           ,myThresh.Gross.Fail.Lo.DO
                                           ,myThresh.Gross.Suspect.Hi.DO
                                           ,myThresh.Gross.Suspect.Lo.DO
                                           ,myThresh.Spike.Hi.DO
                                           ,myThresh.Spike.Lo.DO
                                           ,myThresh.RoC.SD.period.DO
                                           ,myThresh.RoC.SD.number.DO
                                           ,myThresh.Flat.Hi.DO
                                           ,myThresh.Flat.Lo.DO
                                           ,myThresh.Flat.Tolerance.DO)
          }##IF.myField.END
      #
        # B.6.09. pH
        myField <-myName.pH
      if(myField %in% myNames.DataFields.Present==TRUE){##IF.myField.START
          #
            flog.debug(paste0('Processing data for ', myField))
          # myMsg.data <- "pH"
            # myMsg <- paste("WORKING (QC Tests and Flags - ",myMsg.data,")",sep="")
            # myItems.Complete <- myItems.Complete + 1
            # myItems.Log[intCounter,2] <- myMsg
            # fun.Msg.Status(myMsg, intCounter, intItems.Total, strFile)
            # flush.console()
            #
            data.import <- fun.CalcQCStats(data.import
                                           ,myField
                                           ,myThresh.Gross.Fail.Hi.pH
                                           ,myThresh.Gross.Fail.Lo.pH
                                           ,myThresh.Gross.Suspect.Hi.pH
                                           ,myThresh.Gross.Suspect.Lo.pH
                                           ,myThresh.Spike.Hi.pH
                                           ,myThresh.Spike.Lo.pH
                                           ,myThresh.RoC.SD.period.pH
                                           ,myThresh.RoC.SD.number.pH
                                           ,myThresh.Flat.Hi.pH
                                           ,myThresh.Flat.Lo.pH
                                           ,myThresh.Flat.Tolerance.pH)
          }##IF.myField.END
      #
        # B.6.10. Turbidity
        myField <-myName.Turbidity
      if(myField %in% myNames.DataFields.Present==TRUE){##IF.myField.START
          #
            flog.debug(paste0('Processing data for ', myField))
          # myMsg.data <- "Turbidity"
            # myMsg <- paste("WORKING (QC Tests and Flags - ",myMsg.data,")",sep="")
            # myItems.Complete <- myItems.Complete + 1
            # myItems.Log[intCounter,2] <- myMsg
            # fun.Msg.Status(myMsg, intCounter, intItems.Total, strFile)
            # flush.console()
            #
            data.import <- fun.CalcQCStats(data.import
                                           ,myField
                                           ,myThresh.Gross.Fail.Hi.Turbidity
                                           ,myThresh.Gross.Fail.Lo.Turbidity
                                           ,myThresh.Gross.Suspect.Hi.Turbidity
                                           ,myThresh.Gross.Suspect.Lo.Turbidity
                                           ,myThresh.Spike.Hi.Turbidity
                                           ,myThresh.Spike.Lo.Turbidity
                                           ,myThresh.RoC.SD.period.Turbidity
                                           ,myThresh.RoC.SD.number.Turbidity
                                           ,myThresh.Flat.Hi.Turbidity
                                           ,myThresh.Flat.Lo.Turbidity
                                           ,myThresh.Flat.Tolerance.Turbidity)
          }##IF.myField.END
      #
        # B.6.11. Chlorophyll a
        myField <-myName.Chlorophylla
      if(myField %in% myNames.DataFields.Present==TRUE){##IF.myField.START
          #
            flog.debug(paste0('Processing data for ', myField))
          # myMsg.data <- "Chlorophylla"
            # myMsg <- paste("WORKING (QC Tests and Flags - ",myMsg.data,")",sep="")
            # myItems.Complete <- myItems.Complete + 1
            # myItems.Log[intCounter,2] <- myMsg
            # fun.Msg.Status(myMsg, intCounter, intItems.Total, strFile)
            # flush.console()
            #
            data.import <- fun.CalcQCStats(data.import
                                           ,myField
                                           ,myThresh.Gross.Fail.Hi.Chlorophylla
                                           ,myThresh.Gross.Fail.Lo.Chlorophylla
                                           ,myThresh.Gross.Suspect.Hi.Chlorophylla
                                           ,myThresh.Gross.Suspect.Lo.Chlorophylla
                                           ,myThresh.Spike.Hi.Chlorophylla
                                           ,myThresh.Spike.Lo.Chlorophylla
                                           ,myThresh.RoC.SD.period.Chlorophylla
                                           ,myThresh.RoC.SD.number.Chlorophylla
                                           ,myThresh.Flat.Hi.Chlorophylla
                                           ,myThresh.Flat.Lo.Chlorophylla
                                           ,myThresh.Flat.Tolerance.Chlorophylla)
          }##IF.myField.END
  #############################
  #
  # Names of columns for QC Calculations and Tests with Flags for each data column present
  # combine so can check for and remove later.
  myNames.DataFields.Present.QCCalcs   <- as.vector(t(outer(myNames.DataFields.Present,myNames.QCCalcs,paste,sep=".")))

  myNames.Flags.QCTests <- paste0("Flag.",as.vector(t(outer( myNames.QCTests, myNames.DataFields.Present, paste, sep="."))))


  ###########################
  #
  # create a CSV file that is used by the QC Report function
  #
  ###############

  strFile.Out.Prefix <- "QC"

  output_file_name <- paste(strFile.Out.Prefix, input_file_name, sep=config.FileName.Delimiter)

  output_file_path = file.path(export_folder, output_file_name)

  # 10.2. Save to File the data (overwrites any existing file).

  write.csv(data.import, file = output_file_path, quote=FALSE, row.names=FALSE)
  #
  flog.debug(paste0('Wrote CSV file used to generate Report' , "\n\t", output_file_path))

  ##################
  #
  # Run QC Report
  #
  ####################
  fun.Report_file(
    output_file_name,
    import_folder,
    export_folder
  )

  #~~~~~~~~~~~~~~~~~~
  #
  # Format final CSV file
  #
  #~~~~~~~~~~~~~~~~~~
  #
  # 20170613, Reorder columns  
  # Column Order ####
  # Data Fields (only 5, not all, will have to edit for pH, DO, etc)
  ColOrder.DataFields <- c(myName.WaterP
                           ,myName.WaterTemp
                           ,myName.AirBP
                           ,myName.SensorDepth
                           ,myName.AirTemp)
  # Flags
  Flags.QCTests <- paste(myName.Flag,c("Gross","RoC","Spike","Flat"),sep=".")
  # Final Order
  ## apply creates all combinations of 2 vectors
  ColOrder.Final <- c(myName.SiteID
                      ,myName.DateTime
                      ,ColOrder.DataFields
                      ,myName.RowID.Water
                      ,paste0("User.",ColOrder.DataFields)
                      ,"FlagCount"
                      ,myName.Flag.DateTime
                      ,paste(myName.Flag,ColOrder.DataFields,sep=".")
                      ,apply(expand.grid(Flags.QCTests,ColOrder.DataFields), 1, paste, collapse=".")
  )
  # Final Cols that appear in data.import
  ColOrder.Final.T <- ColOrder.Final[ColOrder.Final %in% names(data.import) == TRUE]
  # Final Cols that *do not* appear in data.import
  ColOrder.Final.F <- ColOrder.Final[ColOrder.Final %in% names(data.import) == FALSE]
  # Extra Columns in data.import
  ColOrder.Final.Extra <- names(data.import)[names(data.import) %in% ColOrder.Final == FALSE]
  # Col Order
  data.import <- data.import[,c(ColOrder.Final.T,ColOrder.Final.Extra)]
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  # remove all the 'P' (passing) flags from each Flag.* column just to simplify the CSV file
  #
  for(j in names(data.import))
  {
    column_name_parts <- strsplit(j, '[.]')

    if (column_name_parts[[1]][1] == 'Flag')
    {
      data.import[,j][data.import[,j]=="P"] <- ""
    }
  }

  # set the 'FlagCount' values to the sum of Flag columns that have something other than 'P' in them
  #
  data.import$FlagCount <- rowSums(data.import[, strCol.Flags] != "")

  # remove columns that are entirely empty
  #
#  data.import <- data.import[, ! apply(data.import, 2, function(x) all(gsub(" ", "", x)=="", na.rm=TRUE))]

  # remove some redundant columns to simplify the CSV file
  #
#  remove_columns = c('day', 'month', 'Date', 'Time')
#  data.import <- data.import[, ! names(data.import) %in% remove_columns, drop = F]

  # write the csv file in its final format
  #
  write.csv(data.import, file = output_file_path, quote=FALSE, row.names=FALSE)

  #
  flog.debug(paste0('Wrote CSV file again' , "\n\t", output_file_path))

  flog.debug(paste0('Finished processing file' , "\n\t", input_file_path))

  # Remove data (import)
  rm(data.import)
}

fun.QC <- function(fun.myData.SiteID
                   ,fun.myData.Type
                   ,fun.myData.DateRange.Start
                   ,fun.myData.DateRange.End
                   ,fun.myDir.SUB.import
                   ,fun.myDir.SUB.export) {
  #
  # Convert Data Type to proper case

  #fun.myData.Type <- fun.CamelCase(fun.myData.Type)

  #
  # data directories
  myDir.data.import <- fun.myDir.SUB.import
  myDir.data.export <- fun.myDir.SUB.export

  #
  myDate <- format(Sys.Date(),"%Y%m%d")
  myTime <- format(Sys.time(),"%H%M%S")

  # Verify input dates, if blank, NA, or null use all data
  # if DateRange.Start is null or "" then assign it 1900-01-01
  if (is.na(fun.myData.DateRange.Start) == TRUE || fun.myData.DateRange.Start=="")
  {
    fun.myData.DateRange.Start <- DateRange.Start.Default
  }

  # if DateRange.End is null or "" then assign it today
  if (is.na(fun.myData.DateRange.End)==TRUE || fun.myData.DateRange.End=="")
  {
    fun.myData.DateRange.End <- DateRange.End.Default
  }

  # Read in list of files to work on, uses all files matching pattern ("\\.csv$")
  # ## if change formats will have to make modifications (pattern, import, export)
  filename_pattern = ".+_\\d{8}_\\d{8}-\\d{8}-\\d{6}.csv$"
  filename_pattern = ".+_\\d{8}_\\d{8}.csv$"
  all_files = list.files(path=myDir.data.import) # , pattern=filename_pattern)
  print(paste0('match filename_pattern==', paste(all_files, collapse = ',')))
  # only interested in files if they have the SiteID and Type in the filename
  site_files <- grep(fun.myData.SiteID, all_files, value = TRUE)
  print(paste0('match SiteID_pattern==', paste(site_files, collapse = ',')))
  files2process <- grep(fun.myData.Type,   site_files, value = TRUE)
  print(paste0('match data type_pattern==', paste(files2process, collapse = ',')))
  print(paste(files2process, collapse = ','))

  #
  # Define Counters for the Loop
  intCounter <- 0
  intCounter.Stop <- length(files2process)
  intItems.Total <- intCounter.Stop

  if (intItems.Total == 0)
  {
    flog.debug(paste0("No files matching pattern ", filename_pattern, " to process in folder\n\t", myDir.data.import))
    return()
  }
  flog.debug(paste0("Total files to process = ", intItems.Total))

  myItems.Complete  <- 0
  myItems.Skipped   <- 0
  myFileTypeNum.Air <- 0
  myFileTypeNum.Water <- 0

  # Create Log file
  ##  List of all items (files)
  myItems.ALL <- as.vector(unique(files2process))

  # create log file for processing results of items
  #myItems.Log <- data.frame(cbind(myItems.ALL,NA),stringsAsFactors=FALSE)
  myItems.Log <- data.frame(ItemID = 1:intItems.Total, Status = NA, ItemName = myItems.ALL)

  # Error if no files to process or no files in dir

  # Start Time (used to determine run time at end)
  myTime.Start <- Sys.time()

  # Perform a data manipulation on the data as a new file
  # Could use for (n in files2process) but prefer the control of a counter
  while (intCounter < intCounter.Stop)
  {
    #
    # 0. Increase the Counter
    intCounter <- intCounter + 1
    #
    # 1.0. File Name, Define
    strFile = files2process[intCounter]

    strFile.source.full_name = file.path(myDir.data.import, strFile)

    fun.QC_file(strFile, myDir.data.import, myDir.data.export)

    next()

    #
    # 2. Check File and skip if doesn't match user defined parameters
    #
    if(file.info( strFile.source.full_name )$size == 0)
    {
      flog.debug(paste0("File is empty (size 0 bytes).  Skipping...\n\t",strFile.source.full_name))
      next
    }


    flog.debug(paste0("Begin processing file\n\t",strFile.source.full_name))


    # 1.1. File Name, Parse
    strFile.Base  <- substr(strFile,1,nchar(strFile)-nchar(".csv"))
    strFile.parts <- strsplit(strFile.Base, config.FileName.Delimiter)

    strFile.SiteID   <- strFile.parts[[1]][1]
    strFile.DataType <- strFile.parts[[1]][2]

    # Convert Data Type to Camel Case
    # strFile.DataType <- fun.CamelCase(strFile.DataType)

    strFile.Date.Start <- as.Date(strFile.parts[[1]][3],"%Y%m%d")
    strFile.Date.End   <- as.Date(strFile.parts[[1]][4],"%Y%m%d")

    # 2.2. Check SiteID
    # if not in provided site list then skip
    if(strFile.SiteID %in% fun.myData.SiteID == FALSE)
    {
      myMsg <- "SKIPPED (Non-Match, SiteID)"
      myItems.Skipped <- myItems.Skipped + 1
      myItems.Log[intCounter,2] <- myMsg
      fun.write.log(myItems.Log,myDate,myTime)
      fun.Msg.Status(myMsg, intCounter, intItems.Total, strFile)

      flush.console()
      next
    }
    # 2.3. Check DataType
    # if not equal go to next file (handles both Air and Water)
    if (strFile.DataType %in% fun.myData.Type == FALSE)
    {
      myMsg <- "SKIPPED (Non-Match, DataType)"
      myItems.Skipped <- myItems.Skipped + 1
      myItems.Log[intCounter,2] <- myMsg
      fun.write.log(myItems.Log,myDate,myTime)
      fun.Msg.Status(myMsg, intCounter, intItems.Total, strFile)

      flush.console()
      next
    }
    # 2.4. Check Dates
    # 2.4.2.1. Check File.Date.Start (if file end < my Start then next)
    if(strFile.Date.End<fun.myData.DateRange.Start)
    {
      # inform user of progress and update LOG
      myMsg <- "SKIPPED (Non-Match, Start Date)"
      myItems.Skipped <- myItems.Skipped + 1
      myItems.Log[intCounter,2] <- myMsg
      fun.write.log(myItems.Log,myDate,myTime)
      fun.Msg.Status(myMsg, intCounter, intItems.Total, strFile)

      flush.console()
      next
    }
    # 2.4.2.2. Check File.Date.End (if file Start > my End then next)
    if(strFile.Date.Start>fun.myData.DateRange.End) {

      myMsg <- "SKIPPED (Non-Match, End Date)"
      myItems.Skipped <- myItems.Skipped + 1
      myItems.Log[intCounter,2] <- myMsg
      fun.write.log(myItems.Log,myDate,myTime)
      fun.Msg.Status(myMsg, intCounter, intItems.Total, strFile)

      flush.console()
      next
    }
    #
    # 3.0. Import the data
    data.import <- read.csv( strFile.source.full_name, as.is=TRUE, na.strings="", skip=0)
    # myMsg <- paste0("
    #
    #                 Column names in current file are below.
    #                 ",list(data.import))
    #
    # stop(myMsg)
    #
    # QC required fields: SiteID & (DateTime | (Date & Time))
    fun.helper.CheckRequiredFields( names(data.import), file.path(myDir.data.import, strFile))

    #
    # 4.0. Columns
    # Kick out if missing minimum of fields
    #
    # Check for and add any missing columns (but not for missing data fields)
    # 4.1. Date, Time, DateTime
    # list
    strCol.DT <- c(myName.Date, myName.Time, myName.DateTime)
    # check for missing
    strCol.DT.Missing <- strCol.DT[strCol.DT %in% colnames(data.import)==FALSE]
    # go to next item if no date, time, or date/time field

    if(length(strCol.DT.Missing) == 3)
    {
      myMsg <- "SKIPPED (Missing Fields, Date/Time)"
      myItems.Skipped <- myItems.Skipped + 1
      myItems.Log[intCounter,2] <- myMsg
      fun.write.log(myItems.Log,myDate,myTime)
      fun.Msg.Status(myMsg, intCounter, intItems.Total, strFile)

      flush.console()
      next
    }
    # go to next item if no (date or time) AND no date/time field  (i.e., only 1 of date or time)
    if(length(strCol.DT.Missing)==2 & myName.DateTime%in%strCol.DT.Missing==TRUE)
    {
      myMsg <- "SKIPPED (Missing Fields, 'Date.Time' and one of 'Date' or 'Time')"
      myItems.Skipped <- myItems.Skipped + 1
      myItems.Log[intCounter,2] <- myMsg
      fun.write.log(myItems.Log,myDate,myTime)
      fun.Msg.Status(myMsg, intCounter, intItems.Total, strFile)

      flush.console()
      next
    }
    #
    # add to df
    data.import[,strCol.DT.Missing] <- NA
    #
    # 4.2.  Check for columns present and reorder columns
    # check for columns present
    strCol.Present <- myNames.Order[myNames.Order %in% colnames(data.import)==TRUE]
    #
    myNames.DataFields.Present <- myNames.DataFields[myNames.DataFields %in% colnames(data.import)==TRUE]

    # kick out if no data fields
    if(length(myNames.DataFields.Present)==0){
      myMsg <- "SKIPPED (Missing Fields, DATA)"
      myItems.Skipped <- myItems.Skipped + 1
      myItems.Log[intCounter,2] <- myMsg
      fun.write.log(myItems.Log,myDate,myTime)
      fun.Msg.Status(myMsg, intCounter, intItems.Total, strFile)
      flush.console()
      # go to next Item
    }
    #
    # reorder Columns (and drop extra columns)
    data.import <- data.import[,strCol.Present]

    # add column that will show if there are any flags in that row
    data.import[,'FlagCount'] <- 0

    # 4.3. Add FLAGS
    strCol.Flags <- myNames.Flags[myNames.Cols4Flags %in% colnames(data.import)==TRUE]
    data.import[, strCol.Flags] <- ""

    #
    # 5.  QC Date and Time fields
    #

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
    #format.DateTime <- fun.DateTimeFormat(data.import[,myName.DateTime],"DateTime")
    # get error if field is NA, need to fix
    # same for section below
    #
    # 20160322, new section, check for NA and fill if needed
    if (length(na.omit(data.import[,myName.DateTime]))==0){
      # move 5.2.1 up here
      myField   <- myName.DateTime
      myFormat  <- pbcc.Format.Date # myFormat.DateTime # "%Y-%m-%d %H:%M:%S"
      flog.debug(paste0('date format could be myFormat==', myFormat))

      data.import[,myField][is.na(data.import[,myField])] <- strftime(paste(data.import[,myName.Date][is.na(data.import[,myField])]
                                                                            ,data.import[,myName.Time][is.na(data.import[,myField])]
                                                                            ,sep=" ")
                                                                      ,format=myFormat,usetz=FALSE)
    }

    format.DateTime <- fun.DateTimeFormat(data.import[,myName.DateTime],"DateTime")

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

    #
    # 5.2. Update DateTime, Date, and Time if NA based on other fields
    # 5.2.1. Update Date_Time if NA (use Date and Time)
    myField   <- myName.DateTime
    myFormat  <- myFormat.DateTime #"%Y-%m-%d %H:%M:%S"

    data.import[,myField][is.na(data.import[,myField])] <- strftime(paste(data.import[,myName.Date][is.na(data.import[,myField])]
                                                                          ,data.import[,myName.Time][is.na(data.import[,myField])]
                                                                          ,sep=" ")
                                                                    ,format=myFormat,usetz=FALSE)
    # 5.2.2. Update Date if NA (use Date_Time)
    myField   <- myName.Date
    myFormat  <- myFormat.Date

    data.import[,myField][is.na(data.import[,myField])] <- strftime(data.import[,myName.DateTime][is.na(data.import[,myField])]
                                                                    ,format=myFormat,usetz=FALSE)
    # 5.2.3. Update Time if NA (use Date_Time)
    myField   <- myName.Time
    myFormat  <- myFormat.Time

    data.import[,myField][is.na(data.import[,myField])] <- as.POSIXct(data.import[,myName.DateTime][is.na(data.import[,myField])]
                                                                      ,format=myFormat,usetz=FALSE)

    # Create Month and Day Field
    data.import[,myName.Mo] <- as.POSIXlt(data.import$Date)$mon + 1
    data.import[,myName.Day]   <- as.POSIXlt(data.import$Date)$mday
    #data.import[,myName.Day] <- as.POSIXlt(data.import[,myName.Date])$mday
    data.import[,myName.Yr] <- as.POSIXlt(data.import[,myName.Date])$year+1900

    # 6. QC for each Data Type present
    # sub routine adds QC Calcs, QC Test Flags, Assigns overall Flag, and removes QC Calc Fields
    # cycle each data type (manually code)

    #
    # skip if not present
    #
    # 6.1. WaterTemp
    myField <- myName.WaterTemp

    if(myField %in% myNames.DataFields.Present == TRUE)
    {
      flog.debug(paste0('Processing data for ', myField))

      data.import <- fun.CalcQCStats(data.import
                                     ,myField
                                     ,myThresh.Gross.Fail.Hi.WaterTemp
                                     ,myThresh.Gross.Fail.Lo.WaterTemp
                                     ,myThresh.Gross.Suspect.Hi.WaterTemp
                                     ,myThresh.Gross.Suspect.Lo.WaterTemp
                                     ,myThresh.Spike.Hi.WaterTemp
                                     ,myThresh.Spike.Lo.WaterTemp
                                     ,myThresh.RoC.SD.period.WaterTemp
                                     ,myThresh.RoC.SD.number.WaterTemp
                                     ,myThresh.Flat.Hi.WaterTemp
                                     ,myThresh.Flat.Lo.WaterTemp
                                     ,myThresh.Flat.Tolerance.WaterTemp)
    }

    #
    # 6.2. AirTemp
    myField <- myName.AirTemp
    if(myField %in% myNames.DataFields.Present==TRUE)
    {
      flog.debug(paste0('Processing data for ', myField))
      data.import <- fun.CalcQCStats(data.import
                                  ,myField
                                  ,myThresh.Gross.Fail.Hi.AirTemp
                                  ,myThresh.Gross.Fail.Lo.AirTemp
                                  ,myThresh.Gross.Suspect.Hi.AirTemp
                                  ,myThresh.Gross.Suspect.Lo.AirTemp
                                  ,myThresh.Spike.Hi.AirTemp
                                  ,myThresh.Spike.Lo.AirTemp
                                  ,myThresh.RoC.SD.period.AirTemp
                                  ,myThresh.RoC.SD.number.AirTemp
                                  ,myThresh.Flat.Hi.AirTemp
                                  ,myThresh.Flat.Lo.AirTemp
                                  ,myThresh.Flat.Tolerance.AirTemp)
    }

    #
    # 6.3. WaterP
    myField <- myName.WaterP

    if(myField %in% myNames.DataFields.Present==TRUE)
    {
      flog.debug(paste0('Processing data for ', myField))
      data.import <- fun.CalcQCStats(data.import
                                  ,myField
                                  ,myThresh.Gross.Fail.Hi.WaterP
                                  ,myThresh.Gross.Fail.Lo.WaterP
                                  ,myThresh.Gross.Suspect.Hi.WaterP
                                  ,myThresh.Gross.Suspect.Lo.WaterP
                                  ,myThresh.Spike.Hi.WaterP
                                  ,myThresh.Spike.Lo.WaterP
                                  ,myThresh.RoC.SD.period.WaterP
                                  ,myThresh.RoC.SD.number.WaterP
                                  ,myThresh.Flat.Hi.WaterP
                                  ,myThresh.Flat.Lo.WaterP
                                  ,myThresh.Flat.Tolerance.WaterP)
    }

    # 6.4. AirBP
    myField <- myName.AirBP

    if(myField %in% myNames.DataFields.Present==TRUE)
    {
      flog.debug(paste0('Processing data for ', myField))
      data.import <- fun.CalcQCStats(data.import
                                  ,myField
                                  ,myThresh.Gross.Fail.Hi.AirBP
                                  ,myThresh.Gross.Fail.Lo.AirBP
                                  ,myThresh.Gross.Suspect.Hi.AirBP
                                  ,myThresh.Gross.Suspect.Lo.AirBP
                                  ,myThresh.Spike.Hi.AirBP
                                  ,myThresh.Spike.Lo.AirBP
                                  ,myThresh.RoC.SD.period.AirBP
                                  ,myThresh.RoC.SD.number.AirBP
                                  ,myThresh.Flat.Hi.AirBP
                                  ,myThresh.Flat.Lo.AirBP
                                  ,myThresh.Flat.Tolerance.AirBP)
    }
    #
    # 6.5. SensorDepth
    myField <- myName.SensorDepth

    if(myField %in% myNames.DataFields.Present==TRUE)
    {
      flog.debug(paste0('Processing data for ', myField))
      data.import <- fun.CalcQCStats(data.import
                                  ,myField
                                  ,myThresh.Gross.Fail.Hi.SensorDepth
                                  ,myThresh.Gross.Fail.Lo.SensorDepth
                                  ,myThresh.Gross.Suspect.Hi.SensorDepth
                                  ,myThresh.Gross.Suspect.Lo.SensorDepth
                                  ,myThresh.Spike.Hi.SensorDepth
                                  ,myThresh.Spike.Lo.SensorDepth
                                  ,myThresh.RoC.SD.period.SensorDepth
                                  ,myThresh.RoC.SD.number.SensorDepth
                                  ,myThresh.Flat.Hi.SensorDepth
                                  ,myThresh.Flat.Lo.SensorDepth
                                  ,myThresh.Flat.Tolerance.SensorDepth)
      #
    }
    #
    # 6.5. Discharge
    myField <- myName.Discharge

    if(myField %in% myNames.DataFields.Present==TRUE)
    {
      flog.debug(paste0('Processing data for ', myField))

      data.import <- fun.CalcQCStats(data.import
                                     ,myField
                                     ,myThresh.Gross.Fail.Hi.Discharge
                                     ,myThresh.Gross.Fail.Lo.Discharge
                                     ,myThresh.Gross.Suspect.Hi.Discharge
                                     ,myThresh.Gross.Suspect.Lo.Discharge
                                     ,myThresh.Spike.Hi.Discharge
                                     ,myThresh.Spike.Lo.Discharge
                                     ,myThresh.RoC.SD.period.Discharge
                                     ,myThresh.RoC.SD.number.Discharge
                                     ,myThresh.Flat.Hi.Discharge
                                     ,myThresh.Flat.Lo.Discharge
                                     ,myThresh.Flat.Tolerance.Discharge)
    }
    # B.6.07. Conductivity
    myField <-myName.Cond
    if(myField %in% myNames.DataFields.Present==TRUE){##IF.myField.START
      #
      flog.debug(paste0('Processing data for ', myField))
      # myMsg.data <- "Cond"
      # myMsg <- paste("WORKING (QC Tests and Flags - ",myMsg.data,")",sep="")
      # myItems.Complete <- myItems.Complete + 1
      # myItems.Log[intCounter,2] <- myMsg
      # fun.Msg.Status(myMsg, intCounter, intItems.Total, strFile)
      # flush.console()
      #
      data.import <- fun.CalcQCStats(data.import
                                     ,myField
                                     ,myThresh.Gross.Fail.Hi.Cond
                                     ,myThresh.Gross.Fail.Lo.Cond
                                     ,myThresh.Gross.Suspect.Hi.Cond
                                     ,myThresh.Gross.Suspect.Lo.Cond
                                     ,myThresh.Spike.Hi.Cond
                                     ,myThresh.Spike.Lo.Cond
                                     ,myThresh.RoC.SD.period.Cond
                                     ,myThresh.RoC.SD.number.Cond
                                     ,myThresh.Flat.Hi.Cond
                                     ,myThresh.Flat.Lo.Cond
                                     ,myThresh.Flat.Tolerance.Cond)

    }##IF.myField.END
    #
    # B.6.08. Dissolved Oxygen
    myField <-myName.DO
    if(myField %in% myNames.DataFields.Present==TRUE){##IF.myField.START
      #
      flog.debug(paste0('Processing data for ', myField))
      # myMsg.data <- "DO"
      # myMsg <- paste("WORKING (QC Tests and Flags - ",myMsg.data,")",sep="")
      # myItems.Complete <- myItems.Complete + 1
      # myItems.Log[intCounter,2] <- myMsg
      # fun.Msg.Status(myMsg, intCounter, intItems.Total, strFile)
      # flush.console()
      #
      data.import <- fun.CalcQCStats(data.import
                                     ,myField
                                     ,myThresh.Gross.Fail.Hi.DO
                                     ,myThresh.Gross.Fail.Lo.DO
                                     ,myThresh.Gross.Suspect.Hi.DO
                                     ,myThresh.Gross.Suspect.Lo.DO
                                     ,myThresh.Spike.Hi.DO
                                     ,myThresh.Spike.Lo.DO
                                     ,myThresh.RoC.SD.period.DO
                                     ,myThresh.RoC.SD.number.DO
                                     ,myThresh.Flat.Hi.DO
                                     ,myThresh.Flat.Lo.DO
                                     ,myThresh.Flat.Tolerance.DO)
    }##IF.myField.END
    #
    # B.6.09. pH
    myField <-myName.pH
    if(myField %in% myNames.DataFields.Present==TRUE){##IF.myField.START
      #
      flog.debug(paste0('Processing data for ', myField))
      # myMsg.data <- "pH"
      # myMsg <- paste("WORKING (QC Tests and Flags - ",myMsg.data,")",sep="")
      # myItems.Complete <- myItems.Complete + 1
      # myItems.Log[intCounter,2] <- myMsg
      # fun.Msg.Status(myMsg, intCounter, intItems.Total, strFile)
      # flush.console()
      #
      data.import <- fun.CalcQCStats(data.import
                                     ,myField
                                     ,myThresh.Gross.Fail.Hi.pH
                                     ,myThresh.Gross.Fail.Lo.pH
                                     ,myThresh.Gross.Suspect.Hi.pH
                                     ,myThresh.Gross.Suspect.Lo.pH
                                     ,myThresh.Spike.Hi.pH
                                     ,myThresh.Spike.Lo.pH
                                     ,myThresh.RoC.SD.period.pH
                                     ,myThresh.RoC.SD.number.pH
                                     ,myThresh.Flat.Hi.pH
                                     ,myThresh.Flat.Lo.pH
                                     ,myThresh.Flat.Tolerance.pH)
    }##IF.myField.END
    #
    # B.6.10. Turbidity
    myField <-myName.Turbidity
    if(myField %in% myNames.DataFields.Present==TRUE){##IF.myField.START
      #
      flog.debug(paste0('Processing data for ', myField))
      # myMsg.data <- "Turbidity"
      # myMsg <- paste("WORKING (QC Tests and Flags - ",myMsg.data,")",sep="")
      # myItems.Complete <- myItems.Complete + 1
      # myItems.Log[intCounter,2] <- myMsg
      # fun.Msg.Status(myMsg, intCounter, intItems.Total, strFile)
      # flush.console()
      #
      data.import <- fun.CalcQCStats(data.import
                                     ,myField
                                     ,myThresh.Gross.Fail.Hi.Turbidity
                                     ,myThresh.Gross.Fail.Lo.Turbidity
                                     ,myThresh.Gross.Suspect.Hi.Turbidity
                                     ,myThresh.Gross.Suspect.Lo.Turbidity
                                     ,myThresh.Spike.Hi.Turbidity
                                     ,myThresh.Spike.Lo.Turbidity
                                     ,myThresh.RoC.SD.period.Turbidity
                                     ,myThresh.RoC.SD.number.Turbidity
                                     ,myThresh.Flat.Hi.Turbidity
                                     ,myThresh.Flat.Lo.Turbidity
                                     ,myThresh.Flat.Tolerance.Turbidity)
    }##IF.myField.END
    #
    # B.6.11. Chlorophyll a
    myField <-myName.Chlorophylla
    if(myField %in% myNames.DataFields.Present==TRUE){##IF.myField.START
      #
      flog.debug(paste0('Processing data for ', myField))
      # myMsg.data <- "Chlorophylla"
      # myMsg <- paste("WORKING (QC Tests and Flags - ",myMsg.data,")",sep="")
      # myItems.Complete <- myItems.Complete + 1
      # myItems.Log[intCounter,2] <- myMsg
      # fun.Msg.Status(myMsg, intCounter, intItems.Total, strFile)
      # flush.console()
      #
      data.import <- fun.CalcQCStats(data.import
                                     ,myField
                                     ,myThresh.Gross.Fail.Hi.Chlorophylla
                                     ,myThresh.Gross.Fail.Lo.Chlorophylla
                                     ,myThresh.Gross.Suspect.Hi.Chlorophylla
                                     ,myThresh.Gross.Suspect.Lo.Chlorophylla
                                     ,myThresh.Spike.Hi.Chlorophylla
                                     ,myThresh.Spike.Lo.Chlorophylla
                                     ,myThresh.RoC.SD.period.Chlorophylla
                                     ,myThresh.RoC.SD.number.Chlorophylla
                                     ,myThresh.Flat.Hi.Chlorophylla
                                     ,myThresh.Flat.Lo.Chlorophylla
                                     ,myThresh.Flat.Tolerance.Chlorophylla)
    }##IF.myField.END
    #############################
    #
    # Names of columns for QC Calculations and Tests with Flags for each data column present
    # combine so can check for and remove later.
    flog.debug(paste0('working at line ', '530'))
    myNames.DataFields.Present.QCCalcs   <- as.vector(t(outer(myNames.DataFields.Present,myNames.QCCalcs,paste,sep=".")))

    myNames.Flags.QCTests <- paste0("Flag.",as.vector(t(outer( myNames.QCTests, myNames.DataFields.Present, paste, sep="."))))

    #jab removed for now - I don't see them being used
    #
    # myNames.DataFields.Present <- myNames.DataFields[myNames.DataFields %in% colnames(data.import)==TRUE]
    # # add Date.Time to names for modification
    # myNames.DataFields2Mod <- c(myName.DateTime, myNames.DataFields.Present)
    # #
    # # 5.0. Add "RAW" and "Comment.MOD" fields
    # # default values
    # myName.Raw <- "RAW"
    # myName.Comment.Mod <- "Comment.MOD"
    #
    # # 5.1. Cycle each present field
    # for (j in myNames.DataFields2Mod) {
    #
    #   # A. Add comment field and leave blank
    #   data.import[,paste(myName.Comment.Mod,j,sep=".")] <- ""
    #
    #   # B. Add data.RAW and populate with original data
    #   data.import[,paste(myName.Raw,j,sep=".")] <- data.import[,j]
    # }

    ###########################
    #
    # create a CSV file that is used by the QC Report function
    #
    ###############

    File.Date.Start <- format(as.Date(strFile.Date.Start, myFormat.Date),"%Y%m%d")
    File.Date.End   <- format(as.Date(strFile.Date.End, myFormat.Date),"%Y%m%d")

    strFile.Out.Prefix <- "QC"

    strFile.Out <- paste(paste(strFile.Out.Prefix,
                               strFile.SiteID,
                               strFile.DataType,
                               File.Date.Start,
                               File.Date.End, sep=config.FileName.Delimiter),"csv",sep=".")

    strFile.full_name = file.path(myDir.data.export,strFile.Out)

    # 10.2. Save to File the data (overwrites any existing file).

    write.csv(data.import, file=strFile.full_name, quote=FALSE, row.names=FALSE)
    #
    # flog.debug(paste0('Wrote CSV file' , "\n\t", strFile.full_name))

    ##################
    #
    # Run QC Report
    #
    ####################
    flog.debug(paste0('working at line ', '586'))
    fun.Report(strFile.SiteID
                 ,strFile.DataType
                 ,strFile.Date.Start
                 ,strFile.Date.End
                 ,fun.myDir.SUB.export
                 ,fun.myDir.SUB.export
                 ,strFile.Out.Prefix)
    flog.debug(paste0('working at line ', '594'))

    ##################
    #
    # Format final CSV file
    #
    ####################

    # remove all the 'P' (passing) flags from each Flag.* column just to simplify the CSV file
    #
    for(j in names(data.import))
    {
      column_name_parts <- strsplit(j, '[.]')

      if (column_name_parts[[1]][1] == 'Flag')
      {
        data.import[,j][data.import[,j]=="P"] <- ""
      }
    }

    # set the 'FlagCount' values to the sum of Flag columns that have something other than 'P' in them
    #
    data.import$FlagCount <- rowSums(data.import[, strCol.Flags] != "")

    # remove columns that are entirely empty
    #
    data.import <- data.import[, ! apply(data.import, 2, function(x) all(gsub(" ", "", x)=="", na.rm=TRUE))]

    # remove some redundant columns to simplify the CSV file
    #
    remove_columns = c('day', 'month', 'Date', 'Time')
    data.import <- data.import[, ! names(data.import) %in% remove_columns, drop = F]

    # write the csv file in its final format
    #
    write.csv(data.import, file=strFile.full_name, quote=FALSE, row.names=FALSE)

    #
    flog.debug(paste0('Wrote CSV file again' , "\n\t", strFile.full_name))

    # 11. Clean up
    # 11.1. Inform user of progress and update LOG
    myMsg <- "COMPLETE"
    myItems.Complete <- myItems.Complete + 1
    myItems.Log[intCounter,2] <- myMsg
    fun.write.log(myItems.Log,myDate,myTime)
    fun.Msg.Status(myMsg, intCounter, intItems.Total, strFile)
    flush.console()

    flog.debug(paste0('Finished processing file' , "\n\t", strFile.source.full_name))

    # 11.2. Remove data (import)
    rm(data.import)
  }

  # print a message and return
  flog.debug(paste("Task COMPLETE; ",round(difftime(Sys.time(),myTime.Start,units="mins"),2)," min.",sep=""))

  return()
}

######################################################################


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
                            ,fun.myThresh.RoC.SD.period
                            ,fun.myThresh.RoC.SD.number
                            ,fun.myThresh.Flat.Hi
                            ,fun.myThresh.Flat.Lo
                            ,fun.myThresh.Flat.Tolerance) {
  # AA. Offset timing check ####
  # check for offset timing of Air and Water measurements, 20170509
  #fun.myData.Type should inherit from calling script
  # data fields present
  fun.myField.Data.ALL <- names(fun.data.import)[names(fun.data.import) %in% myNames.DataFields]
  boo.Offset <- fun.OffsetCollectionCheck(fun.data.import, fun.myField.Data.ALL, myName.DateTime)
  if(boo.Offset==TRUE) {##IF.boo.Offset.START
    # check time interval (na.omit removes all)
    df.check <- na.omit(fun.data.import[,c(myName.DateTime,fun.myField.Data)])
    # convert from Character to time if necessary (not necessary)
    # if (is.character(df.check[,myName.DateTime])==TRUE){
    #   myFormat  <- myFormat.DateTime #"%Y-%m-%d %H:%M:%S"
    #   # df.check[,myName.DateTime] <- strftime(df.check[,myName.DateTime],
    #   #                                                     format=myFormat,
    #   #                                                     tz=myTZ)
    # }
    x <- df.check[,myName.DateTime]
    myTimeDiff.all <- difftime(x[-1],x[-length(x)])
    myTimeDiff <- median(as.vector(myTimeDiff.all))
    # create time series
    myTS <- seq(as.POSIXlt(min(x),tz=myTZ),as.POSIXlt(max(x),tz=myTZ),by="30 min") #by=paste0(myTimeDiff," min"))
    length(myTS)
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # remove other data fields (and extra times) before proceeding
    ### Fields to keep
    myFlds.Keep <- c(myName.SiteID,
                     myName.DateTime, myName.Date, myName.Time,
                     myName.Mo, myName.Day, myName.Yr,
                     fun.myField.Data, paste0(myName.Flag,".",fun.myField.Data))
    ### Modify the DF
    # keep only the relevant data field and remove all NA (case-wise)
    fun.data.import.mod <- na.omit(fun.data.import[,myFlds.Keep])
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Check length.  If different add in extra times
    ## NA records would have been removed earlier
    if (length(myTS) != nrow(df.check)){##IF.length.START
      # add extra rows to new Time Series
      ts.alltimes <- as.data.frame(as.character(myTS))
      names(ts.alltimes) <- myName.DateTime
      # merge (works with datetime as character but not as a POSIX field)
      #df.check[,myName.DateTime] <- as.POSIXlt(df.check[,myName.DateTime],origin = "1900-01-01",tz=myTZ)
      ts.alltimes.data <- merge(ts.alltimes, fun.data.import.mod, by=myName.DateTime)
      # use new df moving forward
      fun.data.import.mod <- ts.alltimes.data
    }##IF.length.END
    # then merge back at end
  } else {
    # rename fun.data.import (for non Offset)
    fun.data.import.mod <- fun.data.import
  }##IF.boo.Offset.END
  #~~~~~~~~~~~~~~
  # re-run boo.Offset at end to merge back into fun.data.import
  #~~~~~~~~~~~~~~
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # A.1. Calc, SD Time Interval
  myCalc <- "SD.Time"

  myField <- paste(fun.myField.Data, myCalc, sep = ".")

  # calculate as separate variable
  #http://stackoverflow.com/questions/8857287/how-to-add-subtract-time-from-a-posixlt-time-while-keeping-its-class-in-r
  # myT will be a POSIXt object
  myT <- strptime(fun.data.import.mod[,myName.DateTime],format=myFormat.DateTime)

  myT$hour <- myT$hour - fun.myThresh.RoC.SD.period

  # add back to dataframe
  fun.data.import.mod[,myField] <- as.character(myT)

  #
  # variable for following block
  myField.T1 <- myField

  #
  # A.2. Calc, SD, calc SD of last 25 hours
  myCalc <- "SD"

  myField <- paste(fun.myField.Data, myCalc, sep=".")

  ###################################################
  ## zoo version with rollapply
  #  minimum of 5 records
  if(nrow(fun.data.import.mod) < 5)
  {
    stop(paste0("The data file has less than 5 records. The scripts will not work properly until you have more data."))
  }

  # get interval distance (will crash if less than 5 records)
  myT.diff <- difftime(fun.data.import.mod[5,myName.DateTime],fun.data.import.mod[4,myName.DateTime],units="mins")
  myT.diff[[1]]

  # convert DateTime to POSIX object (already done above)
  #myT <- strptime(fun.data.import.mod[,myName.DateTime],format=myFormat.DateTime)
  # A.2. Use data "as is"
  # create zoo object of data and date/time (use row num instead)
  zoo.data <- zoo(fun.data.import.mod[,fun.myField.Data], seq(from=1, to=nrow(fun.data.import.mod), by=1))  # works
  #
  # B. Rolling SD
  # time difference is in minutes and Threshold is in hours
  # "By" in rollapply goes by # of records not by a set time.
  RollBy <- fun.myThresh.RoC.SD.period/(myT.diff[[1]]/60)

  # right align says the previous 50
  # +1 is to include the record itself
  RollSD <- rollapply(data=zoo.data, width=RollBy+1, FUN=sd, na.rm=TRUE, fill=NA, align="right")
  # add to data frame
  fun.data.import.mod[,myField] <- RollSD

  # clean up
  rm(myT)
  rm(zoo.data)
  rm(RollSD)

# ###################################################

  # A.3. Calc, NxSD, SD * n.per
  myCalc.1 <- "SD"
  myCalc.2 <- "SDxN"
  myField.1 <- paste(fun.myField.Data,myCalc.1,sep=".")
  myField.2 <- paste(fun.myField.Data,myCalc.2,sep=".")
  fun.data.import.mod[,myField.2] <- fun.data.import.mod[,myField.1] * fun.myThresh.RoC.SD.number
  #
  # A.4. Calc, Diff (1:5) (5 is default but can be more)
  for (i in 1:myThresh.Flat.MaxComp) {
    myCalc <- paste("n",i,sep=".")
    myField <- paste(fun.myField.Data,myCalc,sep=".")
    fun.data.import.mod[-(1:i),myField] <- diff(as.numeric(fun.data.import.mod[,fun.myField.Data]),lag=i)
    #
  }

  # A.5. Calc, flat.Hi, count n.1 etc if less than toler
  myCalc <- "flat.Hi"
  myField <- paste(fun.myField.Data,myCalc,sep=".")
  myThresh <- fun.myThresh.Flat.Hi
  # Fields to check
  myFields.Match <- match(paste(fun.myField.Data,"n",1:myThresh,sep="."), names(fun.data.import.mod))
  # use rowSums to count the fields
  fun.data.import.mod[,myField] <- rowSums(abs(fun.data.import.mod[,myFields.Match])<=fun.myThresh.Flat.Tolerance)

  # A.6. Calc, flat.Lo, count if less than toler
  myCalc <- "flat.Lo"
  myField <- paste(fun.myField.Data,myCalc,sep=".")
  myThresh <- fun.myThresh.Flat.Lo
  # Fields to check
  myFields.Match <- match(paste(fun.myField.Data,"n",1:myThresh,sep="."), names(fun.data.import.mod))
  # use rowSums to count the fields
  fun.data.import.mod[,myField] <- rowSums(abs(fun.data.import.mod[,myFields.Match])<=fun.myThresh.Flat.Tolerance)

  ## B. Generate Flags based on Calculation Fields
  # B.1. Gross
  myQCTest <- "Gross"
  myField <- paste("Flag",myQCTest,fun.myField.Data,sep=".")

  # Assign Flags
  # default value
  fun.data.import.mod[,myField] <- myFlagVal.NotEval
  # data is NA then flag = 9 (missing data)
  fun.data.import.mod[,myField][is.na(fun.data.import.mod[,fun.myField.Data])==TRUE] <- myFlagVal.NoData
  # different test for water level, only if negative

  if(fun.myField.Data==myName.SensorDepth)
  {
    # data < 0 (i.e., negative) = 4 (fail)
    fun.data.import.mod[,myField][fun.data.import.mod[,fun.myField.Data] < 0] <- myFlagVal.Fail
    # otherwise flag = 1 (pass)
    fun.data.import.mod[,myField][fun.data.import.mod[,myField]==myFlagVal.NotEval] <- myFlagVal.Pass
  # different test for discharge
  } else if(fun.myField.Data==myName.Discharge)
  {
    # data < 0 (i.e., negative) = 4 (fail)
    fun.data.import.mod[,myField][fun.data.import.mod[,fun.myField.Data] < 0] <- myFlagVal.Fail
    # otherwise flag = 1 (pass)
    fun.data.import.mod[,myField][fun.data.import.mod[,myField]==myFlagVal.NotEval] <- myFlagVal.Pass
  } else
  {
    # data >= Suspect.Hi then flag = 3 (suspect)
    fun.data.import.mod[,myField][fun.data.import.mod[,fun.myField.Data] >= fun.myThresh.Gross.Suspect.Hi] <- myFlagVal.Suspect
    # data <= Suspect.Lo then flag = 3 (Suspect)
    fun.data.import.mod[,myField][fun.data.import.mod[,fun.myField.Data] <= fun.myThresh.Gross.Suspect.Lo] <- myFlagVal.Suspect
    # data >= Fail.Hi then flag = 4 (fail)
    fun.data.import.mod[,myField][fun.data.import.mod[,fun.myField.Data] >= fun.myThresh.Gross.Fail.Hi] <- myFlagVal.Fail
    # data <= Fail.Lo then flag = 4 (fail)
    fun.data.import.mod[,myField][fun.data.import.mod[,fun.myField.Data] <= fun.myThresh.Gross.Fail.Lo] <- myFlagVal.Fail
    # otherwise flag = 1 (pass)
    fun.data.import.mod[,myField][fun.data.import.mod[,myField]==myFlagVal.NotEval] <- myFlagVal.Pass
  }

  # B.2. Spike
  myQCTest <- "Spike"
  myField <- paste("Flag",myQCTest,fun.myField.Data,sep=".")
  myField.Calc.1 <- paste(fun.myField.Data,"n",1,sep=".")
  # Assign Flags
  # default value
  fun.data.import.mod[,myField] <- myFlagVal.NotEval
  # diff 1 is NA then flag = 9 (missing data)
  fun.data.import.mod[,myField][is.na(fun.data.import.mod[,myField.Calc.1])==TRUE] <- myFlagVal.NoData
  # abs(diff 1) >= spike Lo then flag = 3 (suspect)
  fun.data.import.mod[,myField][abs(fun.data.import.mod[,myField.Calc.1]) >= fun.myThresh.Spike.Lo] <- myFlagVal.Suspect
  # abs(diff 1) >= spike Hi then flag = 4 (fail)
  fun.data.import.mod[,myField][abs(fun.data.import.mod[,myField.Calc.1]) >= fun.myThresh.Spike.Hi] <- myFlagVal.Fail
  # otherwise flag = 1 (pass)
  fun.data.import.mod[,myField][fun.data.import.mod[,myField]==myFlagVal.NotEval] <- myFlagVal.Pass

  # B.3. RoC
  myQCTest <- "RoC"
  myField <- paste("Flag",myQCTest,fun.myField.Data,sep=".")
  myField.Calc.1 <- paste(fun.myField.Data,"n",1,sep=".")
  myField.Calc.2 <- paste(fun.myField.Data,"SDxN",sep=".")
  # Assign Flags
  # default value
  fun.data.import.mod[,myField] <- myFlagVal.NotEval
  # data is NA then flag = 9 (missing data)
  fun.data.import.mod[,myField][is.na(fun.data.import.mod[,fun.myField.Data])==TRUE] <- myFlagVal.NoData
  # sd is NA then flag = 9 (missing data)
  fun.data.import.mod[,myField][is.na(fun.data.import.mod[,myField.Calc.1])==TRUE] <- myFlagVal.NoData
  # diff 1 > SD*N then flag = 3 (suspect)
  fun.data.import.mod[,myField][abs(fun.data.import.mod[,myField.Calc.1]) > fun.data.import.mod[,myField.Calc.2]] <- myFlagVal.Suspect
  # otherwise flag = 1 (pass) [no 4/Fail]
  fun.data.import.mod[,myField][fun.data.import.mod[,myField]==myFlagVal.NotEval] <- myFlagVal.Pass

  # B.4. Flat
  myQCTest <- "Flat"
  myField <- paste("Flag",myQCTest,fun.myField.Data,sep=".")
  myField.Calc.1 <- paste(fun.myField.Data,"flat.Hi",sep=".")
  myField.Calc.2 <- paste(fun.myField.Data,"flat.Lo",sep=".")
  # default value
  fun.data.import.mod[,myField] <- myFlagVal.NotEval
  # Lo >= Thresh.Lo = 3 (suspect)
  fun.data.import.mod[,myField][fun.data.import.mod[,myField.Calc.2] >= fun.myThresh.Flat.Lo] <- myFlagVal.Suspect
  # Hi >= Thresh.Hi = 4 (fail)
  fun.data.import.mod[,myField][fun.data.import.mod[,myField.Calc.1] >= fun.myThresh.Flat.Hi] <- myFlagVal.Fail
  # otherwise flag = 1 (pass)
  fun.data.import.mod[,myField][fun.data.import.mod[,myField]==myFlagVal.NotEval] <- myFlagVal.Pass

  # C. Assign Overall Data Flag
  myField <- paste("Flag",fun.myField.Data,sep=".")

  #myNames.QCTests
  # get column numbers (match) for QCTest Flags for this data
  myFields.Match <- match(paste("Flag",myNames.QCTests,fun.myField.Data,sep="."), names(fun.data.import.mod))

  # Conditional rowSums for number of flag fields with specified flags
  myFlags.Num.Pass    <- rowSums(fun.data.import.mod[,myFields.Match]==myFlagVal.Pass)
  myFlags.Num.Suspect <- rowSums(fun.data.import.mod[,myFields.Match]==myFlagVal.Suspect )
  myFlags.Num.Fail    <- rowSums(fun.data.import.mod[,myFields.Match]==myFlagVal.Fail)
  myFlags.Num.Missing <- rowSums(fun.data.import.mod[,myFields.Match]==myFlagVal.NoData)
  myFlags.Num.OK      <- rowSums(fun.data.import.mod[,myFields.Match]==myFlagVal.Pass | fun.data.import.mod[,myFields.Match]==myFlagVal.NoData)

  # Assign
  # default value
  fun.data.import.mod[,myField] <- myFlagVal.NotEval
  # any QC Test flags = 3 then flag = 3 (suspect)
  fun.data.import.mod[,myField][myFlags.Num.Suspect > 0] <- myFlagVal.Suspect
  # any QC Test flags = 4 then flag = 4 (fail)
  fun.data.import.mod[,myField][myFlags.Num.Fail > 0] <- myFlagVal.Fail
  # all QC Test flags = 1 then flag = 1 (pass)
  fun.data.import.mod[,myField][myFlags.Num.Pass == length(myNames.QCTests)] <- myFlagVal.Pass
  # all QC Test flags = 1 or 9 then flag = 1 (pass)
  fun.data.import.mod[,myField][myFlags.Num.OK == length(myNames.QCTests)] <- myFlagVal.Pass
    #fun.data.import.mod[,myField][fun.data.import.mod[,myField]==myFlagVal.NotEval] <- myFlagVal.Pass
  # data is NA then flag = 9 (missing data)
  fun.data.import.mod[,myField][is.na(fun.data.import.mod[,fun.myField.Data])==TRUE] <- myFlagVal.NoData

  # D. Remove QC Calc fields
  #myNames.QCCalcs <- c("SD.Time","SD","SDxN","n.1","n.2","n.3","n.4","n.5","flat.Lo","flat.Hi")
  # get field column numbers
  myFields.Match <- match(paste(fun.myField.Data,myNames.QCCalcs,sep="."), names(fun.data.import.mod))
  # drop fields from data table
  fun.data.import.mod <- fun.data.import.mod[,-na.omit(myFields.Match)]
 #~~~~~~~~~~~~~~~~~~~~~~~~~
  # D.2. Offset Timing Fix
  ## Return a merged file if Offset is TRUE
  if(boo.Offset==TRUE) {##IF.boo.Offset.START
    # removed other data fields (and extra times) before above
    # merge back
    # all fields modified = ?????
    #Fld.Mod <- c(myName.DateTime, fun.myField.Data)
    # get fields added (20170512, add in Flag.Param)
    Fld.Flag.Param <- match(myField, names(fun.data.import))
    Fld.New <- names(fun.data.import.mod)[names(fun.data.import.mod) %in% names(fun.data.import)[-Fld.Flag.Param]==FALSE]
    # merge on date time
    DF.Return <- merge(fun.data.import[,-Fld.Flag.Param], fun.data.import.mod[,c(myName.DateTime, Fld.New)],
                       by=myName.DateTime, all.x=TRUE)
  } else {
    DF.Return <- fun.data.import.mod
  }##IF.boo.Offset.END
  #
  # # 20170605, Add "User" field for Parameter (fun.myField.Data)
  # ## TN only, for reimporting the file
  # myFld <- paste0("User.",fun.myField.Data)
  # DF.Return[,myFld] <- ""
  #
  # function output
  #return(fun.data.import)
  return(DF.Return)

}


