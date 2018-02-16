# Continuous data helper script
# Default Values
# Erik.Leppo@tetratech.com (EWL)
# 20150928
##################
# User defined variable names for input data
##################
# It is assumed that this R script is stored in a directory with the data files as subdirectories
# This script is intended to be "source"d from the main script.
#############################

#####################################################################
config.LoggingLevel <- DEBUG

# Directory Names
config.Folder.RawData   <- file.path(config.Folder.Root, 'data', "Data1_Raw")
config.Folder.QCData    <- file.path(config.Folder.Root, 'data', "Data2_QC")
config.Folder.AggData   <- file.path(config.Folder.Root, 'data', "Data3_Aggregated")
config.Folder.StatsData <- file.path(config.Folder.Root, 'data', "Data4_Stats")

# this is where the log files are created
config.Folder.Logs      <- file.path(config.Folder.Root, "logs")

config.LoggingFile <- file.path(config.Folder.Logs, 'main_script.log')

# this is where the markdown files (templates) are stored
config.Folder.Markdown  <- file.path(config.Folder.Root, 'markdown')

#####################################################################
# USER may make modifications in this section but not mandatory
# this section could be sourced so can use between scripts
#####################################################################

# Delimiter in File Names (e.g., test2_AW_201301_20131231.csv)
config.FileName.Delimiter  <- "_"

#####################################################################

# Acceptable column names for the data
#(special characters (e.g., %, space, or /) are converted to "." by R, "?" converted to "?")
myUnits.AirTemp     <- "C" # C or F
myUnits.WaterTemp   <- myUnits.AirTemp
myUnits.AirBP       <- "psi"
myUnits.WaterP      <- myUnits.AirBP
myUnits.SensorDepth  <- "ft"
myUnits.Discharge   <- "ft3.s"
myUnits.Cond       <- "uS.cm"
myUnits.DO         <- "mg.L"
myUnits.pH         <- "SU"
myUnits.Turbidity  <- "NTU"
myUnits.Chlorophylla <- "g.cm3"
myUnits.GageHeight <- "ft"

## Air and Water
myName.SiteID         <- "SiteID"
myName.Date           <- "Date"
myName.Time           <- "Time"
myName.DateTime       <- "Date.Time" # this has to be in the input file Erik had it as 'Date.Time'

## Water
myName.RowID.Water    <- "Water.RowID"
myName.LoggerID.Water <- "Water.LoggerID"
myName.WaterTemp      <- paste0("Water.Temp.",myUnits.WaterTemp)  # "?" from HoboWare files sometimes adds "? " in front.  Replace with "." in R.

## Air
myName.RowID.Air      <- "Air.RowID"
myName.LoggerID.Air   <- "Air.LoggerID"
myName.AirTemp        <- paste0("Air.Temp.", myUnits.AirTemp)   # "?" from HoboWare files sometimes adds "? " in front.  Replace with "." in R.
myName.WaterP         <- paste0("Water.P.", myUnits.WaterP)
myName.AirBP          <- paste0("Air.BP.", myUnits.AirBP)
myName.SensorDepth     <- paste0("Sensor.Depth.", myUnits.SensorDepth)
myName.Discharge      <- paste0("Discharge.", myUnits.Discharge)
myName.Cond          <- paste0("Conductivity.",myUnits.Cond)
myName.DO            <- paste0("DO.",myUnits.DO)
myName.pH            <- paste0("pH.",myUnits.pH)
myName.Turbidity     <- paste0("Turbidity.",myUnits.DO)
myName.Chlorophylla   <- paste0("Chlorophylla.",myUnits.pH)
myName.GageHeight    <- paste0("GageHeight.",myUnits.GageHeight)

## plot labels
myLab.WaterTemp       <- paste("Temperature, Water (",myUnits.WaterTemp,")",sep="")
myLab.AirTemp         <- paste("Temperature, Air (",myUnits.AirTemp,")",sep="")
myLab.Date            <- "Date"
myLab.DateTime        <- "Date"
myLab.WaterP          <- paste("Pressure, Water (",myUnits.AirBP,")",sep="")
myLab.AirBP           <- paste("Barometric Pressure, Air (",myUnits.WaterP,")",sep="")
myLab.SensorDepth      <- paste("Sensor Depth (",myUnits.SensorDepth,")",sep="")
myLab.Temp.BOTH       <- paste("Temperature (",myUnits.WaterTemp,")",sep="")
myLab.Discharge       <- paste("Discharge (",sub("\\.","/",myUnits.Discharge),")")
myLab.Cond           <- paste0("Conductivity (",sub("\\.","/",myUnits.Cond),")")    #replace "." with "/"
myLab.DO             <- paste0("Dissolved Oxygen (",sub("\\.","/",myUnits.DO),")")  #replace "." with "/"
myLab.pH             <- paste0("pH (",myUnits.pH,")")
myLab.Turbidity      <- paste0("Turbidity (",myUnits.Turbidity,")")
myLab.Chlorophylla   <- paste0("Chlorophyll a (",sub("\\.","/",myUnits.Chlorophylla),")")    #replace "." with "/"
myLab.GageHeight     <- paste0("Gage Height (",myUnits.GageHeight,")")

#####################################################################
# Discrete Measurements
myPrefix.Discrete           <- "Discrete"
myName.Discrete.WaterTemp   <- paste(myPrefix.Discrete,myName.WaterTemp,sep=".")
myName.Discrete.AirTemp     <- paste(myPrefix.Discrete,myName.AirTemp,sep=".")
myName.Discrete.WaterP      <- paste(myPrefix.Discrete,myName.WaterP,sep=".")
myName.Discrete.AirBP       <- paste(myPrefix.Discrete,myName.AirBP,sep=".")
myName.Discrete.SensorDepth  <- paste(myPrefix.Discrete,myName.SensorDepth,sep=".")
myName.Discrete.Discharge   <- paste(myPrefix.Discrete,myName.Discharge,sep=".")
myLab.Discrete.WaterTemp    <- paste(myLab.WaterTemp,"(Discrete)",sep=" ")
myLab.Discrete.AirTemp      <- paste(myLab.AirTemp,"(Discrete)",sep=" ")
myLab.Discrete.WaterP       <- paste(myLab.WaterP,"(Discrete)",sep=" ")
myLab.Discrete.AirBP        <- paste(myLab.AirBP,"(Discrete)",sep=" ")
myLab.Discrete.SensorDepth   <- paste(myLab.SensorDepth,"(Discrete)",sep=" ")
myLab.Discrete.Discharge    <- paste(myLab.Discharge,"(Discrete)",sep=" ")
myName.Discrete.Cond       <- paste(myPrefix.Discrete,myName.Cond,sep=".")
myName.Discrete.DO         <- paste(myPrefix.Discrete,myName.DO,sep=".")
myName.Discrete.pH         <- paste(myPrefix.Discrete,myName.pH,sep=".")
myName.Discrete.Turbidity  <- paste(myPrefix.Discrete,myName.Turbidity,sep=".")
myName.Discrete.Chlorophylla <- paste(myPrefix.Discrete,myName.Chlorophylla,sep=".")
myName.Discrete.GageHeight <- paste(myPrefix.Discrete,myName.GageHeight,sep=".")

myLab.Discrete.Cond       <- paste(myPrefix.Discrete,myName.Cond,sep=".")
myLab.Discrete.DO         <- paste(myPrefix.Discrete,myName.DO,sep=".")
myLab.Discrete.pH         <- paste(myPrefix.Discrete,myName.pH,sep=".")
myLab.Discrete.Turbidity  <- paste(myPrefix.Discrete,myName.Turbidity,sep=".")
myLab.Discrete.Chlorophylla <- paste(myPrefix.Discrete,myName.Chlorophylla,sep=".")
myLab.Discrete.GageHeight <- paste(myPrefix.Discrete,myName.GageHeight,sep=".")



#####################################################################
# Automated QC stuff
## data type/stages
myDataQuality.Raw         <- "RAW"
myDataQuality.QCauto      <- "QCauto"
myDataQuality.QCmanual    <- "QCmanual"
myDataQuality.Final       <- "Final"
myDataQuality.Aggregated  <- "Aggregated"

#####################################################################

# Data Fields
myNames.DataFields <- c(myName.WaterTemp
                        ,myName.AirTemp
                        ,myName.WaterP
                        ,myName.AirBP
                        ,myName.SensorDepth
                        ,myName.Discharge
                        ,myName.Discrete.WaterTemp
                        ,myName.Discrete.AirTemp
                        ,myName.Discrete.WaterP
                        ,myName.Discrete.AirBP
                        ,myName.Discrete.SensorDepth
                        ,myName.Discrete.Discharge
                        , myName.Discrete.Cond
                        , myName.Discrete.DO
                        , myName.Discrete.pH
                        , myName.Discrete.Turbidity
                        , myName.Discrete.Chlorophylla
                        , myName.Discrete.GageHeight)

myNames.DataFields.Lab <- c(myLab.WaterTemp
                            ,myLab.AirTemp
                            ,myLab.WaterP
                            ,myLab.AirBP
                            ,myLab.SensorDepth
                            ,myLab.Discharge
                            ,myLab.Discrete.WaterTemp
                            ,myLab.Discrete.AirTemp
                            ,myLab.Discrete.WaterP
                            ,myLab.Discrete.AirBP,
                            myLab.Discrete.SensorDepth
                            ,myLab.Discrete.Discharge
                            , myLab.Discrete.Cond
                            , myLab.Discrete.DO
                            , myLab.Discrete.pH
                            , myLab.Discrete.Turbidity
                            , myLab.Discrete.Chlorophylla
                            , myLab.Discrete.GageHeight)

myNames.DataFields.Col <- c("blue","green","gray","gray","black","brown")

# Name Order (change order below to change order in output file)
#
myNames.Order <- c(myName.SiteID
                   ,myName.Date
                   ,myName.Time
                   ,myName.DateTime
                   ,myName.WaterTemp
                   ,myName.LoggerID.Air
                   ,myName.RowID.Air
                   ,myName.AirTemp
                   ,myName.WaterP
                   ,myName.AirBP
                   ,myName.SensorDepth
                   ,myName.LoggerID.Water
                   ,myName.RowID.Water
                   ,myName.Discharge
                   ,myName.Discrete.WaterTemp
                   ,myName.Discrete.AirTemp
                   ,myName.Discrete.WaterP
                   ,myName.Discrete.AirBP
                   ,myName.Discrete.SensorDepth
                   ,myName.Discrete.Discharge
                   , myName.Discrete.Cond
                   , myName.Discrete.DO
                   , myName.Discrete.pH
                   , myName.Discrete.Turbidity
                   , myName.Discrete.Chlorophylla
                   , myName.Discrete.GageHeight)

######################################################################

## Data Quality Flag Values

myFlagVal.Pass     <- "P" # try and leave it blank "P"
myFlagVal.NotEval  <- "NA"
myFlagVal.Suspect  <- "S"
myFlagVal.Fail     <- "F"
myFlagVal.NoData   <- "X"
myFlagVal.Order    <- c(myFlagVal.Pass, myFlagVal.Suspect, myFlagVal.Fail, myFlagVal.NoData)

#####################################################################

# QC Tests and Calculations
#http://stackoverflow.com/questions/16143700/pasting-two-vectors-with-combinations-of-all-vectors-elements
#myNames.QCTests.Calcs.combo <- as.vector(t(outer(myNames.QCTests,myNames.QCTests.Calcs,paste,sep=".")))
# combine so can check for and remove later.
#myNames.DataFields.QCTests.Calcs.combo <- as.vector(t(outer(myNames.DataFields,myNames.QCTests.Calcs.combo,paste,sep=".")))
# Data Quality Flag Thresholds
## Gross Min/Max, Fail (equipment)
myThresh.Gross.Fail.Hi.WaterTemp  <- 30
myThresh.Gross.Fail.Lo.WaterTemp  <- -2
myThresh.Gross.Fail.Hi.AirTemp    <- 38
myThresh.Gross.Fail.Lo.AirTemp    <- -25
myThresh.Gross.Fail.Hi.WaterP     <- 17
myThresh.Gross.Fail.Lo.WaterP     <- 13
myThresh.Gross.Fail.Hi.AirBP      <- 15
myThresh.Gross.Fail.Lo.AirBP      <- 13
myThresh.Gross.Fail.Hi.SensorDepth <- 6    # no longer used (only check for negative values for SensorDepth)
myThresh.Gross.Fail.Lo.SensorDepth <- -1   # no longer used (only check for negative values for SensorDepth)
myThresh.Gross.Fail.Hi.Discharge  <- 10^5 #dependant upon stream size (only checkf or negative values)
myThresh.Gross.Fail.Lo.Discharge  <- -1   #dependant upon stream size
myThresh.Gross.Fail.Hi.Cond       <- 1500
myThresh.Gross.Fail.Lo.Cond       <- 10
myThresh.Gross.Fail.Hi.DO         <- 20
myThresh.Gross.Fail.Lo.DO         <- 1
myThresh.Gross.Fail.Hi.pH         <- 12
myThresh.Gross.Fail.Lo.pH         <- 3
myThresh.Gross.Fail.Hi.Turbidity         <- 10^5
myThresh.Gross.Fail.Lo.Turbidity         <- -1
myThresh.Gross.Fail.Hi.Chlorophylla         <- 10^5
myThresh.Gross.Fail.Lo.Chlorophylla          <- -1
myThresh.Gross.Fail.Hi.GageHeight         <- 10^5
myThresh.Gross.Fail.Lo.GageHeight          <- -1

## Gross Min/Max, Suspect (extreme)
myThresh.Gross.Suspect.Hi.WaterTemp  <- 25
myThresh.Gross.Suspect.Lo.WaterTemp  <- -0.1
myThresh.Gross.Suspect.Hi.AirTemp    <- 35
myThresh.Gross.Suspect.Lo.AirTemp    <- -23
myThresh.Gross.Suspect.Hi.WaterP     <- 16.8
myThresh.Gross.Suspect.Lo.WaterP     <- 13.5
myThresh.Gross.Suspect.Hi.AirBP      <- 14.8
myThresh.Gross.Suspect.Lo.AirBP      <- 13.5
myThresh.Gross.Suspect.Hi.SensorDepth <- 5    # no longer used (only check for negative values for SensorDepth)
myThresh.Gross.Suspect.Lo.SensorDepth <- 0    # no longer used (only check for negative values for SensorDepth)
myThresh.Gross.Suspect.Hi.Discharge  <- 10^3 #dependant upon stream size (only checkf or negative values
myThresh.Gross.Suspect.Lo.Discharge  <- -1   #dependant upon stream size
myThresh.Gross.Suspect.Hi.Cond       <- 1200
myThresh.Gross.Suspect.Lo.Cond       <- 20
myThresh.Gross.Suspect.Hi.DO         <- 18
myThresh.Gross.Suspect.Lo.DO         <- 2
myThresh.Gross.Suspect.Hi.pH         <- 11
myThresh.Gross.Suspect.Lo.pH         <- 4
myThresh.Gross.Suspect.Hi.Turbidity         <- 10^3
myThresh.Gross.Suspect.Lo.Turbidity          <- -1
myThresh.Gross.Suspect.Hi.Chlorophylla         <- 10^3
myThresh.Gross.Suspect.Lo.Chlorophylla         <- 1
myThresh.Gross.Suspect.Hi.GageHeight         <- 10^3
myThresh.Gross.Suspect.Lo.GageHeight        <- 1

## Spike thresholds (absolute change)
myThresh.Spike.Hi.WaterTemp   <- 1.5
myThresh.Spike.Lo.WaterTemp   <- 1
myThresh.Spike.Hi.AirTemp     <- 10
myThresh.Spike.Lo.AirTemp     <- 8
myThresh.Spike.Hi.WaterP      <- 0.7
myThresh.Spike.Lo.WaterP      <- 0.5
myThresh.Spike.Hi.AirBP       <- 0.25
myThresh.Spike.Lo.AirBP       <- 0.15
myThresh.Spike.Hi.SensorDepth  <- 5
myThresh.Spike.Lo.SensorDepth  <- 3
myThresh.Spike.Hi.Discharge   <- 10^4 #dependant upon stream size
myThresh.Spike.Lo.Discharge   <- 10^3 #dependant upon stream size
myThresh.Spike.Hi.Cond       <- 10
myThresh.Spike.Lo.Cond       <- 5
myThresh.Spike.Hi.DO         <- 10
myThresh.Spike.Lo.DO         <- 5
myThresh.Spike.Hi.pH         <- 10
myThresh.Spike.Lo.pH         <- 5
myThresh.Spike.Hi.Turbidity         <- 10^4
myThresh.Spike.Lo.Turbidity         <- 10^3
myThresh.Spike.Hi.Chlorophylla         <- 10^4
myThresh.Spike.Lo.Chlorophylla         <- 10^3
myThresh.Spike.Hi.Chlorophylla         <- 10^4
myThresh.Spike.Lo.Chlorophylla         <- 10^3

## Rate of Change (relative change)
myDefault.RoC.SD.number   <- 3
myDefault.RoC.SD.period   <- 25 #hours
myThresh.RoC.SD.number.WaterTemp  <- myDefault.RoC.SD.number
myThresh.RoC.SD.period.WaterTemp  <- myDefault.RoC.SD.period
myThresh.RoC.SD.number.AirTemp    <- myDefault.RoC.SD.number
myThresh.RoC.SD.period.AirTemp    <- myDefault.RoC.SD.period
myThresh.RoC.SD.number.WaterP     <- myDefault.RoC.SD.number
myThresh.RoC.SD.period.WaterP     <- myDefault.RoC.SD.period
myThresh.RoC.SD.number.AirBP      <- myDefault.RoC.SD.number
myThresh.RoC.SD.period.AirBP      <- myDefault.RoC.SD.period
myThresh.RoC.SD.number.SensorDepth <- myDefault.RoC.SD.number
myThresh.RoC.SD.period.SensorDepth <- myDefault.RoC.SD.period
myThresh.RoC.SD.number.Discharge  <- myDefault.RoC.SD.number
myThresh.RoC.SD.period.Discharge  <- myDefault.RoC.SD.period
myThresh.RoC.SD.number.Cond       <- myDefault.RoC.SD.number
myThresh.RoC.SD.period.Cond       <- myDefault.RoC.SD.period
myThresh.RoC.SD.number.DO         <- myDefault.RoC.SD.number
myThresh.RoC.SD.period.DO         <- myDefault.RoC.SD.period
myThresh.RoC.SD.number.pH         <- myDefault.RoC.SD.number
myThresh.RoC.SD.period.pH         <- myDefault.RoC.SD.period
myThresh.RoC.SD.number.Turbidity         <- myDefault.RoC.SD.number
myThresh.RoC.SD.period.Turbidity         <- myDefault.RoC.SD.period
myThresh.RoC.SD.number.Chlorophylla         <- myDefault.RoC.SD.number
myThresh.RoC.SD.period.Chlorophylla        <- myDefault.RoC.SD.period
myThresh.RoC.SD.number.GageHeight         <- myDefault.RoC.SD.number
myThresh.RoC.SD.period.GageHeight        <- myDefault.RoC.SD.period

## No Change (flat-line)
myDefault.Flat.Hi         <- 30  # maximum is myThresh.Flat.MaxComp
myDefault.Flat.Lo         <- 15
myDefault.Flat.Tolerance  <- 0.01 # set to one sigdig less than measurements.  Check with fivenum(x)

myThresh.Flat.Hi.WaterTemp          <- 30
myThresh.Flat.Lo.WaterTemp          <- 20
myThresh.Flat.Tolerance.WaterTemp   <- 0.01
myThresh.Flat.Hi.AirTemp            <- 15
myThresh.Flat.Lo.AirTemp            <- 10
myThresh.Flat.Tolerance.AirTemp     <- 0.01
myThresh.Flat.Hi.WaterP             <- 15
myThresh.Flat.Lo.WaterP             <- 10
myThresh.Flat.Tolerance.WaterP      <- 0.001
myThresh.Flat.Hi.AirBP              <- 15
myThresh.Flat.Lo.AirBP              <- 10
myThresh.Flat.Tolerance.AirBP       <- 0.001
myThresh.Flat.Hi.SensorDepth         <- 60
myThresh.Flat.Lo.SensorDepth         <- 20
myThresh.Flat.Tolerance.SensorDepth  <- 0.0
myThresh.Flat.Hi.Discharge          <- myDefault.Flat.Hi * 2
myThresh.Flat.Lo.Discharge          <- myDefault.Flat.Lo * 2
myThresh.Flat.Tolerance.Discharge   <- 0.01
myThresh.Flat.Hi.Cond              <- myDefault.Flat.Hi * 2
myThresh.Flat.Lo.Cond              <- myDefault.Flat.Lo * 2
myThresh.Flat.Tolerance.Cond       <- 0.01
myThresh.Flat.Hi.DO                <- myDefault.Flat.Hi * 2
myThresh.Flat.Lo.DO                <- myDefault.Flat.Lo * 2
myThresh.Flat.Tolerance.DO         <- 0.01
myThresh.Flat.Hi.pH                <- myDefault.Flat.Hi * 2
myThresh.Flat.Lo.pH                <- myDefault.Flat.Lo * 2
myThresh.Flat.Tolerance.pH         <- 0.01
myThresh.Flat.Hi.Turbidity               <- myDefault.Flat.Hi * 2
myThresh.Flat.Lo.Turbidity                <- myDefault.Flat.Lo * 2
myThresh.Flat.Tolerance.Turbidity         <- 0.01
myThresh.Flat.Hi.Chlorophylla                <- myDefault.Flat.Hi * 2
myThresh.Flat.Lo.Chlorophylla                <- myDefault.Flat.Lo * 2
myThresh.Flat.Tolerance.Chlorophylla        <- 0.01
myThresh.Flat.Hi.GageHeight                <- myDefault.Flat.Hi * 2
myThresh.Flat.Lo.GageHeight               <- myDefault.Flat.Lo * 2
myThresh.Flat.Tolerance.GageHeight        <- 0.01
myThresh.Flat.MaxComp     <- max(myThresh.Flat.Hi.WaterTemp
                                 ,myThresh.Flat.Hi.AirTemp
                                 ,myThresh.Flat.Hi.WaterP
                                 ,myThresh.Flat.Hi.AirBP
                                 ,myThresh.Flat.Hi.SensorDepth
                                 ,myThresh.Flat.Hi.Discharge
                                 , myThresh.Flat.Hi.Cond
                                 , myThresh.Flat.Hi.DO
                                 , myThresh.Flat.Hi.pH
                                 , myThresh.Flat.Hi.Turbidity
                                 , myThresh.Flat.Hi.Chlorophylla
                                 , myThresh.Flat.Hi.GageHeight)

#####################################################################

# Data Fields with Flags
myName.Flag <- "Flag" # flag prefix
myNames.Cols4Flags <- c(myName.DateTime, myNames.DataFields)
myNames.Flags <- paste(myName.Flag, myNames.Cols4Flags, sep=".")  # define ones using in the calling script

## flag labels
myName.Flag.DateTime    <- paste(myName.Flag,myName.DateTime,sep=".")
myName.Flag.WaterTemp   <- paste(myName.Flag,myName.WaterTemp,sep=".")
myName.Flag.AirTemp     <- paste(myName.Flag,myName.AirTemp,sep=".")
myName.Flag.WaterP      <- paste(myName.Flag,myName.WaterP,sep=".")
myName.Flag.AirBP       <- paste(myName.Flag,myName.AirBP,sep=".")
myName.Flag.SensorDepth  <- paste(myName.Flag,myName.SensorDepth,sep=".")
myName.Flag.Discharge   <- paste(myName.Flag,myName.Discharge,sep=".")
myName.Flag.Cond       <- paste(myName.Flag,myName.Cond,sep=".")
myName.Flag.DO         <- paste(myName.Flag,myName.DO,sep=".")
myName.Flag.pH         <- paste(myName.Flag,myName.pH,sep=".")
myName.Flag.Turbidity         <- paste(myName.Flag,myName.Turbidity,sep=".")
myName.Flag.Chlorophylla        <- paste(myName.Flag,myName.Chlorophylla,sep=".")
myName.Flag.GageHeight        <- paste(myName.Flag,myName.GageHeight,sep=".")

# Data Quality Test Names
myNames.QCTests <- c("Gross","Spike","RoC","Flat")
myNames.QCCalcs <- c("SD.Time","SD","SDxN",paste("n",1:myThresh.Flat.MaxComp,sep="."), "flat.Lo", "flat.Hi")

#####################################################################
# Exceedance values for stats (default to Gross-Suspect-Hi value)
myExceed.WaterTemp  <- myThresh.Gross.Suspect.Hi.WaterTemp
myExceed.AirTemp    <- myThresh.Gross.Suspect.Hi.AirTemp
myExceed.SensorDepth <- myThresh.Gross.Suspect.Hi.SensorDepth

#####################################################################
# Date and Time Formats
myFormat.Date           <- "%Y-%m-%d"
myFormat.Time           <- "%H:%M:%S"
myFormat.DateTime       <- "%Y-%m-%d %H:%M:%S"
DateRange.Start.Default <- "1900-01-01" #YYYY-MM-DD
DateRange.End.Default   <- format(Sys.Date(),"%Y-%m-%d")
# Time Zone, used in Gage script in dataRetrieval
myTZ <- "America/New_York"

pbcc.Format.Date <- "%d-%b-%y"

######################################################################

# Time Frames (MM-DD)
myTimeFrame.Annual.Start        <- "0101"
myTimeFrame.Annual.End          <- "1231"
myTimeFrame.WaterYear.Start     <- "1001"
#myTimeFrame.WaterYear.End      <- "0930"
myTimeFrame.Season.Spring.Start <- "0301"
#myTimeFrame.Season.Spring.End  <- "0531"
myTimeFrame.Season.Summer.Start <- "0601"
#myTimeFrame.Season.Summer.End  <- "0831"
myTimeFrame.Season.Fall.Start   <- "0901"
#myTimeFrame.Season.Fall.End    <- "1130"
myTimeFrame.Season.Winter.Start <- "1201"
#myTimeFrame.Season.Winter.End  <- "0228" #but 0229 in leap year, use start dates only
# Time Frame Names
myName.Yr       <- "Year"
myName.YrMo     <- "YearMonth"
myName.Mo       <- "Month"
myName.MoDa     <- "MonthDay"
myName.JuDa     <- "JulianDay"
myName.Day      <- "Day"
myName.Season   <- "Season"
myName.YrSeason <- "YearSeason"
# for summary stats
myNames.Fields.TimePeriods <- c(myName.Yr
                               ,myName.YrMo
                               ,myName.Mo
                               ,myName.MoDa
                               ,myName.JuDa
                               ,myName.Season
                               ,myName.YrSeason)
######################################################################

# Trigger for Stats to exclude (TRUE) or include (FALSE) where flag = "fail"
myStats.Fails.Exclude <- TRUE

######################################################################














