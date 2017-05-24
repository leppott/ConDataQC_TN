# Sourced Routine
##################
# Download USGS Gage Data
##################
# Erik.Leppo@tetratech.com (EWL)
# 20151130
##################
#
# Basic Operations:
# download from USGS based on user selection
# daily means
# 20170524, added Time Zone


# library (load any required helper functions)
library(dataRetrieval, quietly = TRUE, warn.conflicts = FALSE)

fun.GageData <- function(fun.myData.SiteID
                         ,fun.myData.Type
                         ,fun.myData.DateRange.Start
                         ,fun.myData.DateRange.End
                         ,fun.myDir.SUB.import
                         ,fun.myDir.SUB.export
                         ,fun.myTZ=myTZ) {

  # data directories
  #
  myDir.data.import <- fun.myDir.SUB.import
  myDir.data.export <- fun.myDir.SUB.export

  # Start Time (used to determine run time at end)
  myTime.Start <- Sys.time()

  myDate <- format(Sys.Date(),"%Y%m%d")
  myTime <- format(myTime.Start,"%H%M%S")
  #
  # Verify input dates, if blank, NA, or null use all data
  # if DateRange.Start is null or "" then assign it 1900-01-01
  if (is.na(fun.myData.DateRange.Start)==TRUE||fun.myData.DateRange.Start==""){fun.myData.DateRange.Start<-DateRange.Start.Default}
  # if DateRange.End is null or "" then assign it today
  if (is.na(fun.myData.DateRange.End)==TRUE||fun.myData.DateRange.End==""){fun.myData.DateRange.End<-DateRange.End.Default}


  ##################################
  #    DD parameter   Description ("parameter_nm" from whatNWISdata)
  #    01   00060     Discharge, cubic feet per second
  #    02   00065     Gage height, feet
  #    05   00010     Temperature, water, degrees Celsius
  #    06   00020     Temperature, air, degrees Celsius
  #         00095     Conductivity
  #         00040     pH
  #         00300     DO
  #         63680     turbidity
  #         00045     precip
  #         62611     GWL
  #         72019     WLBLS
  #         00045     Precipitation, total, inches
  #
  #   param.code <- c("00060"
  #                   ,"00065"
  #                   ,"00010"
  #                   ,"00020"
  #                   ,"00040"
  #                   ,"00045")
  #   param.desc <- c("Discharge, cubic feet per second"
  #                   ,"Gage height, feet"
  #                   ,"Temperature, water, degrees Celsius"
  #                   ,"Temperature, air, degrees Celsius"
  #                   ,"pH"
  #                   ,"Precipitation, total, inches"
  #                   )
  #   USGS.Code.Desc <- as.data.frame(cbind(param.code,param.desc))
  #   names(USGS.Code.Desc) <- c("Code","Desc")
  #
  ####################
  # USGS Statistic Codes
  # http://help.waterdata.usgs.gov/codes-and-parameters
  # 00011 Instantaneous
  # 00001 Max
  # 00002 Min
  # 00003 Mean (dataRetrieval default)
  # 00006 Sum

  # Define Counters for the Loop
  intCounter <- 0
  intCounter.Stop <- length(fun.myData.SiteID)
  intItems.Total <- intCounter.Stop
  print(paste0("Total items to process = ", intItems.Total))
  flush.console()

  myItems.Complete  <- 0

  ######################
  # Loop through sites
  ######################

  while (intCounter < intCounter.Stop)
  {
    intCounter <- intCounter+1
    site_id <- fun.myData.SiteID[intCounter]
    #
    # Get available data
    data.what.uv <- whatNWISdata(site_id,service="uv")
    # future versions to get all available data
    data.what.uv.param <- data.what.uv[,"parameter_nm"]
    #
    #data.what.Codes <- as.vector(USGS.Code.Desc[,"Code"][data.what.uv[,"parameter_nm"]%in%USGS.Code.Desc$Desc])
    data.what.Codes <- data.what.uv[,"parm_cd"]

    # inform user

    flog.debug(paste0("Getting available data for ",site_id))

    # print(data.what.uv)

    flush.console()

    myCode <- data.what.Codes #"00060" #c("00060","00065") # can download multiple at one time
    myStat <- "00003"  #data, not daily values
    data.myGage <- readNWISuv(site_id
                              ,myCode
                              ,startDate=fun.myData.DateRange.Start
                              ,endDate=fun.myData.DateRange.End
                              ,tz=fun.myTZ )

    # column headers are "X_myCode_myStat"
    # can put in multipe and it only runs on those present
    data.myGage <- renameNWISColumns(data.myGage
                      ,p00060=myName.Discharge
                      ,p00065=myName.WaterLevel
                      ,p00010=myName.WaterTemp
                      ,p00020=myName.AirTemp
                      ,p00040="pH"
                      ,p00045="Precip.Total.in"
                      ,p00011=gsub(".C",".F",myName.WaterTemp)
                      )
    # different data structure for dataRetrieval
    names(data.myGage)

    # drop columns for Agency Code and TimeZone
    myDrop <- c("agency_cd","tz_cd")
    myKeep <- names(data.myGage)[! names(data.myGage) %in% myDrop]

    data.myGage <- data.myGage[,myKeep]


    ##############
    # hard code only Discharge due to time limits on project

  #   NewNames <- c(myName.SiteID,myName.DateTime,myName.Discharge,paste("_cd",myName.Discharge,sep="."))
  #   names(data.myGage) <- NewNames


    # replace "_Inst" with null and leave "_cd"
    names(data.myGage) <- gsub("_Inst","",names(data.myGage))
    # mod SiteID and DateTIme
    names(data.myGage)[1:2] <- c(myName.SiteID,myName.DateTime)

    ## Add GageID field so can retain (20160205)
    data.myGage <- cbind(GageID=site_id, data.myGage)

    # Rework Start and End Dates to match data in file
    strFile.Date.Start  <- format(min(data.myGage[,myName.DateTime]),myFormat.Date)
    strFile.Date.End    <- format(max(data.myGage[,myName.DateTime]),myFormat.Date)

    # 10.0. Output file
    # 10.1. Set Name
    File.Date.Start <- format(as.Date(strFile.Date.Start, myFormat.Date),"%Y%m%d")
    File.Date.End   <- format(as.Date(strFile.Date.End, myFormat.Date),  "%Y%m%d")

    strFile.Out.Prefix <- "Gage"
    strFile.Out <- paste(paste(site_id,
                               fun.myData.Type,
                               File.Date.Start,
                               File.Date.End,
                               sep=config.FileName.Delimiter),
                            "csv",sep=".")

    # 10.2. Save to File the data (overwrites any existing file).

    strFile.full_name = file.path(myDir.data.export,strFile.Out)

    write.csv(data.myGage, file=file.path(strFile.full_name), quote=FALSE, row.names=FALSE)
    #
    cat(paste0('Wrote CSV file' , "\n\t", strFile.full_name, "\n"))

    # 11.1. Inform user of progress and update LOG
    myMsg <- "COMPLETE"
    myItems.Complete <- myItems.Complete + 1
    fun.Msg.Status(myMsg, intCounter, intItems.Total, site_id)

    flush.console()

    # 11.2. Remove data
    rm(data.myGage)
  }

  ######################
  # Loop through sites
  ######################

  # inform user task complete with status
  print(paste("Task COMPLETE; ",round(difftime(Sys.time(),myTime.Start,units="mins"),2)," min.",sep=""))
  flush.console()
}

