# Sourced Routine
##################
# Statistical Summary
##################
# Erik.Leppo@tetratech.com (EWL)
# 20151120
##################
# 20170116, EWL
# added date & time QC
##################
#
# Basic Operations:
# load all files in data directory
# perform Stats
# write Stat summary file


# library (load any required helper functions)
# library(StreamThermal)
library(survival, quietly = TRUE, warn.conflicts = FALSE) # required for doBy
library(doBy, quietly = TRUE, warn.conflicts = FALSE)
# should have been loaded by master script

fun.Stats <- function(fun.myData.SiteID
                     ,fun.myData.Type
                     ,fun.myData.DateRange.Start
                     ,fun.myData.DateRange.End
                     ,fun.myDir.SUB.import
                     ,fun.myDir.SUB.export
                     ,fun.myProcedure.Step
                     ,fun.myFile.Prefix) {
  #
#   ##########
#   # QC (from fun.Master.R)
#   ##########

#   fun.myProcedure.Step <- "STATS"
#   fun.myFile.Prefix <- "DATA"
#   ###########
  #
  # Error Checking - only 1 SiteID and 1 DataType
  if(length(fun.myData.SiteID)!=1){
    myMsg <- "Function can only handle 1 SiteID."
    stop(myMsg)
  }
  if(length(fun.myData.Type)!=1){
    myMsg <- "Function can only handle 1 Data Type."
    stop(myMsg)
  }

  # Convert Data Type to Camel Case
  fun.myData.Type <- paste0(toupper(substring(fun.myData.Type,1,1)),
                           tolower(substring(fun.myData.Type,2,nchar(fun.myData.Type))))
  #
  # data directories
  myDir.data.import <- fun.myDir.SUB.import
  myDir.data.export <- fun.myDir.SUB.export

  #
  myDate <- format(Sys.Date(),"%Y%m%d")
  myTime <- format(Sys.time(),"%H%M%S")

  # Verify input dates, if blank, NA, or null use all data
  # if DateRange.Start is null or "" then assign it 1900-01-01
  if (is.na(fun.myData.DateRange.Start) == TRUE || fun.myData.DateRange.Start==""){
    fun.myData.DateRange.Start <- DateRange.Start.Default
  }

  # if DateRange.End is null or "" then assign it today
  if (is.na(fun.myData.DateRange.End)==TRUE||fun.myData.DateRange.End==""){
    fun.myData.DateRange.End<-DateRange.End.Default
  }

  # 0. Load Single file
  strFile.Prefix     <- toupper(fun.myFile.Prefix)     # DATA = Aggregate, QC = QC
  strFile.SiteID     <- fun.myData.SiteID
  strFile.DataType   <- fun.myData.Type
  strFile.Date.Start <- format(as.Date(fun.myData.DateRange.Start,"%Y-%m-%d"), "%Y%m%d")
  strFile.Date.End   <- format(as.Date(fun.myData.DateRange.End,  "%Y-%m-%d"), "%Y%m%d")

  strFile <- paste(paste(strFile.Prefix,
                         strFile.SiteID,
                         fun.myData.Type,
                         strFile.Date.Start,
                         strFile.Date.End,
                         sep=config.FileName.Delimiter), "csv", sep=".")

  strFile.Base  <- substr(strFile,1,nchar(strFile) - nchar(".csv"))
  strFile.parts <- strsplit(strFile.Base, config.FileName.Delimiter)

  # QC, make sure file exists
  if(strFile %in% list.files(path=myDir.data.import)==FALSE) {
    #
    stop(paste0("Unable to generate summary statistics.  Can't find input file\n\t",
                file.path(myDir.data.import, strFile)))
  }

  #import the file
  data.import <- read.csv(file.path(myDir.data.import, strFile), as.is=TRUE, na.strings="")
  #
  # QC required fields: SiteID & (DateTime | (Date & Time))
  fun.helper.CheckRequiredFields(names(data.import),paste(myDir.data.import,strFile,sep="/"))
  #

  #
  # QC date and time
  # accessing files with Excel can change formats
  # 20170116, EWL
  data.import <- fun.QC.datetime(data.import)


  # Define time period fields

  myNames.Fields.TimePeriods <- c(myName.Yr,myName.YrMo,myName.MoDa,myName.Mo,myName.JuDa,myName.Season,myName.YrSeason)
  # add time period fields
  data.import[,myName.Yr]   <- format(as.Date(data.import[,myName.Date]),format="%Y")
  data.import[,myName.Mo]   <- format(as.Date(data.import[,myName.Date]),format="%m")
  data.import[,myName.YrMo] <- format(as.Date(data.import[,myName.Date]),format="%Y%m")
  data.import[,myName.MoDa] <- format(as.Date(data.import[,myName.Date]),format="%m%d")
  data.import[,myName.JuDa] <- as.POSIXlt(data.import[,myName.Date], format=myFormat.Date)$yday+1
  ## add Season fields
#   md <- data.import[,myName.MoDa]
#   data.import[,myName.Season] <- NA
#   data.import[,myName.Season][as.numeric(md)>=as.numeric("0101") & as.numeric(md)<as.numeric(myTimeFrame.Season.Spring.Start)] <- "Winter"
#   data.import[,myName.Season][as.numeric(md)>=as.numeric(myTimeFrame.Season.Spring.Start) & as.numeric(md)<as.numeric(myTimeFrame.Season.Summer.Start)] <- "Spring"
#   data.import[,myName.Season][as.numeric(md)>=as.numeric(myTimeFrame.Season.Summer.Start) & as.numeric(md)<as.numeric(myTimeFrame.Season.Fall.Start)] <- "Summer"
#   data.import[,myName.Season][as.numeric(md)>=as.numeric(myTimeFrame.Season.Fall.Start) & as.numeric(md)<as.numeric(myTimeFrame.Season.Winter.Start)] <- "Fall"
#   data.import[,myName.Season][as.numeric(md)>=as.numeric(myTimeFrame.Season.Winter.Start) & as.numeric(md)<as.numeric("1231")] <- "Winter"
#   data.import[,myName.SeasonYr] <- paste(data.import[,"Season"],data.import[,"Year"],sep="")
  data.import[,myName.Season] <- NA
  data.import[,myName.Season][as.numeric(data.import[,myName.MoDa])>=as.numeric("0101") & as.numeric(data.import[,myName.MoDa])<as.numeric(myTimeFrame.Season.Spring.Start)] <- "Winter"
  data.import[,myName.Season][as.numeric(data.import[,myName.MoDa])>=as.numeric(myTimeFrame.Season.Spring.Start) & as.numeric(data.import[,myName.MoDa])<as.numeric(myTimeFrame.Season.Summer.Start)] <- "Spring"
  data.import[,myName.Season][as.numeric(data.import[,myName.MoDa])>=as.numeric(myTimeFrame.Season.Summer.Start) & as.numeric(data.import[,myName.MoDa])<as.numeric(myTimeFrame.Season.Fall.Start)] <- "Summer"
  data.import[,myName.Season][as.numeric(data.import[,myName.MoDa])>=as.numeric(myTimeFrame.Season.Fall.Start) & as.numeric(data.import[,myName.MoDa])<as.numeric(myTimeFrame.Season.Winter.Start)] <- "Fall"
  data.import[,myName.Season][as.numeric(data.import[,myName.MoDa])>=as.numeric(myTimeFrame.Season.Winter.Start) & as.numeric(data.import[,myName.MoDa])<as.numeric("1231")] <- "Winter"
  data.import[,myName.YrSeason] <- paste(data.import[,myName.Yr],data.import[,myName.Season],sep="")

  #
  # Loop - Parameter (n=3)
  ## Temperature (Air/Water)
  ## Flow (WaterLevel and Discharge)
  ## Nothing on Pressure (used to calculate waterlevel)
  # future add pH, Cond, etc from USGS gages
  myFields.Data       <- c(myName.WaterTemp, myName.AirTemp, myName.SensorDepth
                           ,myName.Discharge, myName.Cond, myName.DO, myName.pH
                           ,myName.Turbidity, myName.Chlorophylla, myName.GageHeight)
  myFields.Data.Flags <- paste0(myName.Flag,".",myFields.Data)
  myFields.Type       <- c("Thermal","Thermal","Hydrologic"
                           ,"Hydrologic", "WaterChemistry", "WaterChemistry", "WaterChemistry"
                           , "WaterChemistry", "WaterChemistry", "Hydrologic")
  myFields.Keep <- c(myName.SiteID
                     , myName.Date
                     , myName.Time
                     , myName.DateTime
                     , myNames.Fields.TimePeriods
                     , myFields.Data
                     , myFields.Data.Flags
                     )
  # keep only fields needed for stats
 # data.import <- data.import[,myFields.Keep]

  ###############################################

    data2process <- myFields.Data[myFields.Data %in% names(data.import)]
    print(paste("Total items to process = ",length(data2process),":",sep=""))
    print(data2process)
      flush.console()

  ############## QC
  #i <- myFields.Data[1] #QC
  ####################### change from myFields.Data to data2process (need to fix)

  for (i in data2process) {##FOR.i.START
    #
    i.num <- match(i,myFields.Data)
    Fields2Drop <- myFields.Data[-i.num]
    data.stats <- data.import[,!(names(data.import) %in% Fields2Drop)]
    # change fails to NA (so can na.rm=T when run stats)
      # flag field
      myFlag <- myFields.Data.Flags[i.num]
    #data.stats.nofail <- data.stats[data.stats[,myFields.Data.Flags[i.num]]!=myFlagVal.Fail,]

    # 20170519, feedback to user
    print(paste0("Processing item ",i.num," of ",length(data2process),"; ",i))
    flush.console()
    #data.stats.nofail <- data.stats
    #data.stats.nofail[data.stats.nofail[,data.stats[,myFields.Data.Flags[i.num]]=myFlagVal.Fail]] <- na

    # change fail to NA for i (only if user define value == FALSE)
    if(myStats.Fails.Exclude==TRUE) {##IF.myStats.Fails.Include.START
      #
      data.stats[,i][data.stats[,myFlag]==myFlagVal.Fail] <- NA
      #
    }##IF.myStats.Fails.Exclude.END

    #
    # summaryBy doesn't work with Group as variable (change value for running here)
    # have to change some back for dv.i when save
    names(data.stats)[names(data.stats) %in% myName.Date] <- "Date"
    names(data.stats)[names(data.stats) %in% myName.YrMo] <- "YearMonth"
    names(data.stats)[names(data.stats) %in% myName.YrSeason] <- "YearSeason"
    names(data.stats)[names(data.stats) %in% myName.Yr] <- "Year"


    # summaryBy not working with "i" as variable.  Have to do an ugly hack to get it working

#     # QC
#     print("test2")
#     print(i)
#     print("data.stats")
#     print(head(data.stats))
#     flush.console()
#
#
#     data(dietox)
#     dietox12    <- subset(dietox,Time==12)
#     j <- "Weight"
#
#     x<-summaryBy(as.numeric(Weight)+Feed~Evit+Cu, data=dietox12,
#               FUN=c(mean,var,length))
#
#     print(x)
#     flush.console()
#
#    # myDF <- data.stats
#     #x <- summaryBy(as.numeric(Water.Temp.C)~Date,data=myDF,FUN=c(mean),na.rm=TRUE)
#     #print(dim(x))


    # Create Daily Values (mean) (DV is USGS term so use that)

    # if(i==myFields.Data[1]) {
    #   dv.i <- summaryBy(as.numeric(Water.Temp.C)~Date, data=data.stats, FUN=c(mean), na.rm=TRUE
    #                     , var.names="i",id=c(myName.SiteID,"Year","YearMonth",myName.Mo,myName.MoDa,myName.JuDa,myName.Season,"YearSeason"))
    # } else if(i==myFields.Data[2]) {
    #   dv.i <- summaryBy(as.numeric(Air.Temp.C)~Date, data=data.stats, FUN=c(mean), na.rm=TRUE
    #                     , var.names="i",id=c(myName.SiteID,"Year","YearMonth",myName.Mo,myName.MoDa,myName.JuDa,myName.Season,"YearSeason"))
    # } else if (i==myFields.Data[3]) {
    #   dv.i <- summaryBy(as.numeric(Water.Level.ft)~Date, data=data.stats, FUN=c(mean), na.rm=TRUE
    #                     , var.names="i",id=c(myName.SiteID,"Year","YearMonth",myName.Mo,myName.MoDa,myName.JuDa,myName.Season,"YearSeason"))
    # }


#     dv.i <- summaryBy(as.numeric(data.stats[,i])~Date, data=data.stats, FUN=c(mean), na.rm=TRUE
#                       , var.names="i")#,id=c(myName.SiteID,"Year","YearMonth",myName.MoDa,myName.JuDa,myName.Season,"YearSeason"))
#

    # 20170519, fix hard coded names
    #
    # name to myVar then name back
    ColNum.i <- match(i,names(data.stats))
    names(data.stats)[ColNum.i] <- "myVar"
    dv.i <- doBy::summaryBy(as.numeric(myVar)~Date, data=data.stats, FUN=c(mean), na.rm=TRUE
                            , var.names="i",id=c(myName.SiteID, myName.Yr , myName.YrMo, myName.Mo, myName.MoDa
                                                 , myName.JuDa, myName.Season, myName.YrSeason))
    names(data.stats)[ColNum.i] <- i
    #

    # rename fields back (use dv.i generated by summaryBy)
    names(dv.i)[2] <- "mean"
    names(dv.i)[names(dv.i) %in% "Date"] <- myName.Date
    names(dv.i)[names(dv.i) %in% "YearMonth"] <- myName.YrMo
    names(dv.i)[names(dv.i) %in% "YearSeason"] <- myName.YrSeason
    names(dv.i)[names(dv.i) %in% "Year"] <- myName.Yr
    # add parameter as column
    dv.i[,"Parameter"] <- i
    # rearrange columns
    dv.i.ColOrder <- c(myName.SiteID,"Parameter","mean",myName.Date,myNames.Fields.TimePeriods)
    dv.i <- dv.i[,dv.i.ColOrder]

    # save dv
    strFile.Prefix.Out <- "DV"
    strFile.Out <- paste(paste(strFile.Prefix.Out,strFile.SiteID,fun.myData.Type,strFile.Date.Start,strFile.Date.End,i,sep=config.FileName.Delimiter),"csv",sep=".")
    write.csv(dv.i,paste(myDir.data.export,strFile.Out,sep="/"),quote=FALSE,row.names=FALSE)

    # calculate daily mean, max, min, range, sd, n
    # Define FUNCTION for use with summaryBy
    myQ <- c(0.01,0.05,0.10,0.25,0.50,0.75,0.90,0.95,0.99)
    myFUN.Names <- c("mean","median","min","max","range","sd","var","cv","n",paste("q",formatC(100*myQ,width=2,flag="0"),sep=""))
    #
    myFUN.sumBy <- function(x, ...){##FUN.myFUN.sumBy.START
      c(mean=mean(x,na.rm=TRUE)
        ,median=median(x,na.rm=TRUE)
        ,min=min(x,na.rm=TRUE)
        ,max=max(x,na.rm=TRUE)
        ,range=max(x,na.rm=TRUE)-min(x,na.rm=TRUE)
        ,sd=sd(x,na.rm=TRUE)
        ,var=var(x,na.rm=TRUE)
        ,cv=sd(x,na.rm=TRUE)/mean(x,na.rm=TRUE)
        ,n=length(x)
        ,q=quantile(x,probs=myQ,na.rm=TRUE)
        )
    }##FUN.myFUN.sumBy.END
    #
    #
    # summaryBy doesn't work with Group as variable (change value for running here)
    # have to change some back for dv.i.* when save
    #names(data.stats)[names(data.stats) %in% myName.Date] <- "Date"
    names(data.stats)[names(data.stats) %in% myName.Date] <- "Date"
    names(data.stats)[names(data.stats) %in% myName.YrMo] <- "YearMonth"
    names(data.stats)[names(data.stats) %in% myName.YrSeason] <- "YearSeason"
    names(data.stats)[names(data.stats) %in% myName.Yr] <- "Year"
    names(data.stats)[names(data.stats) %in% myName.Mo] <- "Month"
    names(data.stats)[names(data.stats) %in% myName.Season] <- "Season"

    #
    #
    # Save plots as PDF
    strFile.Prefix.Out <- fun.myProcedure.Step
    strFile.plot <- paste(paste(strFile.Prefix.Out,strFile.SiteID,fun.myData.Type,strFile.Date.Start,strFile.Date.End,i,sep=config.FileName.Delimiter),"pdf",sep=".")
    pdf(file=paste(myDir.data.export,strFile.plot,sep="/"),width=11,height=8.5)

      #
      ## Daily
        myTimeFrame <- "day"
        myTF.Field <- myName.Date
        myDF <- data.stats
        #stats.i <- summaryBy(as.numeric(myDF[,i])~Date,data=myDF,FUN=myFUN.sumBy,var.names=myTimeFrame)
        ####### ugly hack
        if(i==myFields.Data[1]) {
          stats.i <- summaryBy(as.numeric(Water.Temp.C)~Date,data=myDF,FUN=myFUN.sumBy,var.names=myTimeFrame)
        } else if(i==myFields.Data[2]) {
          stats.i <- summaryBy(as.numeric(Air.Temp.C)~Date,data=myDF,FUN=myFUN.sumBy,var.names=myTimeFrame)
        } else if (i==myFields.Data[3]) {
          stats.i <- summaryBy(as.numeric(Water.Level.ft)~Date,data=myDF,FUN=myFUN.sumBy,var.names=myTimeFrame)
        }
        #######
        # Range
        #stats.i[,paste(myTimeFrame,"range",sep=".")] <- stats.i[,paste(myTimeFrame,"max",sep=".")] - stats.i[,paste(myTimeFrame,"min",sep=".")]
        # rename
        names(stats.i) <- c("TimeValue",myFUN.Names)
        stats.i[,"Parameter"] <- i
        stats.i[,"TimeFrame"] <- myTimeFrame
        stats.i.d <- stats.i
        # plot
        myPlot.Type <- ifelse(nrow(stats.i)==1,"p","l")
        #strFile.plot <- paste(paste(strFile.Prefix.Out,strFile.SiteID,fun.myData.Type,strFile.Date.Start,strFile.Date.End,i,myTimeFrame,sep="_"),"png",sep=".")
        #png(file=paste(myDir.data.export,strFile.plot,sep="/"))
          plot(stats.i$mean,type=myPlot.Type
               ,main=i,ylab="mean",xlab=myTimeFrame,xaxt="n"
               ,ylim=c(min(stats.i$min),max(stats.i$max)))
          myCol <- "gray"
          lines(stats.i$max,col=myCol)
          lines(stats.i$min,col=myCol)
          polygon(c(1:nrow(stats.i),rev(1:nrow(stats.i))),c(stats.i$max,rev(stats.i$min)),col=myCol,border=NA)
          lines(stats.i$mean)
          # X-Axis
          n.Total <- length(factor(stats.i[,"TimeValue"]))
          pct <- c(20,40,60,80,100)*.01
          myAT <- c(1,round(n.Total * pct,0))
          myLab <- stats.i[,"TimeValue"][myAT]
          axis(1,at=myAT,labels=myLab,tick=TRUE)
        #dev.off()
      #
        ## Julian Day
        myTimeFrame <- "JulianDay"
        myTF.Field <- myName.JuDa
        myDF <- dv.i
        #stats.i <- summaryBy(as.numeric(myDF[,i])~YearMonth,data=myDF,FUN=myFUN.sumBy,var.names=myTimeFrame)
        ####### ugly hack
        stats.i <- summaryBy(as.numeric(mean)~JulianDay,data=myDF,FUN=myFUN.sumBy,var.names=myTimeFrame)
        #######
        #Range
        #stats.i[,paste(myTimeFrame,"range",sep=".")] <- stats.i[,paste(myTimeFrame,"max",sep=".")] - stats.i[,paste(myTimeFrame,"min",sep=".")]
        # rename
        names(stats.i) <- c("TimeValue",myFUN.Names)
        stats.i[,"Parameter"] <- i
        stats.i[,"TimeFrame"] <- myTimeFrame
        stats.i.jd <- stats.i
        # plot
        myPlot.Type <- ifelse(nrow(stats.i)==1,"p","l")
        #strFile.plot <- paste(paste(strFile.Prefix.Out,strFile.SiteID,fun.myData.Type,strFile.Date.Start,strFile.Date.End,i,myTimeFrame,sep="_"),"png",sep=".")
        #png(file=paste(myDir.data.export,strFile.plot,sep="/"))
        plot(stats.i$mean,type=myPlot.Type
             ,main=i,ylab="mean",xlab=myTimeFrame,xaxt="n"
             ,ylim=c(min(stats.i$min),max(stats.i$max)))
        myCol <- "gray"
        lines(stats.i$max,col=myCol)
        lines(stats.i$min,col=myCol)
        polygon(c(1:nrow(stats.i),rev(1:nrow(stats.i))),c(stats.i$max,rev(stats.i$min)),col=myCol,border=NA)
        lines(stats.i$mean)
        # X-Axis
        n.Total <- length(factor(stats.i[,"TimeValue"]))
        pct <- c(20,40,60,80,100)*.01
        myAT <- c(1,round(n.Total * pct,0))
        myLab <- stats.i[,"TimeValue"][myAT]
        axis(1,at=myAT,labels=myLab,tick=TRUE)
        #dev.off()
        #
      ## Year_Month
        myTimeFrame <- "year_month"
        myTF.Field <- myName.YrMo
        myDF <- dv.i
        #stats.i <- summaryBy(as.numeric(myDF[,i])~YearMonth,data=myDF,FUN=myFUN.sumBy,var.names=myTimeFrame)
        ####### ugly hack
          stats.i <- summaryBy(as.numeric(mean)~YearMonth,data=myDF,FUN=myFUN.sumBy,var.names=myTimeFrame)
        #######
        #Range
        #stats.i[,paste(myTimeFrame,"range",sep=".")] <- stats.i[,paste(myTimeFrame,"max",sep=".")] - stats.i[,paste(myTimeFrame,"min",sep=".")]
        # rename
        names(stats.i) <- c("TimeValue",myFUN.Names)
        stats.i[,"Parameter"] <- i
        stats.i[,"TimeFrame"] <- myTimeFrame
        stats.i.ym <- stats.i
        # plot
        myPlot.Type <- ifelse(nrow(stats.i)==1,"p","l")
        #strFile.plot <- paste(paste(strFile.Prefix.Out,strFile.SiteID,fun.myData.Type,strFile.Date.Start,strFile.Date.End,i,myTimeFrame,sep="_"),"png",sep=".")
        #png(file=paste(myDir.data.export,strFile.plot,sep="/"))
        plot(stats.i$mean,type=myPlot.Type
             ,main=i,ylab="mean",xlab=myTimeFrame,xaxt="n"
             ,ylim=c(min(stats.i$min),max(stats.i$max)))
        myCol <- "gray"
        lines(stats.i$max,col=myCol)
        lines(stats.i$min,col=myCol)
        polygon(c(1:nrow(stats.i),rev(1:nrow(stats.i))),c(stats.i$max,rev(stats.i$min)),col=myCol,border=NA)
        lines(stats.i$mean)
        # X-Axis
        n.Total <- length(factor(stats.i[,"TimeValue"]))
        myAT <- 1:n.Total
        myLab <- stats.i[,"TimeValue"][myAT]
        axis(1,at=myAT,labels=myLab,tick=TRUE)
        #dev.off()
      #
        #
        ## Month (all years)
        myTimeFrame <- "month"
        myTF.Field <- myName.Mo
        myDF <- dv.i
        #stats.i <- summaryBy(as.numeric(myDF[,i])~YearMonth,data=myDF,FUN=myFUN.sumBy,var.names=myTimeFrame)
        ####### ugly hack
        stats.i <- summaryBy(as.numeric(mean)~Month,data=myDF,FUN=myFUN.sumBy,var.names=myTimeFrame)
        #######
        #Range
        #stats.i[,paste(myTimeFrame,"range",sep=".")] <- stats.i[,paste(myTimeFrame,"max",sep=".")] - stats.i[,paste(myTimeFrame,"min",sep=".")]
        # rename
        names(stats.i) <- c("TimeValue",myFUN.Names)
        stats.i[,"Parameter"] <- i
        stats.i[,"TimeFrame"] <- myTimeFrame
        stats.i.m <- stats.i
        # plot
        myPlot.Type <- ifelse(nrow(stats.i)==1,"p","l")
        #strFile.plot <- paste(paste(strFile.Prefix.Out,strFile.SiteID,fun.myData.Type,strFile.Date.Start,strFile.Date.End,i,myTimeFrame,sep="_"),"png",sep=".")
        #png(file=paste(myDir.data.export,strFile.plot,sep="/"))
        plot(stats.i$mean,type=myPlot.Type
             ,main=i,ylab="mean",xlab=myTimeFrame,xaxt="n"
             ,ylim=c(min(stats.i$min),max(stats.i$max)))
        myCol <- "gray"
        lines(stats.i$max,col=myCol)
        lines(stats.i$min,col=myCol)
        polygon(c(1:nrow(stats.i),rev(1:nrow(stats.i))),c(stats.i$max,rev(stats.i$min)),col=myCol,border=NA)
        lines(stats.i$mean)
        # X-Axis
        n.Total <- length(factor(stats.i[,"TimeValue"]))
        myAT <- 1:n.Total
        myLab <- stats.i[,"TimeValue"][myAT]
        axis(1,at=myAT,labels=myLab,tick=TRUE)
        #dev.off()
        #
      ## Year_Season
        myTimeFrame <- "year_season"
        myTF.Field <- myName.YrSeason
        myDF <- dv.i
        #stats.i <- summaryBy(as.numeric(myDF[,i])~SeasonYear,data=myDF,FUN=myFUN.sumBy,var.names=myTimeFrame)
        ####### ugly hack
        stats.i <- summaryBy(as.numeric(mean)~YearSeason,data=myDF,FUN=myFUN.sumBy,var.names=myTimeFrame)
        #######
        # Range
        #stats.i[,paste(myTimeFrame,"range",sep=".")] <- stats.i[,paste(myTimeFrame,"max",sep=".")] - stats.i[,paste(myTimeFrame,"min",sep=".")]
        # rename
        names(stats.i) <- c("TimeValue",myFUN.Names)
        stats.i[,"Parameter"] <- i
        stats.i[,"TimeFrame"] <- myTimeFrame
        stats.i.ys <- stats.i
        # plot
        myPlot.Type <- ifelse(nrow(stats.i)==1,"p","l")
        #strFile.plot <- paste(paste(strFile.Prefix.Out,strFile.SiteID,fun.myData.Type,strFile.Date.Start,strFile.Date.End,i,myTimeFrame,sep="_"),"png",sep=".")
        #png(file=paste(myDir.data.export,strFile.plot,sep="/"))
        plot(stats.i$mean,type=myPlot.Type
             ,main=i,ylab="mean",xlab=myTimeFrame,xaxt="n"
             ,ylim=c(min(stats.i$min),max(stats.i$max)))
        myCol <- "gray"
        lines(stats.i$max,col=myCol)
        lines(stats.i$min,col=myCol)
        polygon(c(1:nrow(stats.i),rev(1:nrow(stats.i))),c(stats.i$max,rev(stats.i$min)),col=myCol,border=NA)
        lines(stats.i$mean)
        # X-Axis
        n.Total <- length(factor(stats.i[,"TimeValue"]))
        myAT <- 1:n.Total
        myLab <- stats.i[,"TimeValue"][myAT]
        axis(1,at=myAT,labels=myLab,tick=TRUE)
        #dev.off()
      #
        #
        ## Season (all years)
        myTimeFrame <- "season"
        myTF.Field <- myName.Season
        myDF <- dv.i
        #stats.i <- summaryBy(as.numeric(myDF[,i])~SeasonYear,data=myDF,FUN=myFUN.sumBy,var.names=myTimeFrame)
        ####### ugly hack
        stats.i <- summaryBy(as.numeric(mean)~Season,data=myDF,FUN=myFUN.sumBy,var.names=myTimeFrame)
        #######
        # Range
        #stats.i[,paste(myTimeFrame,"range",sep=".")] <- stats.i[,paste(myTimeFrame,"max",sep=".")] - stats.i[,paste(myTimeFrame,"min",sep=".")]
        # rename
        names(stats.i) <- c("TimeValue",myFUN.Names)
        stats.i[,"Parameter"] <- i
        stats.i[,"TimeFrame"] <- myTimeFrame
        stats.i.s <- stats.i
        # plot
        myPlot.Type <- ifelse(nrow(stats.i)==1,"p","l")
        #strFile.plot <- paste(paste(strFile.Prefix.Out,strFile.SiteID,fun.myData.Type,strFile.Date.Start,strFile.Date.End,i,myTimeFrame,sep="_"),"png",sep=".")
        #png(file=paste(myDir.data.export,strFile.plot,sep="/"))
        plot(stats.i$mean,type=myPlot.Type
             ,main=i,ylab="mean",xlab=myTimeFrame,xaxt="n"
             ,ylim=c(min(stats.i$min),max(stats.i$max)))
        myCol <- "gray"
        lines(stats.i$max,col=myCol)
        lines(stats.i$min,col=myCol)
        polygon(c(1:nrow(stats.i),rev(1:nrow(stats.i))),c(stats.i$max,rev(stats.i$min)),col=myCol,border=NA)
        lines(stats.i$mean)
        # X-Axis
        n.Total <- length(factor(stats.i[,"TimeValue"]))
        myAT <- 1:n.Total
        myLab <- stats.i[,"TimeValue"][myAT]
        axis(1,at=myAT,labels=myLab,tick=TRUE)
        #dev.off()
        #
      ## Year
        myTimeFrame <- "year"
        myTF.Field <- myName.Yr
        myDF <- dv.i
        #stats.i <- summaryBy(as.numeric(myDF[,i])~Year,data=myDF,FUN=myFUN.sumBy,var.names=myTimeFrame)
        ####### ugly hack
        stats.i <- summaryBy(as.numeric(mean)~Year,data=myDF,FUN=myFUN.sumBy,var.names=myTimeFrame)
        #######
        # Range
        #stats.i[,paste(myTimeFrame,"range",sep=".")] <- stats.i[,paste(myTimeFrame,"max",sep=".")] - stats.i[,paste(myTimeFrame,"min",sep=".")]
        # rename
        names(stats.i) <- c("TimeValue",myFUN.Names)
        stats.i[,"Parameter"] <- i
        stats.i[,"TimeFrame"] <- myTimeFrame
        stats.i.y <- stats.i
        # plot
        myPlot.Type <- ifelse(nrow(stats.i)==1,"p","l")
        #strFile.plot <- paste(paste(strFile.Prefix.Out,strFile.SiteID,fun.myData.Type,strFile.Date.Start,strFile.Date.End,i,myTimeFrame,sep="_"),"png",sep=".")
        #png(file=paste(myDir.data.export,strFile.plot,sep="/"))
        plot(stats.i$mean,type=myPlot.Type
             ,main=i,ylab="mean",xlab=myTimeFrame,xaxt="n"
             ,ylim=c(min(stats.i$min),max(stats.i$max)))
        myCol <- "gray"
        lines(stats.i$max,col=myCol)
        lines(stats.i$min,col=myCol)
        polygon(c(1:nrow(stats.i),rev(1:nrow(stats.i))),c(stats.i$max,rev(stats.i$min)),col=myCol,border=NA)
        lines(stats.i$mean)
        # X-Axis
        n.Total <- length(factor(stats.i[,"TimeValue"]))
        myAT <- 1:n.Total
        myLab <- stats.i[,"TimeValue"][myAT]
        axis(1,at=myAT,labels=myLab,tick=TRUE)
        #dev.off()
      #
      #

    dev.off()##PDF.END

    #
    #
    # Combine (all the same so just rbind)
    stats.i.ALL <- rbind(stats.i.y,stats.i.s,stats.i.ys,stats.i.m,stats.i.ym,stats.i.jd,stats.i.d)
    stats.i.ALL[,myName.SiteID] <- fun.myData.SiteID

    # rearrange columns (last 2 to first 2)
    myCol.Order <- c(ncol(stats.i.ALL),(ncol(stats.i.ALL)-2),(ncol(stats.i.ALL)-1),1:(ncol(stats.i.ALL)-3))
    #stats.i.ALL <- stats.i.ALL[,c(myName.SiteID,(ncol(stats.i.ALL)-2):(ncol(stats.i.ALL)-1),2:ncol(stats.i.ALL)-3)]
    stats.i.ALL <- stats.i.ALL[,myCol.Order]
    # save stats
    strFile.Prefix.Out <- fun.myProcedure.Step
    strFile.Out <- paste(paste(strFile.Prefix.Out,strFile.SiteID,fun.myData.Type,strFile.Date.Start,strFile.Date.End,i,sep=config.FileName.Delimiter),"csv",sep=".")
    write.csv(stats.i.ALL,paste(myDir.data.export,strFile.Out,sep="/"),quote=FALSE,row.names=FALSE)
    #

    # need to inform user what part of loop


    #
  }

  return()
}


