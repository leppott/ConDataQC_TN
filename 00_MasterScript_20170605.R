#!/cygdrive/c/Program Files/R/R-3.3.2patched/bin/R
#
# Manipulates Continuous Data Files (run different operations based on user input below)
# Erik.Leppo@tetratech.com (EWL), 2015-11-19
#
#################
# It is assumed that this R script is stored in a directory with the data files as subdirectories.
# If this script is run in TINN-R or R-Studio there is no need to define the working directory
#
####################################################################

# clear the workspace
#
rm( list = ls() )

library(optparse, quietly = TRUE)

# Define working Directory
#
config.Folder.Root <- "C:/Data_and_Tools/tn_realtime/working/R_scripts"

# Define location of the libs Directory
#
config.Folder.Libs <- file.path(config.Folder.Root, "libs")

# load all other libraries via the InitializeLibs.R

setwd(config.Folder.Libs)

isRStudio <- Sys.getenv("RSTUDIO") == "1"

if (isRStudio){
  debugSource("fun.InitializeLibs.R")
} else
{
  source("fun.InitializeLibs.R")
}
setwd(config.Folder.Root)

#####################################################################
#
# USER input in this section (see end of script for explanations)
#
#####################################################################

setwd(config.Folder.Libs)
if (isRStudio){
  debugSource("user_config.R")
} else {
  source("user_config.R")
}
setwd(config.Folder.Root)

# Operation
#
UserOptions.Operation   <- c("GetGageData","QCRaw", "ReportQC", "Aggregate", "ReportAggregate", "SummaryStats")
# UserSelection.Operation <- 'QCRaw'
UserSelection.Operation <- UserOptions.Operation[2]  # number corresponds to intended operation in the line above


# Site ID
# "test2", "HRCC", "PBCC",
UserOptions.SiteID   <-  c("ECO66G12", "ECO66G20", "ECO68C20", "01187300", "02035000")
# UserSelection.SiteID <- "testsiteID"
UserSelection.SiteID <- UserOptions.SiteID[3] #number corresponds to index of the site id array defined
# UserSelection.SiteID <- c(UserOptions.SiteID[7], UserOptions.SiteID[8]) # an array is optional for GetGageData

# data type - Type of data file
#
UserOptions.DataType <- c("Air","Water","AW","AirWater","Gage","AWG","AG","WG")
UserSelection.DataType <- UserOptions.DataType[4] #number corresponds to intended operation in the line above

#
# df[,mCol][df[,myCol]=="P",] <- NA
#

# start date
# YYYY-MM-DD ("-" delimiter), leave blank for all data ("1900-01-01")
#UserSelection.DateRange.Start  <- "2010-09-25"
UserSelection.DateRange.Start  <- ""

# end date
# YYYY-MM-DD ("-" delimiter), leave blank for all data (today)
#UserSelection.DateRange.End    <- "2015-12-11"
UserSelection.DateRange.End    <- ""

# Vector to hold recognized folders.
UserOptions.Folders <- c("C:\\Data_and_Tools\\tn_realtime\\working\\R_scripts\\data\\Data1_RAW",
                         "C:\\Data_and_Tools\\tn_realtime\\working\\R_scripts\\data\\Data2_QC",
                         "Data3_Aggregated",
                         "Data4_Stats",
                         "C:\\inetpub\\wwwdjango\\tndec\\tndecsite\\documents\\incoming",
            						 "C:\\inetpub\\wwwdjango\\tndec\\tndecsite\\documents\\processing",
            						 "C:\\inetpub\\wwwdjango\\tndec\\tndecsite\\documents\\processed\\qc_data",
                         "C:\\software\\Bitnami\\djangostack-1.10.5-0\\apps\\tndec\\tndecsite\\documents\\incoming",
            						 "C:\\software\\Bitnami\\djangostack-1.10.5-0\\apps\\tndec\\tndecsite\\documents\\processing\\rscript_import_folder",
            						 "C:\\software\\Bitnami\\djangostack-1.10.5-0\\apps\\tndec\\tndecsite\\documents\\processed\\qc_data"
                         )

# input folder.  Leave blank for default which is determined using the UserSelection.Operation
#
UserSelection.ImportFolder <- ""
UserSelection.ImportFolder <- UserOptions.Folders[9]


# export folder.  Leave blank for default which is determined using the UserSelection.Operation
#
UserSelection.ExportFolder <- ""
UserSelection.ExportFolder <- UserOptions.Folders[10]

option_list = list(
  make_option(c("-i", "--import_folder"), type="character", default=UserSelection.ImportFolder,
              help="dataset file name", metavar="character"),
  make_option(c("-e", "--export_folder"), type="character", default=UserSelection.ExportFolder,
              help="output file name [default= %default]", metavar="character"),
  make_option(c("-f", "--file_name"), type="character", default="",
              help="dataset file name", metavar="character")
);

opt_parser = OptionParser(option_list=option_list);
opt = parse_args(opt_parser);

UserSelection.ImportFolder = opt$import_folder
UserSelection.ExportFolder = opt$export_folder
UserSelection.File = opt$file_name

if (UserSelection.File != "")
{
  # process the file argument
  fun.QC_file(
    UserSelection.File,
    UserSelection.ImportFolder,
    UserSelection.ExportFolder
  )
  quit("no", 0)
}

#################
#
# Setup and check the import and export folders for this operation and list user settings to this point
#
#################

found_operation <- grep(UserSelection.Operation, UserOptions.Operation, value = TRUE)
if (! length(found_operation) == 1) {
  stop(paste0('Must select a valid [--operation]  Unable to process [',
              UserSelection.Operation, ']!.\nValid options are [',
              paste(UserOptions.Operation, collapse = ', '), ']'))
}
UserSelection.Operation = found_operation

check_delimiter <- grepl(config.FileName.Delimiter, UserSelection.SiteID) #T/F
if(check_delimiter == TRUE)
{
  stop(paste0("SiteID (", UserSelection.SiteID, ") contains the delimiter [", config.FileName.Delimiter,
              "] used in parsing file names.\nChange SiteID name so it does not include the delimiter."))
}

myDate <- format(Sys.Date(),"%Y%m%d")
myTime <- format(Sys.time(),"%H%M%S")

# Verify input dates, if blank, NA, or null use all data
# if DateRange.Start is null or "" then assign it 1900-01-01
if (is.na(UserSelection.DateRange.Start) == TRUE || UserSelection.DateRange.Start=="")
{
  UserSelection.DateRange.Start <- DateRange.Start.Default
}

# if DateRange.End is null or "" then assign it today
if (is.na(UserSelection.DateRange.End)==TRUE || UserSelection.DateRange.End=="")
{
  UserSelection.DateRange.End <- DateRange.End.Default
}

flog.debug(paste0(replicate(80, '#'), collapse=""))
flog.debug(paste0('Data Operation = [', UserSelection.Operation, ']'))
flog.debug(paste0('SiteID = [', UserSelection.SiteID, ']'))
flog.debug(paste0('Data Type = [', UserSelection.DataType, ']'))
flog.debug(paste0('Data Date Range = [', UserSelection.DateRange.Start, '] to [', UserSelection.DateRange.End, ']'))

# get final import and export folders path based on user setting and default value
# if the folder doesn't exist the script exits within the function
UserSelection.ImportFolder <- fun.ImportFolderPath(UserSelection.ImportFolder, UserSelection.Operation)
flog.debug(paste0('Import Directory = [', UserSelection.ImportFolder, ']'))

UserSelection.ExportFolder <- fun.ExportFolderPath(UserSelection.ExportFolder, UserSelection.Operation)
flog.debug(paste0('Export Directory = [', UserSelection.ExportFolder, ']'))

#################
#
# Run function based on value of "UserSelection.Operation"
#
#################

switch(UserSelection.Operation,

  "GetGageData"= {
    fun.GageData(
                  UserSelection.SiteID
                 ,"Gage"
                 ,UserSelection.DateRange.Start
                 ,UserSelection.DateRange.End
                 ,UserSelection.ImportFolder
                 ,UserSelection.ExportFolder
                 )
  },
  # this function also runs the ReportQC
  "QCRaw"= {
    fun.QC(
            UserSelection.SiteID
           ,UserSelection.DataType
           ,UserSelection.DateRange.Start
           ,UserSelection.DateRange.End
           ,UserSelection.ImportFolder
           ,UserSelection.ExportFolder
          )
  },
  "ReportQC" = {
    fun.Report(
                UserSelection.SiteID
               ,UserSelection.DataType
               ,UserSelection.DateRange.Start
               ,UserSelection.DateRange.End
               ,UserSelection.ImportFolder
               ,UserSelection.ExportFolder
               ,"QC"
     )
  },
  "Aggregate" = {
    fun.AggregateData(
                      UserSelection.SiteID
                      ,UserSelection.DataType
                      ,UserSelection.DateRange.Start
                      ,UserSelection.DateRange.End
                      ,UserSelection.ImportFolder
                      ,UserSelection.ExportFolder
                      )
  },
  "ReportAggregate" = {
    fun.Report(
               UserSelection.SiteID
               ,UserSelection.DataType
               ,UserSelection.DateRange.Start
               ,UserSelection.DateRange.End
               ,UserSelection.ImportFolder
               ,UserSelection.ExportFolder
               ,"DATA"
               )
  },
  "SummaryStats" = {
    fun.Stats(
               UserSelection.SiteID
              ,UserSelection.DataType
              ,UserSelection.DateRange.Start
              ,UserSelection.DateRange.End
              ,UserSelection.ImportFolder
              ,UserSelection.ExportFolder
              ,"STATS"
              ,"DATA"
              )
  },
  {
    stop(paste0("Unable to process myData.Operation == ", myData.Operation))
  }
)

# exit script

