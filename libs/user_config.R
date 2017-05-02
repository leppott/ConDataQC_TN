#
#  user configuration - these are the per-run options and are included in the
#  main script where they will overwrite the values in this file
#

#####################################################################
#
# USER input in this section (see end of script for explanations)
#
#####################################################################

# Operation
#
UserOptions.Operation   <- c("GetGageData","QCRaw", "ReportQC", "Aggregate", "ReportAggregate", "SummaryStats")
# UserSelection.Operation <- 'QCRaw'
UserSelection.Operation <- UserOptions.Operation[1]  # number corresponds to intended operation in the line above


# Site ID
#
UserOptions.SiteID   <-  c("test2", "HRCC", "PBCC", "ECO66G12", "ECO66G20", "ECO68C20", "01187300", "02035000")
# UserSelection.SiteID <- "testsiteID"
UserSelection.SiteID <- UserOptions.SiteID[7] #number corresponds to index of the site id array defined
UserSelection.SiteID <- c(UserOptions.SiteID[7], UserOptions.SiteID[8]) # an array is optional for GetGageData

# data type - Type of data file
#
UserOptions.DataType <- c("Air","Water","AW","Gage","AWG","AG","WG")
UserSelection.DataType <- UserOptions.DataType[2] #number corresponds to intended operation in the line above

# start date
# YYYY-MM-DD ("-" delimiter), leave blank for all data ("1900-01-01")
UserSelection.DateRange.Start  <- "2017-02-01"

# end date
# YYYY-MM-DD ("-" delimiter), leave blank for all data (today)
UserSelection.DateRange.End    <- "2017-02-28"

# Vector to hold recognized folders.
UserOptions.Folders <- c("Data1_RAW",
                         "Data2_QC",
                         "Data3_Aggregated",
                         "Data4_Stats",
                         "C:\\inetpub\\wwwdjango\\tndec\\tndecsite\\documents\\incoming",
                         "C:\\inetpub\\wwwdjango\\tndec\\tndecsite\\documents\\processed\\qc_data",
                         "C:\\software\\Bitnami\\djangostack-1.10.5-0\\apps\\tndec\\tndecsite\\documents\\processed\\qc_data"
)

# input folder.  Leave blank for default which is determined using the UserSelection.Operation
#
UserSelection.ImportFolder <- UserOptions.Folders[2]
UserSelection.ImportFolder <- UserOptions.Folders[5]
UserSelection.ImportFolder <- ""

# export folder.  Leave blank for default which is determined using the UserSelection.Operation
#
UserSelection.ExportFolder <- UserOptions.Folders[2]
UserSelection.ExportFolder <- UserOptions.Folders[6]
UserSelection.ExportFolder <- UserOptions.Folders[7]
UserSelection.ExportFolder <- ""