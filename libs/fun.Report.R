# Sourced Routine
##################
# Generate QC Report
#########################
# Erik.Leppo@tetratech.com (EWL)
# 20151022
###################
# Basic Operations:
# load all files in data directory
# find ones specified by user
# generate QC data summaries
# output to PDF
#####################
## ideas
# load one file instead of all
################

# library (load any required helper functions)

library(knitr, quietly = TRUE, warn.conflicts = FALSE)



fun.Report_file <- function(input_file_name,
                            import_folder,
                            export_folder) {

  input_file_path = file.path(export_folder, input_file_name)

  #
  # Convert Data Type to Camel Case
  # fun.myData.Type <- fun.CamelCase(fun.myData.Type)
  #
  # #
  # # data directories
  # myDir.data.import <- fun.myDir.SUB.import
  # myDir.data.export <- fun.myDir.SUB.export

  #
  # Start Time (used to determine run time at end)
  myTime.Start <- Sys.time()

  myDate <- format(Sys.Date(),"%Y%m%d")
  myTime <- format(myTime.Start,"%H%M%S")

  input_file_name_base <- substr(input_file_name, 1, nchar(input_file_name)- nchar(".csv"))
  strFile.parts <- strsplit(input_file_name_base, config.FileName.Delimiter)
  #
  # 0. Load Single file
  strFile.Prefix     <- strFile.parts[[1]][1]
  strFile.SiteID     <- strFile.parts[[1]][2]
  strFile.DataType   <- strFile.parts[[1]][3]
  strFile.Date.Start <- strFile.parts[[1]][4]
  strFile.Date.End   <- strFile.parts[[1]][5]

  fun.myData.SiteID <- strFile.SiteID
  fun.myData.DateRange.Start <- strFile.Date.Start
  fun.myData.DateRange.End <- strFile.Date.End


  # strFile = paste(paste(strFile.Prefix,
  #                       strFile.SiteID,
  #                       fun.myData.Type,
  #                       strFile.Date.Start,
  #                       strFile.Date.End,
  #                       sep=config.FileName.Delimiter),
  #                 "csv", sep=".")
  strFile = input_file_name
  # strFile.source.full_name = file.path(myDir.data.import, strFile)
  #
  # if (! file.exists(strFile.source.full_name))
  # {
  #   flog.error(paste0("1. Unable to find CSV file to use to create report document:\n\t", strFile.source.full_name))
  #   stop()
  # }
  #
  # strFile.Base <- substr(strFile,1,nchar(strFile)-nchar(".csv"))
  # strFile.parts <- strsplit(strFile.Base,config.FileName.Delimiter)

  # import the file
  data.import <- read.csv(input_file_path, as.is=TRUE, na.strings="")

  # pick 'report' based on prefix
  myReport.Name <- "Report_QC"

  # prepare the complete name to the markdown file
  strFile.RMD <- file.path(config.Folder.Markdown, paste(myReport.Name,"rmd", sep="."))

  if (! file.exists(strFile.RMD))
  {
    stop(paste0("1. Unable to find markdown file:\n\t", strFile.RMD))
  }

  # prepare the filename
  strFile.DOCX <- paste0(input_file_name_base,".docx")

  #
  # create the report word document, but hide the messages that look like this
  # NULL
  # Warning messages:
  #   1: In xy.coords(x, y, xlabel, ylabel, log) : NAs introduced by coercion
  # 2: In xy.coords(x, y, xlabel, ylabel, log) : NAs introduced by coercion
  # 3: In xy.coords(x, y, xlabel, ylabel, log) : NAs introduced by coercion
  # 4: In xy.coords(x, y, xlabel, ylabel, log) : NAs introduced by coercion
  # 5: In xy.coords(x, y, xlabel, ylabel, log) : NAs introduced by coercion
  # 6: In xy.coords(x, y, xlabel, ylabel, log) : NAs introduced by coercion
  # 7: In xy.coords(x, y) : NAs introduced by coercion
  # 8: In xy.coords(x, y, xlabel, ylabel, log) : NAs introduced by coercion
  # 9: In xy.coords(x, y, xlabel, ylabel, log) : NAs introduced by coercion
  #
  flog.debug('ready to run markdown')
  suppressWarnings(
    rmarkdown::render(strFile.RMD,
                      output_file=strFile.DOCX,
                      output_dir=export_folder,
                      quiet=TRUE)
  )
  flog.debug('finished markdown')
  #######################################################################

  # Clean up
  rm(data.import)
  rm(data.plot)

  ###########################################################################
  #
  # inform user task complete with status

  export_file_path = file.path(export_folder, strFile.DOCX)

  flog.debug(paste0('Wrote DOCX file' , "\n\t", export_file_path))

  flog.debug(paste0("Task COMPLETE. QC Report.  Total time = ",format(difftime(Sys.time(), myTime.Start))))

  return()
}






# FUNCTION
fun.Report <- function(fun.myData.SiteID
                         ,fun.myData.Type
                         ,fun.myData.DateRange.Start
                         ,fun.myData.DateRange.End
                         ,fun.myDir.SUB.import
                         ,fun.myDir.SUB.export
                         ,fun.myFile.Prefix) {
  #
  # Convert Data Type to Camel Case
  fun.myData.Type <- fun.CamelCase(fun.myData.Type)

  #
  # data directories
  myDir.data.import <- fun.myDir.SUB.import
  myDir.data.export <- fun.myDir.SUB.export

  #
  # Start Time (used to determine run time at end)
  myTime.Start <- Sys.time()

  myDate <- format(Sys.Date(),"%Y%m%d")
  myTime <- format(myTime.Start,"%H%M%S")

  #
  # 0. Load Single file
  strFile.Prefix     <- toupper(fun.myFile.Prefix)     # DATA = Aggregate, QC = QC
  strFile.SiteID     <- fun.myData.SiteID
  strFile.DataType   <- fun.myData.Type
  strFile.Date.Start <- format(as.Date(fun.myData.DateRange.Start,"%Y-%m-%d"),"%Y%m%d")
  strFile.Date.End   <- format(as.Date(fun.myData.DateRange.End,"%Y-%m-%d"),"%Y%m%d")
  strFile = paste(paste(strFile.Prefix,
                        strFile.SiteID,
                        fun.myData.Type,
                        strFile.Date.Start,
                        strFile.Date.End,
                        sep=config.FileName.Delimiter),
                        "csv", sep=".")

  strFile.source.full_name = file.path(myDir.data.import, strFile)

  if (! file.exists(strFile.source.full_name))
  {
    flog.error(paste0("1. Unable to find CSV file to use to create report document:\n\t", strFile.source.full_name))
    stop()
  }

  strFile.Base <- substr(strFile,1,nchar(strFile)-nchar(".csv"))
  strFile.parts <- strsplit(strFile.Base,config.FileName.Delimiter)

  # import the file
  data.import <- read.csv(strFile.source.full_name, as.is=TRUE, na.strings="")

  # pick 'report' based on prefix
  switch(strFile.Prefix,
    "QC"= {
      myReport.Name <- "Report_QC"
    },
    "DATA" = {
      myReport.Name <- "Report_Aggregate"
    },
    "STATS" =
    {
      myReport.Name <- "Report_Stats"
    }
  )

  # prepare the complete name to the markdown file
  strFile.RMD <- file.path(config.Folder.Markdown, paste(myReport.Name,"rmd", sep="."))

  if (! file.exists(strFile.RMD))
  {
    stop(paste0("1. Unable to find markdown file:\n\t", strFile.RMD))
  }

  # prepare the filename
  strFile.DOCX <- paste0(paste(strFile.Prefix,
                                strFile.SiteID,
                                fun.myData.Type,
                                strFile.Date.Start,
                                strFile.Date.End,
                                myReport.Name, sep=config.FileName.Delimiter),".docx")

  #
  # create the report word document, but hide the messages that look like this
  # NULL
  # Warning messages:
  #   1: In xy.coords(x, y, xlabel, ylabel, log) : NAs introduced by coercion
  # 2: In xy.coords(x, y, xlabel, ylabel, log) : NAs introduced by coercion
  # 3: In xy.coords(x, y, xlabel, ylabel, log) : NAs introduced by coercion
  # 4: In xy.coords(x, y, xlabel, ylabel, log) : NAs introduced by coercion
  # 5: In xy.coords(x, y, xlabel, ylabel, log) : NAs introduced by coercion
  # 6: In xy.coords(x, y, xlabel, ylabel, log) : NAs introduced by coercion
  # 7: In xy.coords(x, y) : NAs introduced by coercion
  # 8: In xy.coords(x, y, xlabel, ylabel, log) : NAs introduced by coercion
  # 9: In xy.coords(x, y, xlabel, ylabel, log) : NAs introduced by coercion
  #
  suppressWarnings(
    rmarkdown::render(strFile.RMD,
                      output_file=strFile.DOCX,
                      output_dir=myDir.data.export,
                      quiet=TRUE)
  )
  #######################################################################

  # Clean up
  rm(data.import)
  rm(data.plot)

  ###########################################################################
  #
  # inform user task complete with status

  strFile.export.full_name = file.path(myDir.data.export,strFile.DOCX)

  flog.debug(paste0('Wrote DOCX file' , "\n\t", strFile.export.full_name))

  flog.debug(paste0("Task COMPLETE. QC Report.  Total time = ",format(difftime(Sys.time(), myTime.Start))))

  return()
}



