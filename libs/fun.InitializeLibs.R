#!/cygdrive/c/Program Files/R/R-3.3.2patched/bin/R
#
# Master Continuous Data Script
# Will prompt user for what to do
# Erik.Leppo@tetratech.com
# 20151118
#################
# Master Functions Script that is invoked from a calling the master script
# The master will have different versions but will all call this one.
#
library(futile.logger, quietly = TRUE, warn.conflicts = FALSE)

# This is used to find the pandoc library
Sys.setenv(RSTUDIO_PANDOC="C:/Users/james.bisese/Documents/RStudio/bin/pandoc")

# Source all libraries
myFiles.debugSource <- file.path(config.Folder.Libs,
                                 c("config.R"
                                   ,"fun.DateFormat.R"
                                   ,"fun.Helper.R"
                                   ,"fun.GageData.R"
                                   ,"fun.QC.R"
                                   ,"fun.Report.R"
                                   ,"fun.AggregateData.R"
                                   ,"fun.Stats.R"
                                  ))
#
sapply(myFiles.debugSource, source, .GlobalEnv)


flog.threshold(config.LoggingLevel)

flog.appender(appender.tee(config.LoggingFile))

return()







