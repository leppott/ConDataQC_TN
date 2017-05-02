# Get R libraries
# Installs a defined set of libraries
# Created: 20080527, Erik.Leppo@tetratech.com
################################################

# clear the workspace
rm(list=ls())

# libraries to be installed
data.packages = c(
                  "dataRetrieval"   # loads USGS data into R
                  ,"waterData"      # QC of hydro time series data
                  )

# set CRAN mirror
#(loads gui in R; in R-Studio select ## of mirror in Console pane)
# If know mirror can use "ind=" in 2nd statement and comment out (prefix line with #) the first.
chooseCRANmirror()

# install packages via a 'for' loop
for (i in 1:length(data.packages))
{
  install.packages(data.packages[i])
}

print("Task complete.  Check statements above for errors.")