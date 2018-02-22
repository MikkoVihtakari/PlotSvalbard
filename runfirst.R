### Run first script for inter-OS package development

#rm(list = ls()) ## Clear workspace

## Libraries, remember to add these as dependencies if needed

library(PlotSvalbard)
library(sp)
library(rgdal)
#library(maptools)
#library(rgeos) #gIntersect

## Define paths

if(Sys.info()["sysname"] == "Windows") {
  devel <- "C:/Users/mikko/Dropbox/Workstuff/R/R packages/PlotSvalbard_development/"
  presentation <- "C:/Users/mikko/Dropbox/Workstuff/Lectures/20180221 PlotSvalbard presentation NPI/Figures/"
  } else {
  devel <- "/Users/mvi023/Dropbox/Workstuff/R/R packages/PlotSvalbard_development/"
  presentation <- "/Users/mvi023/Dropbox/Workstuff/Lectures/20180221 PlotSvalbard presentation NPI/Figures/"
}

#### Springer journal widths in inches ####

inch <- 0.0393700787

columnwidth <- 84*inch # 84 mm
halfpagewidth <- 129*inch # 129 mm
pagewidth <- 174*inch # 174 mm
maxheight <- 234*inch # 234 mm
slidewidth <- 228*inch

rm(inch)