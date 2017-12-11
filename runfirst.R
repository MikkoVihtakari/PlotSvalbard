### Run first script for inter-OS package development

#rm(list = ls()) ## Clear workspace

## Libraries, remember to add these as dependencies if needed

library(PlotSvalbard)
library(sp)
library(rgdal)
#library(maptools)
library(rgeos) #gIntersect

## Define paths

if(Sys.info()["sysname"] == "Windows") {
  devel <- "C:/Users/mikko/Dropbox/Workstuff/R/R packages/PlotSvalbard_development/"
  } else {
  devel <- "/Users/mvi023/Dropbox/Workstuff/R/R packages/PlotSvalbard_development/"
}

#### Springer journal widths in inches ####

columnwidth <- 84*0.0393700787 # 84 mm
halfpagewidth <- 129*0.0393700787 # 129 mm
pagewidth <- 174*0.0393700787 # 174 mm
maxheight <- 234*0.0393700787 # 234 mm
