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
}
