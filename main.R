library(jsonlite)
library(ggplot2)
library(magrittr)
library(viridis)
library(dplyr)
library(plotly)
# the directory is taken here 
rm(list = ls())
JsonFilePath <- "~/GitHub/data/hOgg1/json/hOgg1_0.1mMNaCl_2016102604_23.5mspf_112nmpp_2-4_2000X.tif.json"
source("~/R/codes/Tracking/function.R")



SourceData <- fromJSON(JsonFilePath) 
RawDataMat <- MakeItMatrix(RawData = SourceData,InputData = SourceData)
RawDataMatNA <- MakeZeroNA(RawDataMat)
LongRaw <- MakeLongForm(RawDataMatNA)

ggplotly(PlotXY1(LongRaw))


FilteredData <- FilterIt(RawData = SourceData, xmin = 2000, xmax = 6000, ymin = 1400, ymax = 1800, intenmax = 1000, intenmin = 30)
FilDataMat <- MakeItMatrix(RawData = SourceData, InputData = FilteredData)
FilDataMatNA <- MakeZeroNA(FilDataMat)
LongFil <- MakeLongForm(FilDataMatNA)

ggplotly(PlotXY3(LongFil,LongFil,LongRaw))


DetDataMat <- FindTrajectory(FilteredDataSet = FilDataMat,dxmax = 600, dxmin = 1, dymax = 200)
DetDataMatNA <- MakeZeroNA(DetDataMat)
LongDet <- MakeLongForm(DetDataMatNA)

ggplotly(PlotFX3(LongDet,LongFil, LongRaw))
ggplotly(PlotXY3(LongDet,LongFil, LongRaw))

ggplotly(PlotFX2(LongDet,LongFil))
ggplotly(PlotXY2(LongDet,LongFil))

ExtDetTra <- data.frame(ExtractTrajectory(LongDet)[1])
ExtDetFra <- data.frame(ExtractTrajectory(LongDet)[2])

ggplotly(PlotFX2(ExtDetTra, LongFil))

ggplotly(PlotFX1TrajOnFra(ExtDetTra))
ggplotly(PlotFX1TrajOnSeq(ExtDetTra))




ggplotly(PlotFX1SingleTraj(ExtDetTra, list(66)))




