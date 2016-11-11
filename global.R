## global.R ##
library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(shinyFiles)
library(DBI)
library(RSQLite)
library(jsonlite)



## Load UI components -----------------------------
source("components/Header.R", local = TRUE)
source("components/Sidebar.R", local = TRUE)
source("components/Body.R", local = TRUE)




## Config options --------------------------------
rootOptions <- c(Internal="./data/", Cloud="~/Google Drive/", User="~/")




## Imported database setup -----------------------

initializeDB <- !file.exists("./data/imported.sqlite")
# Default to creating a DB file in ./data

if (initializeDB){
  con <- dbConnect(SQLite(), "./data/imported.sqlite")
  # create import table
  SQLstatement <- "CREATE TABLE `import_record` (`filename`	TEXT, `protein`	TEXT, `NaCl`	NUMERIC, `date`	TEXT, `framerate`	NUMERIC, `pixelsize`	INTEGER, `batch`	TEXT, `dilution`	INTEGER, `metadata`	TEXT, `checksum` TEXT);"
  # write to DB
  dbGetQuery(conn=con, SQLstatement)
  # create raw data table
  SQLstatement <- "CREATE TABLE `raw_data` (`checksum`	TEXT, `id`	INTEGER, `uncertainty [nm]`	NUMERIC, `x [nm]`	NUMERIC, `bkgstd [photon]`	NUMERIC, `frame`	INTEGER, `chi2`	NUMERIC, `sigma [nm]`	NUMERIC, `y [nm]`	NUMERIC, `offset [photon]` NUMERIC, `intensity [photon]`	NUMERIC);"
  # write to DB
  dbGetQuery(conn=con, SQLstatement)
  # create correction factors table
  SQLstatement <- "CREATE TABLE `correction_data` (`checksum`	TEXT, `driftVector_dx`	NUMERIC, `driftVector_dy`	NUMERIC, `driftVector_dFrame`	NUMERIC);"
  # write to DB
  dbGetQuery(conn=con, SQLstatement)
  # create ROI table to record all ROIs of interest
  SQLstatement <- "CREATE TABLE `roi_data` (`checksum`	TEXT, `xmin`	INTEGER, `xmax`	INTEGER, `ymin`	INTEGER, `ymax`	INTEGER, `roi_num`	INTEGER);"
  # write to DB
  dbGetQuery(conn=con, SQLstatement)
  # create run table containing run parameters
  SQLstatement <- "CREATE TABLE `run_data` (`checksum`	TEXT, `roi_num`	INTEGER, `start_frame`	INTEGER, `end_frame`	INTEGER, `run_num`	INTEGER);"
  # write to DB
  dbGetQuery(conn=con, SQLstatement)
  # disconnect
  dbDisconnect(con)

}
