### This master script has the purpose to update and load packages and
### set working directories as well as executing seperate do-files


setwd("/Users/martingaedecke/Library/CloudStorage/OneDrive-Nexus365/00_DPhil-Sociology/01_projects/Unplanned parenthood and employment complexity")

project_dir <- getwd()

## Please adjust the data directory to your needs
data_dir <- file.path(project_dir, "01_data")
  data_raw_dir <- file.path(data_dir, "01_raw")
  data_temp_dir <- file.path(data_dir, "02_temp")
  data_posted_dir <- file.path(data_dir, "03_posted")

script_dir <- file.path(project_dir, "02_script")
graph_dir <- file.path(project_dir, "03_graph")
table_dir <- file.path(project_dir, "04_table")
log_dir <- file.path(project_dir, "05_log")

### Install and update packages
source(file.path(script_dir, "00_packages.R"))

### Load dataset
source(file.path(script_dir, "01_loaddata.R"))

### Datacleaning
source('./02_script/02_datacleaning.R')

### Dataanalysis
source('./02_script/03_dataanalysis.R')

