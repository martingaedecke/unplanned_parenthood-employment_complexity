###/////////////////////////////////////////////////###
### Unplanned parenthood and employment complexity  ###
### author: Martin Gädecke                          ###
### Folder setup script                             ###
###/////////////////////////////////////////////////###

# include libaries

### create folder structure
setwd("C:/Users/maddinjedekhe/Documents/OneDrive - Nexus365/00_DPhil-Sociology/01_projects/Unplanned parenthood and employment complexity")

# 02_script
ifelse(!dir.exists("02_script"), dir.create("02_script"), "Folder exists already")

# 03_graph
ifelse(!dir.exists("03_graph"), dir.create("03_graph"), "Folder exists already")

# 04_table
ifelse(!dir.exists("04_table"), dir.create("04_table"), "Folder exists already")

# 05_log
ifelse(!dir.exists("05_log"), dir.create("05_log"), "Folder exists already")

# 06_notes
ifelse(!dir.exists("06_notes"), dir.create("06_notes"), "Folder exists already")

# 11_manuscript
ifelse(!dir.exists("11_manuscript"), dir.create("11_manuscript"), "Folder exists already")