#!/bin/bash
# Script to install all software needed

R --no-save <<EOF
source("http://bioconductor.org/biocLite.R")

packages<-c("dplyr", "data.table", "dtplyr", "stringr", "curl", "jsonlite", "shiny", "rsconnect", "devtools", "ggplot2", "reshape2", "plotly" ,"RColorBrewer","bit64")

biocLite(packages)

devtools::install_github("rstudio/shinyapps")

EOF
