#!/bin/bash
# Script to install all software needed

R --no-save <<EOF
source("http://bioconductor.org/biocLite.R")

for (p in c("dplyr", "data.table", "dtplyr", "stringr") ) {
   biocLite(p)
}
EOF