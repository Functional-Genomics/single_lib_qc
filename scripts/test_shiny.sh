#!/bin/bash

set -e

if [ "$#" -ne 2 ]; then
  echo "ERROR: Incorrect amount of arguments!"
  echo "Usage: <path_to_ShinyApp> <path_to_extended_matrix>"
  exit 1
fi

path_to_shiny=$1
path_to_matrix=$2
Rscript -e "library(methods); shiny::runApp('$path_to_shiny', launch.browser=TRUE)" $path_to_matrix

exit
