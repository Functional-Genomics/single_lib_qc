#!/bin/bash

set -e

path_to_shiny=`pwd`/ShinyApp
path_to_matrix=`pwd`/profiles_data/extended_matrix
Rscript -e "library(methods); shiny::runApp('$path_to_shiny', launch.browser=TRUE)" $path_to_matrix

exit
