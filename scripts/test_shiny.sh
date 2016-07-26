#!/bin/bash

set -e

path_to_matrix=`echo $PWD/profiles_data/extended_matrix`
Rscript -e 'library(methods); shiny::runApp("ShinyApp/", launch.browser=TRUE)' $path_to_matrix

exit
