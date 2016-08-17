#!/bin/bash

set -e

path_to_shiny=`pwd`/ShinyApp_Plotly
if [ ! -e $path_to_shiny ]; then
    echo "ERROR: unable to find $path_to_shiny"
    exit 1
fi
if [ "$1-" == "-" ]; then
    path_to_matrix=`pwd`/profiles_data/extended_matrix
else
    path_to_matrix=$1
fi

which Rscript >> /dev/null || ( echo "ERROR: Rscript not found" && exit 1 )

Rscript -e "library(methods); shiny::runApp('$path_to_shiny', launch.browser=TRUE)" $path_to_matrix

exit
