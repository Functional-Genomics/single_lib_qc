#!/bin/bash
# Visualize and inspect the qc profiles in a
# shiny App
set -e

if [ "$1" == "-h" ]; then
    cat <<EOF
USAGE: qc_profile_vis.sh <path to the qc_profiles file> [config_file]
EOF
    exit 0
fi

# Rscript is needed
which Rscript >> /dev/null || ( echo "ERROR: Rscript not found" && exit 1 )

# setup.sh should have bee sourced...
if [ "$QC_R_DIR-" == "-" ]; then
    echo "ERROR: QC_R_DIR variable not defined. Did you source the setup.sh file in the toplevel folder?"
    exit 1
fi

# 
path_to_shiny=$QC_R_DIR/../ShinyApp_Plotly
if [ ! -e $path_to_shiny ]; then
    echo "ERROR: unable to find $path_to_shiny"
    exit 1
fi
if [ "$1-" == "-" ]; then
    path_to_matrix=$QC_R_DIR/../profiles_data/extended_matrix
else
    path_to_matrix=`readlink -f $1`
fi

Rscript -e "library(methods); shiny::runApp('$path_to_shiny', launch.browser=TRUE)" $path_to_matrix $2

exit 0
