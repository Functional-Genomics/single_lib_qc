#!/bin/bash
# Visualize and inspect the qc profiles in a
# shiny App
set -e

if [ "$1" == "-h" ]; then
    cat <<EOF
USAGE: qc_profile_vis.sh [-i path to the qc_profiles file] [-c config_file] [-f filter]
EOF
    exit 0
fi

INPUT_FILE=
CONFIG_FILE=none
FILTER=none
IP=127.0.0.1
while getopts "i:c:f:s:"  Option
do
   case $Option in
    i ) INPUT_FILE=$OPTARG;;
    c ) CONFIG_FILE=$OPTARG;;
    s ) IP=$OPTARG;;
    f ) FILTER=$OPTARG;;
   esac
done

# Rscript is needed
which Rscript >> /dev/null || ( echo "ERROR: Rscript not found" && exit 1 )
# setup.sh should have been sourced...
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
if [ "$INPUT_FILE-" == "-" ]; then
   path_to_matrix=$QC_R_DIR/../profiles_data/extended_matrix
else
   path_to_matrix=`readlink -f $INPUT_FILE`
fi
Rscript -e "library(methods); shiny::runApp('$path_to_shiny', launch.browser=TRUE, host='$IP')" $path_to_matrix $CONFIG_FILE $FILTER
exit 0
