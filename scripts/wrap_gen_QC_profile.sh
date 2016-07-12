#!/bin/bash

set -e

time_path=$1

time_file=`basename $time_path`
prefix=$(echo $time_file | sed "s/_[0-9]*.time//;s/.time//")
library_path=`dirname $time_path` 
general_path=`dirname $library_path`
general_prefix=`basename $general_path`

output=~/storage/profiles/$general_prefix/$prefix #supposing that you have a link named "storage" from your home (~) directory

#echo "time_path" $time_path
#echo "time_file" $time_file
#echo "library_path" $library_path
echo "prefix" $prefix
#echo "general_path" $general_path
#echo "general_prefix" $general_prefix
#echo "output" $output

if [ ! -d ~/storage/profiles/$general_prefix ]; then 
	mkdir -p ~/storage/profiles/$general_prefix
fi

touch $output
 
generate_QC_profile.R $library_path $prefix $output

exit
