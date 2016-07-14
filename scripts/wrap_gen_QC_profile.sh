#!/bin/bash

set -e

time_path=$1
output_folder=$2

time_file=`basename $time_path`
prefix=$(echo $time_file | sed "s/_[0-9]*.time//;s/.time//")
library_path=`dirname $time_path` 
general_path=`dirname $library_path`
general_prefix=`basename $general_path`

output=$output_folder/$general_prefix/$prefix #assuming that you have a link named "storage" from your home (~) directory

#echo "time_path" $time_path
#echo "time_file" $time_file
#echo "library_path" $library_path
echo "prefix" $prefix
#echo "general_path" $general_path
#echo "general_prefix" $general_prefix
#echo "output" $output

if [ ! -d $output_folder/$general_prefix ]; then 
	mkdir -p $output_folder/$general_prefix
fi

touch $output"_profile"
 
generate_QC_profile.R $library_path $prefix $output"_profile"

exit
