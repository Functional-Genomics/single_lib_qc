#!/bin/bash

set -e

info_path=$1
output_folder=$2

info_file=`basename $info_path`
prefix=$(echo $info_file | sed "s/_[0-9]*\..*\.info//;s/\..*\.info//")
library_path=`dirname $info_path`
general_path=`dirname $library_path`
prefix_letters=`echo $prefix | sed "s/[0-9].*//"`
general_prefix=`echo $general_path | sed "s/.*$prefix_letters/$prefix_letters/"`

output=$output_folder/$general_prefix/$prefix

#echo "info_path" $info_path
#echo "info_file" $info_file
#echo "library_path" $library_path
echo $prefix "profile generated"
#echo "general_path" $general_path
#echo "general_prefix" $general_prefix
#echo "output" $output

if [ ! -d $output_folder/$general_prefix ]; then
	mkdir -p $output_folder/$general_prefix
fi

#touch $output"_profile"

generate_QC_profile.R $library_path $prefix $output"_profile"

exit
