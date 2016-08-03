#!/bin/bash

set -e

info_path=$1
output_folder=$2

if [[ -z "$1" ]] || [[ -z "$2" ]]; then
	echo "info_path or output_folder are empty"
	exit 1
fi
echo "info_path is" $1
echo "output_folder is" $2

info_file=`basename $info_path`
if [[ -z "$info_file" ]]; then echo "info_file is empty"; exit 1; fi
echo "info_file is" $info_file

prefix=$(echo $info_file | sed "s/_[0-9]*\..*\.info//;s/\..*\.info//")
if [[ -z "$prefix" ]]; then echo "prefix is empty"; exit 1; fi
echo "prefix is" $prefix

library_path=`dirname $info_path`
echo "library_path is" $library_path

general_path=`dirname $library_path`
echo "general_path is" $general_path

prefix_letters=`echo $prefix | sed "s/[0-9].*//"`
if [[ -z "$prefix_letters" ]]; then echo "prefix_letters is empty"; exit 1; fi

general_prefix=`echo $general_path | sed "s/.*$prefix_letters/$prefix_letters/"`
echo "general prefix is" $general_prefix

output=$output_folder/$general_prefix/$prefix

#echo "info_path" $info_path
#echo "info_file" $info_file
#echo "library_path" $library_path
#echo "general_path" $general_path
#echo "general_prefix" $general_prefix
#echo "output" $output

if [ ! -d $output_folder/$general_prefix ]; then
	mkdir -p $output_folder/$general_prefix
fi

#> $output"_profile"

generate_QC_profile.R $library_path $prefix $output"_profile"

echo $prefix "profile generated"
echo "output is in" $output_folder/$general_prefix
echo

exit
