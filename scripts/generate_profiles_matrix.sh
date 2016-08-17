#!/bin/bash

set -e

top_folder=$1 #directory, where to look for libraries
output_folder=$2 #directory, where to output the matrixes (with and without REST-API data)
temporary_storage=$3 #directory, where to store all the temporary data

if [ ! -d $output_folder ]; then
	mkdir -p $output_folder
fi

if [ ! -d $temporary_storage ]; then
	mkdir -p $temporary_storage
fi

echo "Creating profiles of each library..."
if [ ! -d $temporary_storage/profiles ]; then
	mkdir -p $temporary_storage/profiles
fi
find $top_folder -name "*.info" -exec wrap_gen_QC_profile.sh {} $temporary_storage/profiles \;

if [[ $? != 0 ]]; then exit; fi #checking if previous script was executed ok

echo "Creating list with profiles paths..."
> $temporary_storage/profiles_paths
find_profiles_paths.sh $temporary_storage/profiles $temporary_storage/profiles_paths

echo "Generating the matrix..."
> $output_folder/profiles_matrix
generate_profiles_matrix.R $temporary_storage/profiles_paths $output_folder/profiles_matrix

echo "Appending data from REST API..."
append_REST_data.sh $output_folder/profiles_matrix $output_folder

echo "Done"

exit
