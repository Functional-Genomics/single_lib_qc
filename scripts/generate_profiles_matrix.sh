#!/bin/bash

set -e

top_folder=$1 #directory, where to look for libraries
output_folder=$2 #directory, where to output the matrixes
temporary_storage=$3 #directory, where to store all the temporary data

echo "Creating profiles of each library..."
if [ ! -d $temporary_storage/profiles ]; then 
	mkdir -p $output_folder/$general_prefix
fi
find $top_folder -name "*.info" -exec wrap_gen_QC_profile.sh {} $temporary_storage/profiles \;

echo "Creating list with profiles paths..."
touch $temporary_storage/profiles_paths
find_profiles_paths.sh $temporary_storage/profiles $temporary_storage/profiles_paths

echo "Generating the matrix..."
touch $output_folder/profiles_matrix
generate_profiles_matrix.R $temporary_storage/profiles_paths $output_folder/profiles_matrix

echo "Appending data from REST API..."
append_REST_data.sh $output_folder/profiles_matrix $output_folder

echo "Done"

exit

