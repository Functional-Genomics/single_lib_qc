#!/bin/bash

set -e

library_path=$1 # path to the library
profiles_storage=$2 # where to put profiles
path_to_runs_file=$3

find $library_path -name "*.info" -exec wrap_gen_QC_profile_old.sh {} $profiles_storage/profiles $path_to_runs_file \;

exit
