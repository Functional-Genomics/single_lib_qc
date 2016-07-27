#!/bin/bash

set -e

library_path=$1 # path to the library
profiles_storage=$2 # where to put profiles

find $library_path -name "*.info" -exec wrap_gen_QC_profile.sh {} $profiles_storage/profiles \;

exit
