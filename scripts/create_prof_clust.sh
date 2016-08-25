#!/bin/bash

set -e

libraries_storage=$1 # where to look for directories with libraries
profiles_storage=$2 # where to put profiles
pattern=$3 # what directories to look for
path_to_runs_file=$4 #OPTIONAL; file, wehre to look for additional data

if [ -z "$3" ]
then
  L=`ls --color=never -d $libraries_storage/`
else
  L=`ls --color=never -d $libraries_storage/$pattern/`
fi

for dir in $L
do
  bsub -o $profiles_storage/stdoutput -e $profiles_storage/stderror gen_QC_profile_single.sh $dir $profiles_storage $path_to_runs_file
done

exit
