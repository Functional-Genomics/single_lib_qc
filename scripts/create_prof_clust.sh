#!/bin/bash

set -e

libraries_storage=$1 # where to look for directories with libraries
profiles_storage=$2 # where to put profiles
pattern=$3 # what directories to look for

L=`ls -d $libraries_storage/$pattern/`

for dir in $L
do
  bsub -o $profiles_storage/stdoutput -e $profiles_storage/stderror gen_QC_profile_single.sh $dir $profiles_storage
done

exit
