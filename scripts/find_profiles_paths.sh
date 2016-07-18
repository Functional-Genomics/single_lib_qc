#!/bin/bash

set -e

path_to_profiles=$1
output=$2

find $path_to_profiles -name "*_profile" > $output

exit  
