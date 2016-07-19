#!/bin/bash

set -e

profiles_matrix=$1 #file, what matrix to use for extending
output=$2 #folder, where to output new matrix

touch $output/extended_matrix
touch $output/missing_REST_prefixes

add_REST_data.R $profiles_matrix $output/extended_matrix $output/missing_REST_prefixes

exit


