#!/bin/sh
#; -*- mode: Makefile;-*-
#the next line restarts using\
exec make --warn-undefined-variables -Rf "$0"  ${1+"$@"} 


ifndef info_path
$(error missing parameter: info_path)
endif

file_exists=$(if  $(realpath $(1)),,$(error  $(1) not found))

$(call file_exists,$(info_path))

$(info * info_path=$(info_path))

# by default it will use the folder where the info path is
ifndef output_folder
output_folder=$(dir $(info_path))
else
$(call mkdir -p $(output_folder))
endif
$(info * output_folder=$(output_folder))

# optional parameter
#
db_file?=



prefix=$(shell echo $(notdir $(info_path))|sed "s/.fastq.*info$$//")
$(info * prefix=$(prefix))


#
# prefix
lib=$(shell echo $(prefix)| sed "s/_[12]$$//")
$(info * lib=$(lib))


library_path=$(dir $(info_path))
$(info * library_path=$(library_path))


output=$(output_folder)/$(prefix).qc_profile.tsv

# TODOL update
files_used_by_QC_profile=$(info_path) $(library_path)/$(lib).data_info.tsv $(library_path)/$(lib).f.fastqc.tsv $(library_path)
$(output): $(files_used_by_QC_profile)
	generate_QC_profile.R $(library_path) $(prefix) $@.tmp && mv $@.tmp $@ &&\
	echo $prefix "profile generated"
