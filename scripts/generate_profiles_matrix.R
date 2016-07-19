#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)

profiles_paths <- args[1] # file, list of paths to profiles
output <- args[2] #file, where to write the profiles matrix/dataframe

library(dtplyr) # seams dplyr and data.table
suppressPackageStartupMessages(library(data.table))
suppressPackageStartupMessages(library(dplyr))

if (length(args)!=2) {
  cat("usage:  <path_to_list_of_profiles> <where_to_write_the_matrix> \n")
  cat("ERROR: incorrect amount of arguments\n");
  q(status=1);
}

# used functions----------------------------------------------------------------------------------------------------

# generates matrix of all profiles, which paths are passed as a list
GenerateProfilesMatrix <- function (profiles_paths) {
  
  paths_list <- fread(profiles_paths, header = F, col.names = "Path")
  
  profiles_dataframe <- data.table()
  
  for (path in paths_list$Path) {
    profile <- fread (path)
    profiles_dataframe <- bind_rows(profiles_dataframe, profile)
  } 
  
  colnames(profiles_dataframe)[1] <- "Prefix"
  
  return (profiles_dataframe)
  
}

# executable code----------------------------------------------------------------------------------------------------

profiles_matrix <- GenerateProfilesMatrix (profiles_paths)
rownames(profiles_matrix) <- profiles_matrix$Prefix #assigning prefixes as row names
profiles_matrix[, Prefix:=NULL] #dropping "prefix" column
write.table (profiles_matrix, file = output, sep ="\t", row.names = T, col.names = NA)

q(status=0)