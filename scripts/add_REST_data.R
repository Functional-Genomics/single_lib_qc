#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)

path_to_matrix <- args[1] #file, profiles matrix
output_matrix <- args[2] #file, where to write the extended matrix
output_missing <- args[3] #file, where to write missing prefixes

library(dtplyr) # seams dplyr and data.table
suppressPackageStartupMessages(library(data.table))
suppressPackageStartupMessages(library(dplyr))
library(curl)
library(jsonlite)

if (length(args)!=3) {
  cat("usage:  <path_to_profiles_matrix> <where_to_write_the_new_matrix> <where_to_write_missing_prefixes> \n")
  cat("ERROR: incorrect amount of arguments\n");
  q(status=1);
}

missing_data <- vector()

# used functions----------------------------------------------------------------------------------------------------

AppendRESTData <- function (matrix) {
  
  prefix_list <- matrix$Prefix
  add_table <- data.table()
  counter <- 1
  
  for (prefix in prefix_list) {
    
    add_row <- fromJSON(paste0("http://www.ebi.ac.uk/fg/rnaseq/api/json/70/getRun/", prefix))
    if (length(add_row) == 0) {
      add_row <- data.table(STUDY_ID = NA,
                            ORGANISM = NA,
                            REFERENCE_ORGANISM = NA,
                            MAPPING_QUALITY = NA)
      missing_data[counter] <<- prefix
      counter <- counter + 1
      
    } else {
      add_row <- add_row[, c("STUDY_ID", "ORGANISM", "REFERENCE_ORGANISM", "MAPPING_QUALITY")]
    }
    add_table <- bind_rows(add_table, add_row)
  }
  
  full_matrix <- bind_cols(matrix, add_table) 
  return(full_matrix)
  
}

# executable code----------------------------------------------------------------------------------------------------

matrix <- fread(path_to_matrix, na.strings = c("","NA")) #reading matrix and filling blank spaces with NA
colnames(matrix)[1] <- "Prefix"
extended_matrix <- AppendRESTData (matrix)
write.table(extended_matrix, file = output_matrix, sep ="\t", row.names = T, col.names = NA)
write.table(missing_data, file = output_missing, quote = F, row.names = F, col.names = F)

