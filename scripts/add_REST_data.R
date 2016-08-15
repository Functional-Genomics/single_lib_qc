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
    
    add_row <- fromJSON(paste0("http://www.ebi.ac.uk/fg/rnaseq/api/json/0/getRun/", prefix))
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
  
  matrix <- bind_cols(matrix, add_table)
  
  # adding data about kindgoms 
  organisms <- unique(matrix$ORGANISM)
  genuses <- sub("_.*", "", unique(matrix$ORGANISM))
  kingdoms <- vector()
  KINGDOM <- vector()
  
  for (genus in genuses) {
    data <- fromJSON(paste0("https://rest.ensembl.org/taxonomy/classification/", genus, "?"))
    kingdoms <- c(kingdoms, data$scientific_name[length(data$scientific_name) - 1])                
  }
  
  mapping <- data.table(organisms, kingdoms)
  setkey(mapping, organisms)
  
  
  for (species in matrix$ORGANISM) {
    kingdom <- mapping[species,,]$kingdoms
    KINGDOM <- c(KINGDOM, kingdom)
  }
  
  matrix$KINGDOM <- KINGDOM
  
  R_path <- Sys.getenv("QC_R_DIR")
  columns_to_keep <- fread(paste0(R_path, "/columns_to_keep"), header = F, col.names = "Columns")
  matrix <- matrix[, columns_to_keep$Columns, with = F]
  
  return(matrix)
}

# executable code----------------------------------------------------------------------------------------------------

matrix <- fread(path_to_matrix, na.strings = c("","NA")) #reading matrix and filling blank spaces with NA
colnames(matrix)[1] <- "Prefix"
extended_matrix <- AppendRESTData (matrix)
write.table(extended_matrix, file = output_matrix, sep ="\t", row.names = F, col.names = T, quote = F)
write.table(missing_data, file = output_missing, quote = F, row.names = F, col.names = F)

