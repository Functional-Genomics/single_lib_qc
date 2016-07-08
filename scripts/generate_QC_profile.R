#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)

path_to_directory <- args[1] #path to folder (e.g. /.../SRR869/SRR869012/)
index <- args[2] #prefix of the library's files (e.g. SRR869012)
output <- args[3] #where to place the output (used in write.table)

library(data.table)
library(dplyr)
library(stringr)

if (length(args)!=3) {
  cat("usage:  <path_to_folder> <filename_prefix> <output_file> \n")
  cat("ERROR: incorrect amount of arguments\n");
  q(status=1);
}



###########################################################################
#used functions#

#MAIN FUNCTION#

#generates QC profile of the given library (takes path to the directory and the library's index of character types)
GenerateQCProfile <- function (path_to_directory, index) {
  
  #list with the types of files which may contain profiling data 
  types_of_files <- list(info = paste0(index, ".*.info"),
                         stats = paste0(index, ".*.bam.stats"),
                         gene.stats = paste0(index, ".*.gene.stats"),
                         stats.csv = paste0(index, ".*.stats.csv"), 
                         time = paste0(index,".*.time")) 
  
  files_paths_list <- list()
  files_list <- list()
  
  #looking for files with the necessary data and making lists with the full paths to each file
  for (type in types_of_files) {
    temporary_list <- list.files(path_to_directory, pattern = type)
    files_list <- c(files_list, temporary_list)
  } 
  
  #creating compelete list with files paths
  files_paths_list <- paste0(path_to_directory, sep = "/", files_list)
  files_paths_list <- unique(files_paths_list) #getting rid of the duplicates (check for another way?)
  
  #IMPORTANT: checks for the library's integrity (there should be only 5 files)
  if (length(files_paths_list)!=5){
    cat(index, " ", "ERROR! incomplete library:")
    for (type in types_of_files) {
      if (length((grep(paste0(type, "($|\\s)"), files_paths_list))) == 0) {
        cat(" ", type, "file missing")
      }
    }
    cat("\n")
    q(status=1)
  }
  
  
  #reading data from the file as a list of dataframes (specific, as there are always only 5 files that we need to check)
  reads_from_info <- read.table(files_paths_list[grep("info", files_paths_list)], comment.char = " ")
  reads_from_stats <- lapply(files_paths_list[-grep("info", files_paths_list)], fread, header=FALSE)
  
  #getting the nreads and rs data from the info 
  info_dataframe <- GetRsNreads (reads_from_info)
  info_dataframe$V1 <- paste0("INFO_", info_dataframe$V1) #adding INFO_ prefix to the row names
  
  #cleaning dataframes
  reads_from_stats[[grep("iRAP", reads_from_stats)]] <- convert_time_list (reads_from_stats) #converting time/memory dataframe to a 2-column one, leaving memory (.time dataframe)
  
  #adding prefixes
  reads_from_stats[[grep("Exons|Introns", reads_from_stats)]]$V1 <- paste0("GENE.STATS_",  reads_from_stats[[grep("Exons|Introns", reads_from_stats)]]$V1)
  reads_from_stats[[grep("gene", reads_from_stats)]]$V1 <- paste0("STATS_",  reads_from_stats[[grep("gene", reads_from_stats)]]$V1)
  reads_from_stats[[grep("entries", reads_from_stats)]]$V1 <- paste0("STATS.CSV_",  reads_from_stats[[grep("entries", reads_from_stats)]]$V1)
  reads_from_stats[[grep("iRAP", reads_from_stats)]]$V1 <- paste0("TIME_",  reads_from_stats[[grep("iRAP", reads_from_stats)]]$V1)
  
  #converting list of the dataframes to a single dataframe
  stats_dataframe <- rbindlist(reads_from_stats)
  
  #deleting underscores (just in case)
  #stats_dataframe$V1 <- gsub('_', ' ', stats_dataframe$V1)
  
  #creating profile vector
  profile_vector <- bind_rows(info_dataframe, stats_dataframe)$V2
  names(profile_vector) <- bind_rows(info_dataframe, stats_dataframe)$V1
  
  #creating profile dataframe
  profile_dataframe<- transform(profile_vector, as.numeric())
  colnames(profile_dataframe) <- index
  
  #transposing profile dataframe
  transposed_profile_dataframe <- TransposeWithNames(profile_dataframe)
  
  #cleaning dataframe (dropping non-important columns, if there are such)
  transposed_profile_dataframe <- DropUnnecessaryColumns(transposed_profile_dataframe)
  
  return (transposed_profile_dataframe)
  
}

#SECONDARY FUNCTIONS#

#gets rs and nreads from the list with data from .info, returns a dataframe with columns V1 and V2
GetRsNreads <- function (reads_from_info) {
  
  sample <- str_split_fixed(reads_from_info$V1, "#", n=2)
  nreads <- strsplit(sample[grep("nreads", sample)], "=")
  rs <- strsplit(sample[grep("rs", sample)], "=")[[2]] #!TOO SPECIFIC CODE/REDO
  rs_nreads_list <- c(nreads, rs)
  
  rs_nreads_dataframe <- data.frame(matrix(unlist(rs_nreads_list), nrow=2, byrow=T),stringsAsFactors=FALSE)
  colnames(rs_nreads_dataframe) = c("V1", "V2")
  
  return(rs_nreads_dataframe)
}

#converts the "time" list of the reads_from_stats to a 2-column dataframe (time+memory)
convert_time_list <- function (reads_from_stats) {
  
  col_memory_names <-paste0(c("fastqInfo","iRAP-QC","iRAP-Mapping","iRAP-Quant","iRAP-Mapping-QC","iRAP-CRAM"), "_memory")
  col_memory_values <- reads_from_stats[[grep("iRAP", reads_from_stats)]]$V4
  memory_dataframe <- data.frame (col_memory_names, col_memory_values)
  colnames(memory_dataframe) <- c("V1", "V2")
  
  reads_from_stats[[grep("iRAP", reads_from_stats)]] <- subset(reads_from_stats[[4]], select = c(V1,V3))
  colnames(reads_from_stats[[grep("iRAP", reads_from_stats)]]) <- c("V1", "V2")
  
  return(bind_rows(reads_from_stats[[grep("iRAP", reads_from_stats)]], memory_dataframe))
  
}

#transposes a dataframe while keeping the names and the variables types
TransposeWithNames <- function (data.frame) {
  rows <- rownames (data.frame)
  cols <- colnames (data.frame)
  
  transposed_data_frame <- transpose(data.frame)
  rownames(transposed_data_frame) <- cols
  colnames(transposed_data_frame) <- rows
  
  return(transposed_data_frame)
}

#deletes unnecessary columns
DropUnnecessaryColumns <- function (data_frame) {
  
  unnecessary_columns_list <- list ("source", "IG", "TR")
  
  for (name in unnecessary_columns_list) {
    if ( length(grep(name, colnames(data_frame))) != 0 ) 
      data_frame <- data_frame[,-grep(name, colnames(data_frame))] #source column
  }
  
  return (data_frame)
  
}
###########################################################################

profile <- GenerateQCProfile(path_to_directory, index)
#write the output to a file
write.table (cbind(rownames(profile), profile), file = output, sep ="\t", row.names = T, col.names = T)

q(status=0)
