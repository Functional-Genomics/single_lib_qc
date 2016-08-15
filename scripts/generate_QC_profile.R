#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)

path_to_directory <- args[1] # path to folder (e.g. /.../SRR869/SRR869012/)
prefix <- args[2] # prefix of the library's files (e.g. SRR869012)
output <- args[3] # where to place the output (used in write.table)

library(dtplyr) # seams dplyr and data.table
suppressPackageStartupMessages(library(data.table))
suppressPackageStartupMessages(library(dplyr))
library(stringr)

if (length(args)!=3) {
  cat("usage:  <path_to_folder> <filename_prefix> <output_file> \n")
  cat("ERROR: incorrect amount of arguments\n");
  q(status=1);
}

# used functions----------------------------------------------------------------------------------------------------

# MAIN FUNCTION-----------------------------------------------------------------------------------------------------

# generates QC profile of the given library (takes path to the directory and the library's prefix of character types)
GenerateQCProfile <- function (path_to_directory, prefix) {
  
  # list with the mandatory types of files which contain profiling data 
  types_of_files <- list(info = paste0(prefix, ".*.info$"),
                         fastqc.tsv_1 = paste0(prefix, "(_1\\..*\\.fastqc.tsv$)|), prefix, (\\..*\\.fastqc.tsv$)"),
                         fastqc.tsv_2 = paste0(prefix, "_2\\..*\\.fastqc.tsv$"),
                         stats = paste0(prefix, ".*.bam.stats$"),
                         gene.stats = paste0(prefix, ".*.gene.stats$"),
                         stats.csv = paste0(prefix, ".*.stats.csv$"),
                         genes.raw.csv = paste0(prefix, ".*.genes.raw.*.tsv$"),
                         time = paste0(prefix,".*.time$")) 
  
  files_paths <- list()
  
  # looking for files with the necessary data and making lists with the full paths to each file
  for (type in types_of_files) {
    temporary_list <- list.files(path_to_directory, pattern = type, full.names = T)
    files_paths <- c(files_paths, temporary_list)
  } 
  
  # creating compelete list with files paths
  files_paths <- unlist(files_paths)
  files_paths <- unique(files_paths) #getting rid of the duplicates (check for another way?)
  
  # checking if the .fastqc.tsv files contain any info
  for (file in files_paths) {
    if (file.info(file)$size <= 1 || is.na(file.info(file)$size) == T) 
      files_paths <- files_paths[-grep(paste0("^", file, "$"), files_paths)]
  }
  
  # what to do if the .fastqc.tsv file is present
  if (length(grep("fastqc.tsv", files_paths)) == 0) { 
    amount_of_files <- (length(types_of_files) - 2)
  } else if (length(grep("fastqc.tsv", files_paths)) == 1) { 
    amount_of_files <- (length(types_of_files) - 1) 
  } else {
    amount_of_files <- length(types_of_files) 
  }
  
  # checks for the library's integrity (some files are essential)
  if (length(files_paths) != amount_of_files) {
    cat(prefix, " ", "ERROR! incomplete library: \n")
    types_of_files["fastqc.tsv_1"] <- NULL #leaving only essential files
    types_of_files["fastqc.tsv_2"] <- NULL #leaving only essential files
    for (type_name in names(types_of_files)) {
      file <- unlist(types_of_files[type_name])
      if (length(grep(paste0(file, "($|\\s)"), files_paths)) == 0) {
        cat(type_name, "file missing/empty", "\n")
      }
    }
    cat("\n")
    q(status = 1)
  }
  
  # reading data from the files as a list of dataframes 
  reads_from_info <-
    read.table(files_paths[grep("info", files_paths)], comment.char = " ")
  reads_from_stats <-
    lapply(files_paths[-c(grep("info", files_paths), grep("time", files_paths))], fread, header = F)
  # reading from the .time file
  time_path <- files_paths[grep("time", files_paths)]
  ncols <- max(count.fields(time_path))
  reads_from_time <- read.table(time_path, header = F, 
                                col.names = paste0("V", 1:ncols), fill = T)[1:4]
  
  # converting time/memory dataframe to a 2-column one, leaving memory (.time dataframe)
  reads_from_time <- ConvertTimeList (reads_from_time)
  # addind time df to stats df
  reads_from_stats <- append(reads_from_stats, list(reads_from_time))
  
  # summing up the data from .genes.raw.tsv
  genes_raw_pos <- grep("genes.raw", files_paths) - 1 #position of the genes.raw data in the reads_from_stats dataframe
  reads_from_stats[[genes_raw_pos]]$V2 <- 
    sum(reads_from_stats[[genes_raw_pos]]$V2)
  reads_from_stats[[genes_raw_pos]]$V1 <- "GENES.RAW.TSV_sum"
  reads_from_stats[[genes_raw_pos]] <- 
    unique(reads_from_stats[[genes_raw_pos]])
  
  # getting the nreads and rs data from the info 
  info_dataframe <- GetInfoData(reads_from_info)
  info_dataframe$V1 <- paste0("INFO_", info_dataframe$V1) # adding INFO_ prefix to the row names
  
  # adding prefixes
  reads_from_stats[[grep("Exons|Introns", reads_from_stats)]]$V1 <-
    paste0("GENE.STATS_",  reads_from_stats[[grep("Exons|Introns", reads_from_stats)]]$V1)
  reads_from_stats[[grep("source", reads_from_stats)]]$V1 <-
    paste0("STATS_",  reads_from_stats[[grep("source", reads_from_stats)]]$V1)
  reads_from_stats[[grep("entries", reads_from_stats)]]$V1 <-
    paste0("STATS.CSV_",  reads_from_stats[[grep("entries", reads_from_stats)]]$V1)
  reads_from_stats[[grep("iRAP", reads_from_stats)]]$V1 <-
    paste0("TIME_",  reads_from_stats[[grep("iRAP", reads_from_stats)]]$V1)
  # if the data from .fastqc.tsv is present, add prefixes to it, too
  if (length(grep("fastqc.tsv", files_paths)) != 0) {
    for (position in grep("FASTQC", reads_from_stats)) {
      appendix <- paste0("fastqc.tsv_", position,"_")
      reads_from_stats[[position]]$V1 <-
        paste0(appendix,  reads_from_stats[[position]]$V1)
    }
  }
  
  # deleting duplicates from part with time and memory values + adding some new values
  reads_from_stats[[grep("iRAP", reads_from_stats)]] <-
    DeleteTMDuplicates(reads_from_stats[[grep("iRAP", reads_from_stats)]])
  
  # converting list of the dataframes to a single dataframe
  stats_dataframe <- rbindlist(reads_from_stats)
  
  # creating profile vector
  profile_vector <- bind_rows(info_dataframe, stats_dataframe)$V2
  names(profile_vector) <- bind_rows(info_dataframe, stats_dataframe)$V1
  
  # creating profile dataframe
  profile_dataframe <- transform(profile_vector, as.numeric())
  colnames(profile_dataframe) <- prefix
  
  # transposing profile dataframe
  transposed_profile_dataframe <- TransposeWithNames(profile_dataframe)
  
  # cleaning dataframe (dropping non-important columns, if there are such)
  transposed_profile_dataframe <- DropUnnecessaryColumns(transposed_profile_dataframe)
  
  return (transposed_profile_dataframe)
  
}

# SECONDARY FUNCTIONS-----------------------------------------------------------------------------------

# gets rs and nreads from the list with data from .info, returns a dataframe with columns V1 and V2
GetInfoData <- function (reads_from_info) {
  
  info_data <- as.vector(reads_from_info[, 1])
  info_vect <- vector()
  
  features_list <- c("#rs", "#nreads", "strand")
  for (feature in features_list)
    info_vect <- c(info_vect, grep(feature, info_data, value = T))
  info_vect <- unique(info_vect)
  
  info_vect <- unlist(as.data.table(strsplit(info_vect, "="))[2])
  if (length(grep("se", info_data)) != 0) {
    info_vect <- c(info_vect, "single-end")
  } else if (length(grep("pe", info_data)) != 0){
    info_vect <- c(info_vect, "paired-end")
  } else {
    info_vect <- c(info_vect, "NA")
  }
  
  info_df <- data.table(V1 = c("rs", "nreads", "strand", "end_type"), V2 = info_vect)
  
  return(info_df)
}

# converts the "time" list of the reads_from_stats to a 2-column dataframe (time+memory)
ConvertTimeList <- function (reads_from_time) {
  
  names <- reads_from_time$V1
  
  col_memory_names <- paste0(names, "_memory")
  col_memory_values <- reads_from_time$V4
  
  memory_dataframe <- data.frame (col_memory_names, col_memory_values)
  colnames(memory_dataframe) <- c("V1", "V2")
  
  reads_from_time <- subset(reads_from_time, select = c(V1,V3))
  colnames(reads_from_time) <- c("V1", "V2")
  
  memory_dataframe$V2 <- as.double(memory_dataframe$V2)
  reads_from_time <- rbind(reads_from_time, memory_dataframe)
  reads_from_time$V1 <- as.character(reads_from_time$V1)
  
  return(reads_from_time)
  
}

# deletes duplicates in the time and memory list and adds "sum" column
DeleteTMDuplicates <- function (df) {
  var_names <- unique(df$V1)
  new_df <- data_frame()
  for (name in var_names) {
    
    part <- subset(df, df$V1 == name)
    
    last <- part[length(part$V2), ]
    last$V1 <- paste0(last$V1, "_last")
    
    max <- part[which(part$V2 == max(part$V2), arr.ind = T), ]
    max$V1 <- paste0(max$V1, "_max")
    
    summ <- aggregate(V2~V1, data = part, FUN = sum) 
    summ$V1 <- paste0(summ$V1, "_sum")
    
    new_df <- bind_rows(new_df, last, max, summ)
  
  }
  # deleting _memory_sum, as its meaningless
  new_df <- new_df[-grep("_memory_sum", new_df$V1), ]
  new_df <- new_df[!duplicated(new_df), ]
  
  #adding summary of all the last times
  temp_df <- new_df[grep("TIME_.*_last", new_df$V1), ]
  temp_df <- temp_df[-grep("memory", temp_df$V1), ]
  temp_df <- data.table(V1 = "TIME_sum", V2 = sum(temp_df$V2))
  
  return(bind_rows(new_df, temp_df))
}

# transposes a dataframe while keeping the names and the variables types
TransposeWithNames <- function (data.frame) {
  rows <- rownames (data.frame)
  cols <- colnames (data.frame)
  
  transposed_data_frame <- transpose(data.frame)
  rownames(transposed_data_frame) <- cols
  colnames(transposed_data_frame) <- rows
  
  return(transposed_data_frame)
}

# deletes unnecessary columns
DropUnnecessaryColumns <- function (data_frame) {
  
  unnecessary_columns_list <- list ("source", "IG", "TR", "3prime_overlapping_ncrna", "insdc", 
                                    "macro_lncRNA", "mirbase", "misc_RNA", "non_stop_decay", "nonsense_mediated_decay",
                                    "polymorphic_pseudogene", "ribozyme", "sense_intronic", "sense_overlapping",
                                    "Mt_rRNA", "Mt_tRNA", "transcribed_unitary_pseudogene", "vaultRNA", "FlyBase", 
                                    "pre_miRNA", "WormBase", "FASTQC")
  
  for (name in unnecessary_columns_list) {
    if (length(grep(name, colnames(data_frame))) != 0)
      data_frame <-
        data_frame[, -grep(name, colnames(data_frame))] #source column
  }
  
  return (data_frame)
  
}
#--------------------------------------------------------------------------------------------------------

# Executable code-----------------------------------------------------------------------------------------

profile <- GenerateQCProfile(path_to_directory, prefix)
# writes the output to a file
write.table (profile, file = output, sep ="\t", row.names = T, col.names = NA)

q(status=0)
