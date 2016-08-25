#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)

path_to_runs <- NULL
path_to_directory <<- args[1] # path to folder (e.g. /.../SRR869/SRR869012/)
prefix <<- args[2] # prefix of the library's files (e.g. SRR869012)
output <<- args[3] # where to place the output (used in write.table)
if (length(args)>3)
  path_to_runs <<- args[4] # path to runs.tsv file



library(dtplyr) # seams dplyr and data.table
suppressPackageStartupMessages(library(data.table))
suppressPackageStartupMessages(library(dplyr))
library(stringr)

R_folder_path <<- Sys.getenv("QC_R_DIR")
columns_to_keep <- data.table()

if (length(args) < 3 || length(args) > 4) {
  cat("usage:  <path_to_folder> <filename_prefix> <output_file> optionally:<path_to_runs.tsv> \n")
  cat("ERROR: incorrect amount of arguments\n");
  q(status=1);
}

# checking if some data was already uploaded
columns_to_keep <<- fread(paste0(R_folder_path, "/columns_to_keep"), header = F, col.names = "Columns")

runs_df <- NULL
if (!is.null(path_to_runs) ) {
  cat("INFO: extra matrix provided",path_to_runs,"\n")
  runs_df <<- suppressWarnings(fread(path_to_runs))
  setkey(runs_df, Run)
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
                         time = paste0(prefix,".*.time$"),
                         irap.versions = paste0("irap.versions.tsv")) 
  
  files_paths <- list()
  
  # looking for files with the necessary data and making lists with the full paths to each file
  for (type in types_of_files) {
    temporary_list <- list.files(path_to_directory, pattern = type, full.names = T)
    files_paths <- c(files_paths, temporary_list)
  } 
  
  # creating compelete list with files paths
  files_paths <- unlist(files_paths)
  files_paths <- unique(files_paths) #getting rid of the duplicates (check for another way?)
  
  # checking if the files contain any info (or exist at all)
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
                                col.names = paste0("V", 1:ncols), fill = T)[1:5]
  
  # converting time/memory dataframe to a 2-column one, leaving memory (.time dataframe)
  reads_from_time <- GetTimeData(reads_from_time)
  # addind time df to stats df
  reads_from_stats <- append(reads_from_stats, list(reads_from_time, date_dataframe))
  
  # getting pipeline version from the data
  reads_from_stats[[grep("Pipeline", reads_from_stats)]] <-
    GetVersionData(reads_from_stats[[grep("Pipeline", reads_from_stats)]])
  
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
  reads_from_stats[[grep("memory", reads_from_stats)]]$V1 <-
    paste0("TIME_",  reads_from_stats[[grep("memory", reads_from_stats)]]$V1)
  reads_from_stats[[grep("Pipeline", reads_from_stats)]]$V1 <-
    paste0("VERSION_", reads_from_stats[[grep("Pipeline", reads_from_stats)]]$V1)
  reads_from_stats[[grep("date", reads_from_stats)]]$V1 <-
    paste0("TIME_",  reads_from_stats[[grep("date", reads_from_stats)]]$V1)
  # if the data from .fastqc.tsv is present, add prefixes to it, too
  if (length(grep("fastqc.tsv", files_paths)) != 0) {
    for (position in grep("FASTQC", reads_from_stats)) {
      appendix <- paste0("fastqc.tsv_", position,"_")
      reads_from_stats[[position]]$V1 <-
        paste0(appendix,  reads_from_stats[[position]]$V1)
    }
  }
  
  # deleting duplicates from part with time and memory values + adding some new values
  reads_from_stats[[grep("memory", reads_from_stats)]] <-
    CleanTimeDF(reads_from_stats[[grep("memory", reads_from_stats)]])
  reads_from_stats[[grep("date", reads_from_stats)]] <-
    CleanTimeDF(reads_from_stats[[grep("date", reads_from_stats)]])
  
  # converting every column to character
  reads_from_stats <- lapply(reads_from_stats, ConvertColsToChar)
  
  # converting list of the dataframes to a single dataframe
  stats_dataframe <- bind_rows(reads_from_stats)
  
  # creating profile vector
  profile_vector <- bind_rows(info_dataframe, stats_dataframe)$V2
  names(profile_vector) <- bind_rows(info_dataframe, stats_dataframe)$V1
  
  # creating profile dataframe
  profile_dataframe <- transform(profile_vector, as.numeric())
  
  # transposing profile dataframe
  transposed_profile_dataframe <- TransposeWithNames(profile_dataframe)
  transposed_profile_dataframe <- bind_cols(data.table(Prefix = prefix), transposed_profile_dataframe)
  
  # substituting blanks for underscores
  colnames(transposed_profile_dataframe) <- gsub(" |-", "_", colnames(transposed_profile_dataframe)) 
  
  # cleaning dataframe (dropping non-important columns, if there are such)
  keep <- intersect(columns_to_keep$Columns, colnames(transposed_profile_dataframe))
  transposed_profile_dataframe <- transposed_profile_dataframe[, keep, with = F]
  
  # adding more data to profile (species, study_id, etc.)
  extended_profile <- AddMoreData(transposed_profile_dataframe, path_to_directory)
  
  return (extended_profile)
  
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
  
  if (length(grep("strand", info_vect)) > 1) {
    cat("WARNING: strand from .info has", length(grep("strand", info_vect)), "values (" ,
        unlist(as.data.table(strsplit(grep("strand", info_vect, value = T), "="))[2]),
        ")", path_to_directory, "\tprefix:", prefix, "\n")
    strand_pos_last <- grep("strand", info_vect)[length(grep("strand", info_vect))]
    strand_last <- info_vect[strand_pos_last]
    info_vect <- info_vect[-grep("strand", info_vect)]
    info_vect <- c(info_vect, strand_last)
  }
  
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
GetTimeData <- function (reads_from_time) {
  
  names <- reads_from_time$V1
  
  col_memory_names <- paste0(names, "_memory")
  col_memory_values <- as.double(reads_from_time$V4)
  
  col_date_names <- paste0(names, "_date")
  col_date_values <- as.Date(reads_from_time$V5) 
  
  memory_dataframe <- data.frame(col_memory_names, col_memory_values)
  colnames(memory_dataframe) <- c("V1", "V2")
  memory_dataframe$V1 <- as.character(memory_dataframe$V1)
  
  date_dataframe <<- data.frame(col_date_names, col_date_values)
  colnames(date_dataframe) <<- c("V1", "V2")
  date_dataframe$V1 <<- as.character(date_dataframe$V1)
  
  time_dataframe <- subset(reads_from_time, select = c(V1,V3))
  colnames(time_dataframe) <- c("V1", "V2")
  time_dataframe$V1 <- as.character(time_dataframe$V1)
  
  reads_from_time <- bind_rows(time_dataframe, memory_dataframe)
  
  return(reads_from_time)
}

# gets pipeline version
GetVersionData <- function (df) {
  df[df$V1 == "Pipeline"]
  df <- subset(df, select = c("V1", "V3"))
  colnames(df) <- c("V1", "V2")
  return(df)
}

# cleans time/memory/date part of the df
CleanTimeDF <- function (df) {
  var_names <- unique(df$V1)
  new_df <- data_frame()
  for (name in var_names) {
    
    part <- subset(df, df$V1 == name)
    
    last <- part[length(part$V2), ]
    last$V1 <- paste0(last$V1, "_last")
    
    max <- part[which(part$V2 == max(part$V2), arr.ind = T), ]
    max$V1 <- paste0(max$V1, "_max")
    
    if (length(grep("_memory|_date", name)) == 0) {
      summ <- aggregate(V2~V1, data = part, FUN = sum) 
      summ$V1 <- paste0(summ$V1, "_sum")
      
      new_df <- bind_rows(new_df, last, max, summ)
    } else {
      # this is for processing the memory or the date parts (as we don't need sum of their last values)
      new_df <- bind_rows(new_df, last, max)
    }
    
  }
  
  #adding summary of all the last times
  if (length(grep("_date", new_df)) == 0) {
    temp_df <- new_df[grep("TIME_.*_last", new_df$V1), ]
    temp_df <- temp_df[-grep("memory", temp_df$V1), ]
    temp_df <- data.table(V1 = "TIME_sum", V2 = sum(temp_df$V2))
    new_df <- bind_rows(new_df, temp_df)
  }
  
  return(new_df)
}

# converts every column in dataframe into numeric type
ConvertColsToChar <- function (df) {
  df$V1 <- as.character(df$V1)
  df$V2 <- as.character(df$V2)
  return(df)
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

# append additional data to the profile (species, study_id, etc.)
AddMoreData <- function (profile, path_to_directory) {
  prefix <- profile$Prefix
  data_info_path <- list.files(path_to_directory, pattern = paste0(prefix, ".*\\.data_info.tsv$"), full.names = T)
  columns <- c("ORGANISM", "Annotation", "Run", "STUDY_ID", "Genome", "Genome:size", "Genome:min.length", "Genome:max.length", "Genome:num.sequences", "Genome:mean.length",
                   "Genome:median.length", "Exons:size", "Exons:min.length", "Exons:max.length", "Exons:num.seqs",
                   "Exons:mean.length", "Exons:median.length", "Transcripts:size", "Transcripts:min.length", "Transcripts:max.length", "Transcripts:num.seqs",
                   "Transcripts:mean.length", "Transcripts:median.length", "Genes:size", "Genes:min.length", "Genes:max.length", "Genes:num.seqs",
                   "Genes:mean.length", "Genes:median.length", "Taxon_ID", "KINGDOM")
  if (length(data_info_path) == 0) {
    if (is.null(runs_df)) {
      add_df <- data.table(matrix(ncol = length(columns), nrow = 1))
      colnames(add_df) <- columns
    } else {
      add_df <- runs_df[prefix, ][, -"Run", with = F]
    }
  } else {
    add_df <- fread(data_info_path)
  }
  
  profile_extended <- bind_cols(profile, add_df)
  return(profile_extended)
}
#--------------------------------------------------------------------------------------------------------

# Executable code-----------------------------------------------------------------------------------------

profile <- GenerateQCProfile(path_to_directory, prefix)
# writes the output to a file
write.table (profile, file = output, sep ="\t", row.names = F, col.names = T, quote = F)

q(status=0)
