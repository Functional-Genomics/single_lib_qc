create_data_for_boxplot <- function(dataset, feature, value, type, total) {
  # feature: "ORGANISM" or "STUDY_ID"
  # value: only valid if TOTAL = F, some value of that feature (e.g. "mus_musculus" for ORGANISM) 
  # type: "num" or "per"
  # total: logical, should the data be filtered according to a single value?
  
  if (total == F) {
    id = "Prefix"
    raw_data <- subset(dataset, unlist(dataset[, feature, with = F]) %in% value)
    } else if (total == T) {
    id = feature
    raw_data <- create_stats_matrix(dataset, feature, mean)
    }
  
  raw_data <- raw_data[, c(id,
                           "STATS.CSV_All_entries", "STATS.CSV_ReadsSpliced", "STATS.CSV_ReadsUnmapped", "STATS.CSV_ReadsMapped", "STATS.CSV_UniquelyMappedReads", "STATS.CSV_MultimapReads"), with = F]
  
  if (type == "per") {
    for (reads_feature in colnames(raw_data)[3:length(colnames(raw_data))]) {
      dt <- (raw_data[, reads_feature, with = F] / raw_data[, "STATS.CSV_All_entries", with = F]) * 100
      raw_data[, reads_feature] <- dt
    }
  }
  
  plot_data <- melt(raw_data, id.vars = id, 
                    measure.vars = c("STATS.CSV_ReadsSpliced", "STATS.CSV_ReadsUnmapped", "STATS.CSV_ReadsMapped", "STATS.CSV_UniquelyMappedReads", "STATS.CSV_MultimapReads"), 
                    variable.name = "Feature", value.name = "Value")
  
  return(plot_data)
}