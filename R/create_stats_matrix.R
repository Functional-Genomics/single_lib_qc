create_stats_matrix <- function(dataset, type, stat_fun) {
  
  # type should be either "STUDY_ID" or "ORGANISM"
  # stat_fun examples: mean, sd, median
  
  data <- copy(dataset)
  if (type == "STUDY_ID") setkey(data, "STUDY_ID")
  if (type == "ORGANISM") setkey(data, "ORGANISM")
  
  ids <- unlist(unique(data[, type, with = F]))
  
  i <- 1
  
  dt_list <- list()
  for (id in ids) {
    dt <- data[id, ]
    numeric_part <- dt[,sapply(dt, is.numeric) | sapply(dt, is.integer), with = F]
    stats <- transpose(data.table(sapply(numeric_part, stat_fun, na.rm = T)))
    colnames(stats) <- names(numeric_part)
    stats <- bind_cols(data.table(id), stats)
    
    dt_list[[i]] <- stats
    i <- i+1
  }
  stats_matrix <- bind_rows(dt_list)
  stats_matrix$id[which(is.na(stats_matrix$id))] <- "No ID"
  colnames(stats_matrix)[1] <- type
  
  format(stats_matrix)
  return(stats_matrix)
} 

