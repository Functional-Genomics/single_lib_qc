create_stack_barplot <- function (Prefix, dataset, feature_type) {
  profile <- dataset[Prefix, ]
  
  if (feature_type == "TIME") {
    profile <- profile[, grep("TIME_.*", colnames(profile), value = T), with = F]
    profile <- profile[, -grep("TIME_.*_memory_.*|TIME_sum", colnames(profile), value = T), with = F]
    
    time_types <- unique(gsub("_last$|_max$|_sum$", "", colnames(profile)))
    value_types <- c("last", "max", "sum")
    
    p <- plot_ly()
    for (time_type in time_types) {
      for (value_type in value_types) {
        pdata <- profile[, grep(paste0(time_type, "_last|",
                                       time_type, "_max|",
                                       time_type, "_sum"), colnames(profile), value = T), with = F]
        p <- add_trace(data = pdata, x = as.numeric(pdata), y = value_type, 
                       type = "bar", orientation = "h", name = gsub("TIME_iRAP_|TIME_", "", paste0(time_type, "_", value_type)),
                       evaluate = T) %>%
          layout(xaxis = list(title = "Time, min"),
                 yaxis = list(title = ""),
                 margin = list(l = 300),
                 barmode = "stack")
      }
    }
  }
  if (feature_type == "MEMORY") {
    profile <- profile[, grep("TIME_.*", colnames(profile), value = T), with = F]
    profile <- profile[, grep("TIME_.*_memory_.*", colnames(profile), value = T), with = F]
    
    memory_types <- unique(gsub("_last$|_max$", "", colnames(profile)))
    value_types <- c("last", "max")
    
    p <- plot_ly()
    for (memory_type in memory_types) {
      for (value_type in value_types) {
        pdata <- profile[, grep(paste0(memory_type, "_last|",
                                       memory_type, "_max|",
                                       memory_type, "_sum"), colnames(profile), value = T), with = F]
        p <- add_trace(data = pdata, x = as.numeric(pdata), y = value_type, 
                       type = "bar", orientation = "h", name = gsub("TIME_iRAP_|TIME_", "", paste0(memory_type, "_", value_type)),
                       evaluate = T) %>%
          layout(xaxis = list(title = "Memory, MB"),
                 yaxis = list(title = ""),
                 margin = list(l = 300),
                 barmode = "stack")
      }
    }
  }
  return(p)
}