create_stack_barplot <- function (Prefix, dataset, feature_type) {
  profile <- dataset[Prefix, ]
  
  if (feature_type == "TIME") {
    profile <- profile[, grep("TIME_.*", colnames(profile), value = T), with = F]
    profile <- profile[, -grep("TIME_.*_memory_.*|TIME_sum", colnames(profile), value = T), with = F]
    
    time_types <- unique(gsub("_last$|_max$|_sum$", "", colnames(profile)))
    value_types <- c("last", "max", "sum")
    
    colors_time = brewer.pal(length(time_types), "Paired")
    color_tick <- 1
    
    p <- plot_ly()
    for (time_type in time_types) {
      color_now = colors_time[color_tick]
      color_tick <- color_tick + 1
      for (value_type in value_types) {
        pdata <- profile[, paste0(time_type, "_", value_type), with = F]
        p <- add_trace(data = pdata, x = as.numeric(pdata), y = value_type, 
                       type = "bar", orientation = "h", name = gsub("TIME_iRAP_|TIME_", "", paste0(time_type, "_", value_type)),
                       marker = list(color = color_now),
                       evaluate = T) %>%
          layout(xaxis = list(title = "Time, min"),
                 yaxis = list(title = ""),
                 barmode = "stack")
      }
    }
  }
  if (feature_type == "MEMORY") {
    profile <- profile[, grep("TIME_.*", colnames(profile), value = T), with = F]
    profile <- profile[, grep("TIME_.*_memory_.*", colnames(profile), value = T), with = F]
    
    memory_types <- unique(gsub("_last$|_max$", "", colnames(profile)))
    value_types <- c("last", "max")
    
    colors_mem = brewer.pal(length(memory_types), "Paired")
    color_tick <- 1
    
    p <- plot_ly()
    for (memory_type in memory_types) {
      color_now = colors_mem[color_tick]
      color_tick <- color_tick + 1
      for (value_type in value_types) {
        pdata <- profile[, paste0(memory_type, "_", value_type), with = F]
        p <- add_trace(data = pdata, x = as.numeric(pdata), y = value_type, 
                       type = "bar", orientation = "h", name = gsub("TIME_iRAP_|TIME_", "", paste0(memory_type, "_", value_type)),
                       marker = list(color = color_now),
                       evaluate = T) %>%
          layout(xaxis = list(title = "Memory, MB"),
                 yaxis = list(title = ""),
                 barmode = "stack")
      }
    }
  }
  return(p)
}