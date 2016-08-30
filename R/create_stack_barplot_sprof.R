create_stack_barplot_sprof <- function (Prefix, dataset, feature_type) {
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
      
      pdata <- profile[, grep(time_type, colnames(profile), value = T), with = F]
      p <- add_trace(data = pdata, x = as.numeric(pdata), y = value_types, 
                     type = "bar", orientation = "h", name = gsub("TIME_iRAP_|TIME_", "", time_type),
                     marker = list(color = color_now), legendgroup = time_type,
                     evaluate = T) %>%
        layout(xaxis = list(title = "Time, min"),
               yaxis = list(title = ""),
               barmode = "stack")
      
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
      
      pdata <- profile[, grep(memory_type, colnames(profile), value = T), with = F]
      p <- add_trace(data = pdata, x = as.numeric(pdata), y = value_types, 
                     type = "bar", orientation = "h", name = gsub("TIME_iRAP_|TIME_", "", memory_type),
                     marker = list(color = color_now), legendgroup = memory_type,
                     evaluate = T) %>%
        layout(xaxis = list(title = "Memory, MB"),
               yaxis = list(title = ""),
               barmode = "stack")
      
    }
  }
  return(p)
}