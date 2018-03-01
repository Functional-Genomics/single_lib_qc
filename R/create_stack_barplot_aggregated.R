create_stack_barplot_agg <- function (stats_matrix, feature_type, value_type, stats_type) {
  # stats_matrix: means, median or sd matrix
  # feature_type: "TIME" or "MEMORY"
  # value_type: sum, last or max
  # stats_type: "mean", "median", "sd"
  
  if (feature_type == "TIME") 
    columns <- grep(paste0("^TIME", "_[^memory]*_", value_type), colnames(stats_matrix), value = T)
  if (feature_type == "MEMORY") 
    columns <- grep(paste0("TIME", "_.*_memory_", value_type), colnames(stats_matrix), value = T)

  group <- stats_matrix[, c(colnames(stats_matrix)[1], columns), with = F]
  setkeyv(group, colnames(group)[1])
  
  colors = brewer.pal(length(colnames(group)), "Paired")
  color_tick <- 1
  
  p <- plot_ly()
  for (type in colnames(group)[-1]) {
    color_now = colors[color_tick]
    color_tick <- color_tick + 1
    
    p <- add_trace(p, data = group, x = unlist(group[, type, with = F]), y = unlist(group[, 1, with = F]), 
                   type = "bar", orientation = "h", name = gsub("TIME_iRAP_|TIME_", "", type),
                   marker = list(color = color_now), legendgroup = type,
                   evaluate = T) %>%
      layout(xaxis = list(title = paste0(feature_type, " (", value_type, "), min (", stats_type, ")")),
             yaxis = list(title = ""),
             margin = list(l = 200),
             barmode = "stack")
    
  }
  return(p)
}

# # function that replaces NA with 0 in data table
# replace_na <- function(DT) {
#   for (i in names(DT))
#     DT[is.na(get(i)), i:=0, with=FALSE]
# }

