check_classes_file <- function(R_path, dataset) {
  
  if (file.exists(paste0(R_path, "/classes")) == F) {
    classes <- data.table(dataset$Prefix, "?")
    colnames(classes) <- c("Prefix", "Class")
    write.table(classes, paste0(R_path, "/classes"), 
                append = F, quote = F, row.names = F)
  }
  
}