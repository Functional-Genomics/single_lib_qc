check_classes_file <- function(file,dataset,verbose=FALSE) {
  
    if ( ! file.exists(file)) {
        if (verbose) cat("Creating a new classes file: ",file,"...")
        classes <- data.table(dataset$Prefix, "?")
        colnames(classes) <- c("Prefix", "Class")
        # TSV (for consistency)
        write.table(classes, file, sep="\t",
                    append = FALSE, quote = FALSE, row.names = FALSE)
        if (verbose) cat("done.\n")
        return(FALSE)
    }
    return(TRUE)
}
