
write_classes <- function(file,classes) {

    write.table(classes, classes_file, sep="\t",
                append = FALSE, quote = FALSE, row.names = FALSE)

}
check_classes_file <- function(file,dataset,verbose=FALSE) {
  
    if ( ! file.exists(file)) {
        if (verbose) cat("Creating a new classes file: ",file,"...")
        classes <- data.table(dataset$Prefix, "?")
        colnames(classes) <- c("Prefix", "Class")
        # TSV (for consistency)
        write_classes(file,classes)
        if (verbose) cat("done.\n")
        return(FALSE)
    }
    return(TRUE)
}
