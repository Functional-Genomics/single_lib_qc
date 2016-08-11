transpose_profile <- function (profile) {
  if(is.null(profile) == T) return(NULL) 
  
  vector <- transpose(profile)
  rownames(vector) <- colnames(profile)
  colnames(vector) <- "Value"
  return(vector)
}