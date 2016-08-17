determine_profile_class <- function(prefix, classes) {
  if (classes[prefix, ]$Class == "?" | is.na(classes[prefix, ]$Class) == T) {
    current_type = "Unknown"
  }
  else {
    current_type = classes[prefix, ]$Class
  } 
  
  return(current_type)
}