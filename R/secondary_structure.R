secondary_structure <- function(id, features) {
  obj <- list(id = id, features = features)
  class(obj) <- "secondary_structure"
  return(obj)
}

is.secondary_structure <- function(obj) {
  return("secondary_structure" %in% class(obj))
}

print.secondary_structure <- function(obj) {
  cat("Secondary structure with ID ", obj$id, " and ",
      nrow(obj$features), "features\n")
}
