#' Create a secondary structure object
#'
#' @param id The ID of the structure, use in reports and plots
#' @param features A data.frame containing the secondary structure features,
#' with columns containing type, length, start and end positions
#'
#' @return A list of class "secondary_structure"
#' @export
#'
#' @examples
#' my_features <- data.frame()
#' my_structure <- secondary_structure("species1", my_features)
secondary_structure <- function(id, features) {
  obj <- list(id = id, features = features)
  class(obj) <- "secondary_structure"
  return(obj)
}

#' Check if an object is a secondary_structure
#'
#' @param obj The object to be checked.
#'
#' @return Boolean.
#' @export
#'
#' @examples
#' my_structure <- secondary_structure("a1", data.frame())
#' is.secondary_structure(my_structure)
is.secondary_structure <- function(obj) {
  return(inherits(obj, "secondary_structure"))
}

#' Print a secondary_structure object's basic information
#'
#' @param x A secondary_structure object.
#' @param ... Other arguments for print(), not used but included for consistency.
#'
#' @return Nothing.
#' @export
#'
#' @examples
#' # call the generic instead of the method
#' my_structure <- secondary_structure("a1", data.frame())
#' print(my_structure)
print.secondary_structure <- function(x, ...) {
  cat("Secondary structure with ID", x$id, "and",
      nrow(x$features), "features\nUse ss.summary() for details\n")
}

#' Get features from a secondary_structure object
#'
#' @param obj A secondary_structure object.
#'
#' @return A data.frame.
#' @export
#'
#' @examples
#' my_structure <- secondary_structure("a1", data.frame())
#' my_features <- features(my_structure)
features <- function(obj) {
  stopifnot(is.secondary_structure(obj))
  return(obj$features)
}

#' Get id from a secondary_structure object
#'
#' @param obj A secondary_structure object.
#'
#' @return A character string.
#' @export
#'
#' @examples
#' my_structure <- secondary_structure("a1", data.frame())
#' my_id <- id(my_structure)
id <- function(obj) {
  stopifnot(is.secondary_structure(obj))
  return(obj$id)
}
