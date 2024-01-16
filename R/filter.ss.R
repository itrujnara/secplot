#' Filter a secondary structure based on a predicate
#'
#' @param ss The secondary_structure object to filter.
#' @param pred The predicate, in dplyr format.
#'
#' @return A secondary_structure object with the same ID and filtered features.
#' @export
#'
#' @examples
#' my_structure <- secondary_structure("a1", data.frame(Type = rep("a", 10),
#' Length = 1:10, Start = 1:10, Stop = 1:10))
#' # filter on a simple condition
#' structure_filtered <- filter_ss(my_structure, Length > 100)
#' # filter on a complex condition
#' structure_filtered_2 <- filter_ss(my_structure, (Start < 200 | Start > 500) & Length > 20)
filter_ss <- function(ss, pred) {
  stopifnot(is.secondary_structure(ss))
  pred <- dplyr::enquo(pred)
  id <- id(ss)
  features <- features(ss)
  features <- dplyr::filter(features, !!pred)
  return(secondary_structure(id = id, features = features))
}
