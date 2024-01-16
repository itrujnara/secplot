#' Load a secondary structure object from a TSV file
#'
#' @param path The path to the TSV file.
#' @param header Whether the first line in the file contains TSV headers.
#' @param id The ID that should be stored in the object. Optional, inferred from the filename if not given.
#'
#' @return A list of class "secondary_structure".
#' @export
#'
#' @examples
#' \dontrun{
#' structure <- read.ss("structure.tsv")
#' structure2 <- read.ss("~/data/other.tsv", header = FALSE, id = "dog")
#' }
read.ss <- function(path, header = TRUE, id = NULL) {
  data <- utils::read.delim(path, header = header)
  stopifnot(ncol(data) == 4)
  colnames(data) <- c("Type", "Length", "Start", "End")
  def_id <- tools::file_path_sans_ext(basename(path))
  new_id <- ifelse(is.null(id), def_id, id)
  return(secondary_structure(id = new_id, features = data))
}
