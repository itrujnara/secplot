read.ss <- function(path, header = TRUE, id = NULL) {
  data <- read.delim(path, header = header)
  stopifnot(ncol(data) == 4)
  colnames(data) <- c("Type", "Length", "Start", "End")
  def_id <- tools::file_path_sans_ext(basename(path))
  new_id <- ifelse(is.null(id), def_id, id)
  return(secondary_structure(id = new_id, features = data))
}
