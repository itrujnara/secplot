#' NOT IMPLEMENTED
#'
#' This function is not implemented. Use ss_plot_one or ss_plot_many instead.
#'
#' @param x A secondary_structure object.
#' @param ... Other arguments, unused.
#'
#' @return Nothing.
#' @export
#'
plot.secondary_structure <- function(x, ...) {
  stop("plot() generic is not implemented for secondary_structure and will likely not be.
       Use dedicated plotting functions instead.")
}

#' Plot a single secondary structure
#'
#' @param ss A secondary_structure object.
#' @param position A string defining positioning, see below.
#'
#' @details
#' # Position
#' The following position options are available:
#' - `level` - all features will be plotted in a single line
#' - `stagger` - each feature will be shifted vertically
#'
#' @return A ggplot object.
#' @export
#'
#' @examples
#' \dontrun{
#' ss_plot_one(my_structure)
#' ss_plot_one(my_structure, position = "stagger")
#' }
#'
ss_plot_one <- function(ss, position = c("level", "stagger")) {
  position <- match.arg(position)
  ypos <- switch(position,
                 level = as.factor(1),
                 stagger = as.factor(1:nrow(features(ss))))
  p <- ggplot2::ggplot(features(ss)) +
    ggplot2::aes(x = .data$Start, xend = .data$End, y = ypos,
                 yend = ypos, color = .data$Type) +
    ggplot2::geom_segment(linewidth = 3) +
    ggplot2::labs(title = paste("Secondary structure with ID", id(ss)), x = "Position",
         y = "") +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.y = ggplot2::element_blank(),
                   axis.ticks.y = ggplot2::element_blank(),
                   legend.title = ggplot2::element_blank())
  return(p)
}

#' Plot multiple secondary structures for comparison
#'
#' @param ss_list A list of secondary_structure objects.
#'
#' @return A ggplot object.
#' @export
#'
#' @examples
#' \dontrun{
#' ss_plot_many(my_structures)
#' }
#'
ss_plot_many <- function(ss_list) {
  strucs <- lapply(ss_list, function(ss) {
    f <- features(ss)
    f$ID <- id(ss)
    return(f)
  }) %>% dplyr::bind_rows()

  p <- ggplot2::ggplot(strucs) +
    ggplot2::aes(x = .data$Start, xend = .data$End, y = .data$ID,
                 yend = .data$ID, color = .data$Type) +
    ggplot2::geom_segment(linewidth = 2) +
    ggplot2::labs(title = paste("Comparison of", length(ss_list), "secondary structures"), x = "Position") +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.title = ggplot2::element_blank())
  return(p)
}
