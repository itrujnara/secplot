% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot.ss.R
\name{ss_plot_one}
\alias{ss_plot_one}
\title{Plot a single secondary structure}
\usage{
ss_plot_one(ss, position = c("level", "stagger"))
}
\arguments{
\item{ss}{A secondary_structure object.}

\item{position}{A string defining positioning, see below.}
}
\value{
A ggplot object.
}
\description{
Plot a single secondary structure
}
\section{Position}{
The following position options are available:
\itemize{
\item \code{level} - all features will be plotted in a single line
\item \code{stagger} - each feature will be shifted vertically
}
}

\examples{
\dontrun{
ss_plot_one(my_structure)
ss_plot_one(my_structure, position = "stagger")
}

}
