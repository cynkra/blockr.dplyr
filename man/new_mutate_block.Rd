% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mutate.R
\name{new_mutate_block}
\alias{new_mutate_block}
\title{Mutate block constructor}
\usage{
new_mutate_block(string = list(newcol = "paste('type', 'here')"), ...)
}
\arguments{
\item{string}{Reactive expression returning character vector of
expressions}

\item{...}{Additional arguments forwarded to \code{\link[=new_block]{new_block()}}}
}
\value{
A block object for mutate operations
}
\description{
This block allows to add new variables and preserve existing ones
(see \code{\link[dplyr:mutate]{dplyr::mutate()}}). Changes are applied after clicking the submit button.
}
\examples{
\dontrun{
# Basic usage with mtcars dataset
library(blockr.core)
serve(new_mutate_block(), list(data = mtcars))

# With a custom dataset
df <- data.frame(x = 1:5, y = letters[1:5])
serve(new_mutate_block(), list(data = df))
}
}
\seealso{
\code{\link[=new_transform_block]{new_transform_block()}}
}
