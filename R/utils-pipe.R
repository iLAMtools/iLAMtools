#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
#' @param lhs A value or the magrittr placeholder.
#' @param rhs A function call using the magrittr semantics.
#' @return The result of calling `rhs(lhs)`.
NULL

##' Check that value is in a range
##'
##' A shortcut for x >= a | x <= b.
##' @param x numeric values
##' @param range a vector of length two, of the form c(a,b)
##' @return a vector of logicals
##' 1:10 %inr% c(0,5)
##' @author Simon Barthelme
##' @export
`%inr%` <- function(x,range)
{
  if (!is.numeric(range) || length(range) != 2)
  {
    stop("Range must be a vector of 2 numeric values")
  }
  if (!is.numeric(x))
  {
    stop("x must be numeric")
  }
  else
  {
    if (diff(range) < 0)
    {
      stop('Range must be increasing')
    }

    x >= range[1] & x <= range[2]
  }
}
