#' Intelligently take exponential
#'
#' The inverse of \code{\link{ilog}} for \code{base=exp(1)}.
#'
#' @param x A numeric vector
#' @param value A numeric indicating the value subtracted from exp(x).
#' Default is the smallest positive value of x.
#' @param ... Values passed on to log, e.g. base
#' @return A numeric vector, typically the logarithm of \code{x}
#' @seealso \code{\link{exp}},\code{\link{ilog}}, \code{\link{min_value}}
#' @export
#' @examples
#' x <- 0:2
#' all.equal(iexp(ilog(x)), x) # TRUE
iexp = function(x, value = NULL) {
  expx <- exp(x)

  if (is.null(value)) {
    warning('Inferring value to subtract from smallest value in exp(x).')
    value <- min(expx)
  }

  return(expx-value)
}
