#' Intelligently take logarithms
#'
#' Deals with zero values when taking logarithms by adding `value` to all
#' observations before taking logarithms. By default, `value` is the minimum
#' non-zero value.
#'
#' @param x A numeric vector
#' @param value A numeric indicating the value to add to x before taking logs.
#' Default is the smallest positive value of x.
#' @param ... Values passed on to log, e.g. base
#' @return A numeric vector, typically the logarithm of \code{x}
#' @seealso \code{\link{log}}, \code{\link{iexp}}, \code{\link{min_value}}
#' @export
#' @examples
#' ilog(1:2)             # Returns log(1:2)
#' ilog(-2:2)            # Returns an error due to negative values
#' ilog(0:2)             # Returns log(0:2+1)
#' ilog(-2:2, value = 3) # Returns log(-2:2 + 3)
ilog = function(x, value = min_value(x), ...) {
  if (all(x > 0, na.rm = TRUE)) {
    warning('`x` is all positive. `value` was ignored and logs were taken.')
    return(log(x, ...))
  }

  xv <- x + value

  if (any(x < 0, na.rm = TRUE)) {
    if (any(xv <=0, na.rm = TRUE)) {
      stop('Non-positive `x+value` values detected.')
    } else {
      warning('Negative `x` values detected, but all `x+value` are positive and therefore `log(x+value)` are return.')
    }
  }

  return(log(xv, ...))
}
