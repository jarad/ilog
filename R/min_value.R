#' Find minimum non-zero value
#'
#' Find the minimum non-zero value in a numeric vector. Returns an error if
#' any negative values exist.
#'
#' @param x A numeric vector
#' @return The minimum non-zero value of x
#'
#' @seealso \code{\link{ilog}}, \code{\link{iexp}}
#' @export
#' @examples
#' min_value(0:2) # 1
min_value <- function(x) {
  if (any(x<0, na.rm=TRUE))
    stop("Negative values in x.")

  return(min(x[x>0], na.rm = TRUE))
}
