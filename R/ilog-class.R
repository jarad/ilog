#' ilog class
#'
#' Creates the ilog class which contains attributes for whether or not the
#' vector was logged and the value that was added if it was.
#'
#' @param x A vector
#' @return A vector of class ilog
#' @examples
#' new_ilog(1:2, value = 1, logged = TRUE)
new_ilog <- function(x, value, logged = TRUE) {
  attr(x, "value")  <- value
  attr(x, "logged") <- logged
  set_ilog_class(x)
}


print.ilog <- function(x) {
  print(as.numeric(x))
}

set_ilog_class <- function(x) {
  class(x) <- c("ilog", class(x))
  x
}

remove_ilog_class <- function(x) {
  attr(x, "value") <- NULL
  attr(x, "logged") <- NULL
  class(x) <- class(x)[which(class(x) != "ilog")]
  x
}
