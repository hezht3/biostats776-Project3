#' Print `p3_class`
#'
#' Print number of observations of a `p3_class` (Project 3 class).
#'
#' @details This function prints number of observations of a `p3_class` object.
#'
#' @param x p3_class, a `p3_class` pbject.
#'
#' @return a `p3_class` object.
#'
#' @export
#'
#' @examples
#' set.seed(1234)
#' x <- rnorm(100)
#' p3 <- make_p3_class(x)
#' print(p3)         # explicitly using the print() method
#' ## a p3_class with 100 observations
#' p3                # using autoprinting
#' ## a p3_class with 100 observations
#'
print.p3_class <- function(x) {
    cat("a p3_class with",
        length(x),
        "observations",
        sep = " ")
    invisible(x)
}
