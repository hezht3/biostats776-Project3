#' Create `p3_class`
#'
#' create a new S3 class called `p3_class` (Project 3 class).
#'
#' @details This function create a new S3 class called `p3_class` to the vector input by the user.
#'
#' @param x vector, a vector to be transformed to `p3_class`.
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
make_p3_class <- function(x) {
    if(is.vector(x) == FALSE) stop("Error: x should be a vector")

    structure(x, class = "p3_class")
}
