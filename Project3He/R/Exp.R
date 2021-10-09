#' Exponential transformation
#'
#' A function that computes the exponential of a number using the truncated series expansion.
#'
#' @details This function computes the exponential to the number to be exponentiated and the number of terms
#' to be used in the series expansion beyond the constant 1, passed as arguments to the function.
#'
#' @param x numeric, a single number to be exponentiated.
#' @param k integer, the number of terms to be used in the series expansion beyond the constant 1. The value
#' of `k` is always $\geq 1$.
#'
#' @return exponential of the number x.
#'
#' @export
#'
#' @examples
#' Exp(2, 5)
#' ## [1] 7.266667
#'
Exp <- function(x, k) {
    if (is.numeric(x) == FALSE) stop("Error: x should be a single number")

    is.wholenumber <-
        function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol

    if (is.wholenumber(k) == FALSE) stop("Error: k should be an integer")
    if (k < 1) stop("Error: the value of k should be greater or equal to 1")

    return(
        sum(sapply(c(0:k), function(t) x^t/factorial(t)))
    )
}
