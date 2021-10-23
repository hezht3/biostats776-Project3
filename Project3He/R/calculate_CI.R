#' Calculate Confidence Interval
#'
#' A function that computes a confidence interval (CI) (e.g. a 95% CI) for the estimate of the mean in the
#' population.
#'
#' @details This function computes the confidence interval to the mean value of samples contained in a vector
#' and the confidence level set by the user.
#'
#' @param x vector, a vector containing numeric values, the confidence interval of which mean being calcualted.
#' @param conf floating number, a number within the range of [0, 1] ($=1-\alpha$), with $\alpha$ as statistical
#' significance level.
#'
#' @return a matrix, where the first value of the matrix is the `lower_bound`, the second value of the matrix
#' is the `upper_bound`.
#'
#' @export
#'
#' @examples
#' x <- c(rnorm(100))
#' calculate_CI(x, conf = 0.95)
#'
calculate_CI <- function(x, conf) {
    if(is.vector(x) == FALSE & class(x) != "p3_class") stop("Error: x should be a vector")
    if(all(is.numeric(x), na.rm = TRUE) == FALSE) stop("Error: class of all elements in x should be numeric")
    if(is.numeric(conf) == FALSE) stop("Error: conf should be a number within [0, 1]")
    if(conf < 0 | conf > 1) stop("Error: conf should be a number within [0, 1]")

    if(class(x) == "p3_class") x <- unclass(x)

    mean <- sample_mean(x)
    sd_mean <- sample_sd(x)/sqrt(length(x))
    t_score = qt(p = (1 - conf)/2, df = length(x) - 1, lower.tail = FALSE)
    output <- matrix(c(mean - t_score*sd_mean, mean + t_score*sd_mean),
                     nrow = 1,
                     ncol = 2,
                     dimnames = list("(Intercept)",
                                     c("lower_bound", "upper_bound")))

    return(output)
}
