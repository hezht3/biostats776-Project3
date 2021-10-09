sample_sd <- function(x) {
    if(is.vector(x) == FALSE) stop("Error: x should be a vector")
    if(all(is.numeric(x), na.rm = TRUE) == FALSE) stop("Error: class of all elements in x should be numeric")
    return(sqrt(sum(sapply(x, function(t) (t - sum(x)/length(x))^2))/(length(x) - 1)))
}
