sample_mean <- function(x) {
    if(is.vector(x) == FALSE) stop("Error: x should be a vector")
    if(all(is.numeric(x), na.rm = TRUE) == FALSE) stop("Error: class of all elements in x should beretu")
    return(sum(x)/length(x))
}
