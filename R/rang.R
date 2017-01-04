#' First-step Resampling Function
#'
#' This function allows you to randomly generate a desired number
#' of samples for certain times from your data sets.
#' @param SEED  a single value, interpreted as an integer.
#' @param N  a positive integer giving the number of iterations.
#' @param M  a positive integer giving the range for resampling.
#' @param n  a non-negative integer giving the number of items to choose.
#' @keywords  resampling
#' @return a matrix that each row represents the indices of samples for each iteration.
#' @export
#' @examples
#' rang(1, 10, 100, 5)

rang <- function(SEED, N, M, n) {
    set.seed(SEED)
    samp <- list()
    for (i in 1:N) {
        sam <- sample(1:M, n)
        samp <- rbind(samp, sam)
    }
    for (i in 1:N) {
        for (j in 1:n) {
            if (samp[i, j] < 10) {
                samp[i, j] <- paste(0, samp[i, j], sep="")
            }
        }
    }
    return(samp)
}
