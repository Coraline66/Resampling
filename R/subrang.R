#' Second-step Resampling Function
#'
#' This function is used for constructing sample subsets from the
#' matrix of second-step resampling.
#' @param SEED  a single value, interpreted as an integer.
#' @param N  a positive integer giving the number of iterations.
#' @param n  a non-negative integer giving the number of items to choose.
#' @param j  a positive integer representing the row index of matrix sam to
#'        choose from.
#' @param sam  matrix attained by first-step resampling.
#' @param type  character indicating the type of data, e.g., "luad".
#' @keywords  resampling
#' @return a matrix that each row represents the indices of samples for
#'         each iteration.
#' @export
#' @examples
#' subrang(SEED=1, N=5, n=3, j=2, sam=sam, type="sarc")

subrang <- function(SEED, N, n, j, sam, type) {
    set.seed(SEED)
    samp <- list()
    if ((type == "luad")|(type == "brca")) {
        for (i in 1:N) {
            if (dim(sam)[2] == 1) {
                samp <- rbind(samp, rep(sam[j,], n))
            } else {
                sampl <- sample(sort(as.numeric(sam[j,])), n)
                samp <- rbind(samp, sampl)
            }
        }
    } else if (type == "sarc") {
        len <- card(sam=sam)
        for (i in 1:N) {
            if (len$dl[j] == 0) {
                if (len$lm[j] == 1) {
                    sampl <- sample(c(as.numeric(len$LM[[j]]),
                                      as.numeric(len$LM[[j]])), n, replace=TRUE)
                } else {
                    sampl <- sample(sort(as.numeric(len$LM[[j]])), n)
                }
            }
            if (len$lm[j] == 0) {
                if (len$dl[j] == 1) {
                    sampl <- sample(c(as.numeric(len$DL[[j]]),
                                      as.numeric(len$DL[[j]])), n, replace=TRUE)
                } else {
                    sampl <- sample(sort(as.numeric(len$DL[[j]])), n)
                }
            }
            if ((len$dl[j] > 0) & (len$lm[j] > 0)) {
                if (len$dl[j] == 1) {
                    sampl.dl <- sample(c(as.numeric(len$DL[[j]]),
                                      as.numeric(len$DL[[j]])), n, replace=TRUE)
                } else {
                    sampl.dl <- sample(sort(as.numeric(len$DL[[j]])), n)
                }
                if (len$lm[j] == 1) {
                    sampl.lm <- sample(c(as.numeric(len$LM[[j]]),
                                      as.numeric(len$LM[[j]])), n, replace=TRUE)
                } else {
                    sampl.lm <- sample(sort(as.numeric(len$LM[[j]])), n)
                }
                sampl <- append(sampl.dl, sampl.lm)
            }
            samp <- rbind(samp, sampl)
        }
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
