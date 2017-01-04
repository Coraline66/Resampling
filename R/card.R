#' SARC Cardinality Calculation
#'
#' The function provides a prerequisite to the second-step resampling.
#' It calculates the cardinalities of two sample conditions in the new
#' truth set for TCGA-sarc.
#' @param sam  matrix attained from first-step resampling.
#' @keywords  cardinality-sarc
#' @return a list consisting of vectors that contain the information of
#'         sample partitions "liposarcoma" and "leiomyosarcoma" of the new truth
#'         sets, including the cardinalities.
#' @export
#' @examples
#' card(sam)

card <- function(sam) {
    DL <- vector(mode = "list")
    LM <- vector(mode = "list")
    dl <- vector()
    lm <- vector()
    for (k in 1:dim(sam)[1]) {
        DL[[k]] <- vector()
        LM[[k]] <- vector()
        for (l in 1:dim(sam)[2]) {
            if (sam[k, l] <= 58) {
                DL[[k]] <- append(DL[[k]], sam[k, l]$sam)
            } else {
                LM[[k]] <- append(LM[[k]], sam[k, l]$sam)
            }
        }
        dl[k] <- length(DL[[k]])
        lm[k] <- length(LM[[k]])
    }
    len <- list("DL" = DL, "LM" = LM, "dl" = dl, "lm" = lm)
    return(len)
}
