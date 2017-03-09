
#' Validation Graphs
#'
#' This function plots the two validation scores for the given number of cluster sets
#'
#' @param valid The list including the validation scores. The result of \code{clValid}
#' is suggested to be used for this argument.
#' @param clNumber The interval of the number of cluster sets to be drawn in the plots
#' #' @return Two graph each of which displaying a single validation scores for the
#' given number of clusters.
#' @author Ozan Cinar, Ozlem Ilk-Dag, Cem Iyigun
#' @details
#' This function draws the two validation scores in separated graphs for the desired
#' number of clusters
#' @export

validGraph <- function(valid, clNumber, ...) {

    clIndex <- sort(intersect(clNumber, valid$clusterNumbers))
    within <- mat.or.vec(1, length(clIndex))
    between <- mat.or.vec(2, length(clIndex))

    for(i in 1:length(clIndex)) {
        within[i] <- valid$withinDist[which(valid$clusterNumbers == clIndex[i])]
        between[, i] <- valid$betweenDists[, which(valid$clusterNumbers == clIndex[i])]
    }

    par(mfrow = c(1, 2))
    plot(clNumber, (within / between[1, ]), xlab = "Cluster Number",
         ylab = "sum(within) / min(between)", ...)
    plot(clNumber, (within / between[2, ]), xlab = "Cluster Number",
         ylab = "sum(within) / mean(between)", ...)
}
