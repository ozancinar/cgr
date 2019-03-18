
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
