
clValid <- function(hcluster, clNumber) {

    if(min(clNumber) == 1) {stop("Minimum number of clusters cannot be one.
                                 Between Distance is zero.")}

    sumWithin <- vector(mode = "numeric", length = length(clNumber))
    minBetween <- vector(mode = "numeric", length = length(clNumber))
    meanBetween <- vector(mode = "numeric", length = length(clNumber))
    dendrogram <- hcluster$HClust
    dist <- hcluster$distMat

    for(c in seq_along(clNumber)) {

        cn <- clNumber[c]
        cl <- cutree(dendrogram, k = cn)
        with1 <- vector(mode = "numeric", length = cn)
        btw1 <- vector(mode = "numeric", length = cn)
        btw2 <- vector(mode = "numeric", length = cn)

        for(i in 1:cn) {

            temp <- dist[which(cl == i), which(cl == i)]
            with1[i] <- sum(temp) / 2

            b1 <- vector(mode = "numeric", length = cn - 1)
            b2 <- vector(mode = "numeric", length = cn - 1)
            v <- 1

            for(j in 1:cn) {

                if(i == j) {v = v
                } else {
                    temp <- dist[which(cl == i), which(cl == j)]
                    b1[v] <- min(temp)
                    b2[v] <- mean(temp)
                    v <- v + 1
                }
            }

            btw1[i] <- min(b1)
            btw2[i] <- min(b2)
        }

        sumWithin[c] <- max(with1)
        minBetween[c] <- min(btw1)
        meanBetween[c] <- min(btw2)
    }

    withinDist <- as.matrix(t(sumWithin))
    dimnames(withinDist) <- list(method = "sum(within)",
                                 clusterNumber = paste(clNumber, "Cls", sep = ""))
    betweenDist <- rbind(minBetween, meanBetween)
    dimnames(betweenDist) <- list(method = c("min(between)", "mean(between)"),
                                  clusterNumber = paste(clNumber, "Cls", sep = ""))

    result <- list(withinDist = withinDist, betweenDists = betweenDist,
                   clusterNumbers = clNumber,
                   valid1 = withinDist / betweenDist[1, ],
                   valid2 = withinDist / betweenDist[2, ])

}

