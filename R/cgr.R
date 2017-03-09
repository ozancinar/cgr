
#' Clustering Genes with Replicates
#'
#' Clustering the genes with respect to their behaviour similarities in their
#' expression levels through time.
#'
#' @param data a n x m matrix where n shows the number of genes and m shows the
#' multiplication of the number of time points and the number of replications.
#' The matrix includes the gene expression levels for each gene at each replications
#' and the time points.
#' @param tps a vector of length m which shows the succesive time points of the columns
#' in the data matrix.
#' @param reps a vector of length m which shows the replication number of genes
#' under each column of the data matrix.
#' @param w a value between 0 and 1 which sets the weight for the magnitude metric
#' used for the convex combination. Note that 1 - w is used as the weight for the
#' slope metric.
#' @param linkageMethod the linkage method used in the hierarchical clustering.
#' It is set for "Ward's Distance" as default.
#' @return a list which includes the cluster dendogram, magnitude, slope and the
#' combined distance matrices, successive time points as a vector and the data set
#' arranged in a 3-dimensional array where the new dimension reflects the time points.
#' @author Ozan Cinar, Ozlem Ilk-Dag, Cem Iyigun
#' @details
#' This function takes a data matrix where the rows are for the genes and the columns
#' are for replications and time points. Further, by using the weight, w, and the
#' linkage methods that are chosen, it calculates a general distance matrix which
#' considers both the magnitude and slope differences with contemprary weight chosen
#' by the user and uses this combined distance matrix to reach a dendogram of the genes.
#' @export


cgr <- function(data, tps, reps, w = 0.5, linkageMethod = "ward.D2", ...) {

    if(w < 0 | w > 1) {stop("Invalid weight (w)")}

    if ((mean(sapply(tapply(tps, reps, sort), function(x) {
        all(x == sort(unique(tapply(tps, reps, sort))[[1]]))
    }))) != 1) {stop("Successive time points in the replicates are not the
                     same")}

    if (length(unique(table(reps))) != 1) {stop("Length of the replications are
                                                not the same")}

    ### Converting the data set into a 3D array where the dimensions are G for the
    ### number of genes, K for the number of time points, R for the number of
    ### replications

    arrayData <- array(as.matrix(data), dim = c(dim(data)[1],
                                                (length(tps) / max(reps)), max(reps)))

    for(r in 1:max(reps)) {
        arrayData[, , r] <- arrayData[, order(tps[which(reps == r)]), r]
    }

    dimnames(arrayData) <- list(Genes = rownames(data),
                                TimePoints = paste("t = ", sort(unique(tapply(tps, reps, sort))[[1]]),
                                                   sep = ""),
                                Replication = paste("Rep", sort(unique(reps)), sep = ""))


    ### Sorting the time points into a vector named "time"

    successiveTime <- sort(unique(tapply(tps, reps, sort))[[1]])
    G <- dim(arrayData)[1]
    K <- dim(arrayData)[2]
    R <- dim(arrayData)[3]

    magDist <- mat.or.vec(G, G)
    slopeDist <- mat.or.vec(G, G)

    ### Calculating the squared Euclidean distances

    for(r in 1:R) {
        magDist <- magDist + (as.matrix(dist(arrayData[, , r])))^2
    }

    ### Calculating the slopes

    slopes <- array(NA, dim = c(G, (K - 1), R))
    for(r in 1:R) {
        for(g in 1:G) {
            for(k in 1:(K - 1)) {
                slopes[g, k, r] <- (arrayData[g, (k + 1), r] -
                                        arrayData[g, k, r]) / (successiveTime[k + 1] - successiveTime[k])
            }
        }
    }


    ### Calculating the squared STS distances

    for(r in 1:r) {
        slopeDist <- slopeDist + (as.matrix(dist(slopes[, , r])))^2
    }


    ### Standardizing the distance matrices

    rd <- range(magDist)
    if(rd[2] == 0 & rd[1] == 0) {magDist = magDist
    warning("Magnitude distances are all 0.")
    } else if((rd[2] - rd[1]) == 0) {magDist = magDist / (rd[1])
    } else {magDist <- magDist / (rd[2] - rd[1])
    }

    rc <- range(slopeDist)
    if(rc[2] == 0 & rc[1] == 0) {slopeDist = slopeDist
    warning("Slope distances are all 0.")
    } else if((rc[2] - rc[1]) == 0) {slopeDist = slopeDist / (rc[1])
    } else {slopeDist <- slopeDist / (rc[2] - rc[1])
    }

    dimnames(slopeDist) <- list(rownames(data), rownames(data))

    ### Combining the distance matrices

    distMat <- (w * magDist) + ((1 - w) * slopeDist)

    ### Clustering the genes with a hierarchical clustering

    cluster <- hclust(as.dist(distMat), method = linkageMethod)

    result <- list(HClust = cluster, distMat = distMat, magDist = magDist,
                   slopeDist = slopeDist, timePts = successiveTime,
                   expVals = arrayData)

    } # end of the function
