
validPlot <- function(low, up) {
   par(mfrow = c(1, 2))
   if(!any(valid$clusterNumbers == low)) {stop("the lowest number of clusters entered is not in the validation scores.")}
   if(!any(valid$clusterNumbers == up)) {stop("the highest number of clusters entered is not in the validation scores.")}
   lowP <- which(valid$clusterNumbers == low)
   upP <- which(valid$clusterNumbers == up)
   
   range <- valid$clusterNumbers[lowP:upP]
   
   valids1 <- valid$valid1[lowP:upP]
   plot(range, valids1, xlab = "Cluster Number",
         ylab = "sum(within) / min(between)", ylim = range(valids1), xlim = c(low, up),
         type = "b", pch = 19)
   minP <- which.min(valids1)
   points(minP + (low - 1), valids1[minP], pch = 19, col = "purple", cex = 1.5)
   decP <- which.min(diff(valids1) / valids1[-1])
   lines((decP + low - 1):(decP + low), valids1[decP:(decP + 1)], col = "green", lwd = 2)
   legend("topright", c("Min Score", "Largest Decrease"), pch = c(19, 19), lty = c(0, 1), col = c("purple", "green")) 
   
   valids2 <- valid$valid2[lowP:upP]
   plot(range, valids2, xlab = "Cluster Number",
        ylab = "sum(within) / min(between)", ylim = range(valids2), xlim = c(low, up),
        type = "b", pch = 19)
   minP <- which.min(valids2)
   points(minP + (low - 1), valids2[minP], pch = 19, col = "purple", cex = 1.5)
   decP <- which.min(diff(valids2) / valids2[-1])
   lines((decP + low - 1):(decP + low), valids2[decP:(decP + 1)], col = "green", lwd = 2)
   legend("topright", c("Min Score", "Largest Decrease"), pch = c(19, 19), lty = c(0, 1), col = c("purple", "green")) 
}
