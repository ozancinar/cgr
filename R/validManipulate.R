
validManipulate <- function(low, up) {
   if(low < 2) {stop("the smallest number of clusters can be 2.")}
   manipulate(validPlot(low = low, up = up), low = slider(low, up, step = 1, initial = low), up = slider(low, up, step = 1, initial = up))
}
