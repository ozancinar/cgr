#' Interactive Graph
#'
#' This function draws the valid scores
#'
#' @param low The lower limit in the graph.
#' @param op The upper limit in the graph.
#' @return A graph
#' @author Ozan Cinar, Ozlem Ilk-Dag, Cem Iyigun
#' @details
#' An interactive graphing tool for the cluster validation scores
#' @export

validManipulate <- function(low, up) {
   if(low < 2) {stop("the smallest number of clusters can be 2.")}
   manipulate(validPlot(low = low, up = up), low = slider(low, up, step = 1, initial = low), up = slider(low, up, step = 1, initial = up))
}
