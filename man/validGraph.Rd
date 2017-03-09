\name{validGraph}
\alias{validGraph}
\title{Validation Graphs}
\description{
This function draws static plots of the two validation scores for the given number of cluster sets.
}
\usage{
validGraph(valid, clNumber, ...)
}
\arguments{
\item{valid}{The list including the validation scores. The result of \code{clValid}
is suggested to be used for this argument.}

\item{clNumber}{The interval of the number of cluster sets to be drawn in the plots
#'}
}
\value{
Two graph each of which displaying a single validation scores for the
given number of clusters.
}
\details{
This function draws the two validation scores in separated graphs for the desired
number of clusters
}
\author{
   Ozan Cinar \email{ozancinar@maastrichtuniversity.nl} \cr
   Ozlem Ilk-Dag 
   Cem Iyigun
}
\examples{
# Generate a data with the default arguments. 
# The data wil include 15 groups each of which contains 20 genes.
# There will be 6 time points and 2 replications.
# data <- generateRepData()

# Getting the cluster object
# exp <- data$expvals
# hclust <- cgr(data = exp, tps = rep(c(1:6), 2), reps = rep(1:2, each = 6)) 

# Calculating the cluster validation scores
# valid <- clValid(hcluster = hclust, clNumber = 3:30)

# Drawing the cluster validation graphs
# validGraph(valid = valid, clNumber = 6:30, pch = 19, type = "b")
}


