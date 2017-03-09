\name{validManipulate}
\alias{validManipulate}
\title{Interactive Cluster Validation Graphs}
\description{
This function draws the valid scores in an interactive graph. 
}
\usage{
validManipulate(low, up)
}
\arguments{
\item{low}{The lower limit in the graph.}

\item{up}{The upper limit in the graph.}
}
\value{
A graph
}
\details{
An interactive graphing tool for the cluster validation scores
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

# Calculating the cluster validation scores
# validManipulate(3, 30)
}

