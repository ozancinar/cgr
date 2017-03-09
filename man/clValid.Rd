\name{clValid}
\alias{clValid}
\title{Evaluating Clusters}
\description{
This function finds the cluster validation scores for the desired number of
cluster sets.
}
\usage{
clValid(hcluster, clNumber)
}
\arguments{
\item{hcluster}{A list including the results of \code{cgr}.}

\item{clNumber}{A vector of the desired number of clusters.}
}
\value{
A list which includes within and between cluster distances for the
desired number of cluster. Further, the list includes the two validation scores.
}
\details{
Creates an object that includes cluster validation scores. Needs the result object of cgr
with minimum and maximum number of clusters to be searched. At the end it
results the within and between cluster validation values.
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
}
