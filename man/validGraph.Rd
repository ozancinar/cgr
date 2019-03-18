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
   Ozan Cinar, Ozlem Ilk-Dag, Cem Iyigun
}