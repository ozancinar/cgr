\name{cgr}
\alias{cgr}
\title{Clustering Genes with Replicates}
\description{
Clustering the genes with respect to their behaviour similarities in their
expression levels through time.
}
\usage{
cgr(data, tps, reps, w = 0.5, linkageMethod = "ward.D2", ...)
}
\arguments{
\item{data}{a n x m matrix where n shows the number of genes and m shows the
multiplication of the number of time points and the number of replications.
The matrix includes the gene expression levels for each gene at each replications
and the time points.}

\item{tps}{a vector of length m which shows the succesive time points of the columns
in the data matrix.}

\item{reps}{a vector of length m which shows the replication number of genes
under each column of the data matrix.}

\item{w}{a value between 0 and 1 which sets the weight for the magnitude metric
used for the convex combination. Note that 1 - w is used as the weight for the
slope metric.}

\item{linkageMethod}{the linkage method used in the hierarchical clustering.
It is set for "Ward's Distance" as default.}
}
\value{
a list which includes the cluster dendogram, magnitude, slope and the
combined distance matrices, successive time points as a vector and the data set
arranged in a 3-dimensional array where the new dimension reflects the time points.
}
\details{
This function takes a data matrix where the rows are for the genes and the columns
are for replications and time points. Further, by using the weight, w, and the
linkage methods that are chosen, it calculates a general distance matrix which
considers both the magnitude and slope differences with contemprary weight chosen
by the user and uses this combined distance matrix to reach a dendogram of the genes.
}
\author{
   Ozan Cinar, Ozlem Ilk-Dag, Cem Iyigun
}