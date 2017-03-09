\name{generateRepData}
\alias{generateRepData}
\title{Generating Short Time-Course Replicated Data}
\description{
This function generates random genes that are similar to the ones in the simulation set used in Irigoien et al., 2011. 
}
\usage{
generateRepData(nofg = 20, tpl = 1, tpu = 6, unequal = FALSE, nofgl = NULL, nofgu = NULL)
}
\arguments{
\item{nofg}{the number of genes wanted to be in each groups. If the same number of genes wanted for each group, the number should be specified here, and unequal argument should be FALSE.}
\item{tpl}{the smallest time point. As default it is set to 1.}
\item{tpu}{the highest time point. As default it is set to 6.}
\item{unequal}{the argument to specify if different number of genes are wanted for each cluster.}
\item{nofgl}{this argument specifies the lowest number of genes that can be in any groups if the number of genes in groups are unequal.}
\item{nofgu}{this argument specifies the highest number of genes that can be in any groups if the number of genes in groups are unequal.}
}
\value{
A list with three objects. The first object (nofgenesingroups) shows how many genes there are in each group. The second object keeps from which group the individual genes were generated. The last object is a data matrix where the rows are genes and the columns are time points and replicates.
}
\author{
   Ozan Cinar \email{ozancinar@maastrichtuniversity.nl} \cr
   Ozlem Ilk-Dag 
   Cem Iyigun
}
\references{
   I. Irigoien, V. Sergi and A. Concepcion, (2011) \emph{Microarray Time Course Experiments: Finding Profiles}, IEEE-ACM Transactions on Computational Biology and Bioinformatics 8, 464 - 475.
}
\examples{
# Generate a data with the default arguments. 
# The data wil include 15 groups each of which contains 20 genes.
# There will be 6 time points and 2 replications.
# data <- generateRepData()

# To generate genes with different number (randomly selected between 5 and 25) for each group.
# data <- generateRepData(unequal = TRUE, nofgl = 5, nofgu = 25)
# table(data$group)
}

