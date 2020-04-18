\name{plot-ecocrop}

\alias{plot,Rcpp_EcocropModel,ANY-method}
\alias{plot}

\title{
Ecocrop plot
}

\description{
Plot the parameters of an ecocrop model
}


\usage{
\S4method{plot}{Rcpp_EcocropModel,ANY}(x, nr, nc, col="red", ...)
}

\arguments{
 \item{x}{EcocropModel object}
 \item{nr}{number of rows in the plot}
 \item{nc}{number of columns in the plot}
 \item{col}{color of the lines}
 \item{...}{additional arguments. None implemented}
}


\value{
none
}

\examples{
potato <- ecocropPars("potato")
m <- ecocrop(potato)
plot(m)
}
