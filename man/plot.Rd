\name{plot-ecocrop}

\alias{plot,Rcpp_EcocropModel,ANY-method}

\title{
Ecocrop plot
}

\description{
Plot the parameters of an ecocrop model
}


\usage{
\S4method{plot}{Rcpp_EcocropModel}(x, nc, nr, col="red", ...)
}

\arguments{
 \item{x}{EcocropModel object}

 \item{nc}{number of columns in the plot}
  
 \item{nr}{number of rows in the plot}

 \item{col}{color of the lines}
  
 \item{...}{additional arguments. None implemented}
}


\value{
none
}

\examples{
library(ecocrop)
potato <- ecocropPars("potato")
m <- ecocrop(potato)
plot(m)
}
