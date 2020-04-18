\name{crop}

\alias{crop<-}
\alias{crop<-,Rcpp_EcocropModel,matrix-method}

\title{
Set crop paramters
}

\description{
Set crop parameters to a ecocrop model. 
}


\usage{
crop(x) <- value
}

\arguments{
 \item{x}{EcocropModel object}
 \item{value}{ matrix with crop parameters. Each column represents parameters for an environmental variable, and must have a name. The matrix must have four rows that represent the x-coordinates for a table-mountain shaped relative response function (y-coordinates: 0,1,1,0; see the plot in the examples). The only exception is the "duration" parameter with has a single value (in the first row). For functions where only "one tail" is relevant, the values for the other tail can be set to very high or low values, including Inf and -Inf.
 See \code{\link{ecocropPars}} for default parameters for many species.
}
}

\value{
none
}

\examples{
x <- ecocropPars("potato")
potato <- x$parameters

m <- ecocrop()
crop(m) <- potato

plot(m)
}
