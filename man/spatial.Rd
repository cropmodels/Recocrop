\name{spatial}

\alias{runSpatial}
\alias{runSpatial,Rcpp_EcocropModel-method}


\title{
Apply an EcoCrop model to spatial data 
}

\description{
tbd
}


\usage{
\S4method{runSpatial}{Rcpp_EcocropModel}(x, ..., filename="", overwrite=FALSE, wopt=list())
}


\arguments{
  \item{x}{EcocropModel object}
  \item{...}{SpatRaster objects with environmental data}  
  \item{filename}{character. Output filename. Optional}
  \item{overwrite}{logical. If \code{TRUE}, \code{filename} is overwritten}
  \item{wopt}{list. Options for writing files as in \code{\link{writeRaster}}}
}


\value{
SpatRaster
}


\examples{
# make ecocrop model
potato <- ecocropPars("potato")
m <- ecocrop(potato)

# Predictors 
library(terra)
f <- system.file("ex/tmin.tif", package="ecocrop")
tn <- rast(f)
f <- system.file("ex/prec.tif", package="ecocrop")
pr <- rast(f)

# match the names 
x <- run(m, tmin=tn, prec=pr)

plot(x)
}
