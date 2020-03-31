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
crop <- ecocropPars("Pearl millet")
m <- ecocrop(crop)

# Predictors 
library(terra)
f <- system.file("ex/tavg.tif", package="ecocrop")
ta <- rast(f)
f <- system.file("ex/prec.tif", package="ecocrop")
pr <- rast(f)

options(m) <- list(max=TRUE)
# match the names 
x <- runSpatial(m, tavg=ta, prec=pr)
plot(x)

options(m) <- list(max=FALSE)
# match the names 
xx <- runSpatial(m, tavg=ta, prec=pr)
plot(xx[[7:8]])
}
