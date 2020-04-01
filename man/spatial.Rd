\name{spatial}

\alias{runSpatial}
\alias{runSpatial,Rcpp_EcocropModel-method}


\title{
Spatial EcoCrop model
}

\description{
Make spatial predictions with an EcoCrop model. Create a model, and provide SpatRaster objects as predictors.
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

## Predictors 
library(terra)
fta <- system.file("ex/ta.tif", package="ecocrop")
fpr <- system.file("ex/pr.tif", package="ecocrop")
fph <- system.file("ex/ph.tif", package="ecocrop")

# monthly average temperature
ta <- rast(fta)
# monthly precipitation
pr <- rast(fpr)
# pH
ph <- rast(fph)

# just for plotting
preds <- c(mean(ta), sum(pr), ph)
names(preds) <- c("tavg", "prec", "ph")
plot(preds)


## make ecocrop model
crop <- ecocropPars("maize")
m <- ecocrop(crop)
control(m, get_max=TRUE)

# run the model, make sure to match 
# the predictor and parameter names 
mz <- runSpatial(m, tavg=ta, prec=pr, ph=ph, wopt=list(names="maize"))
plot(mz)

control(m, get_max=FALSE)
mz12 <- runSpatial(m, tavg=ta, prec=pr, ph=ph)
plot(mz12[[7:8]])

control(m, lim_fact=TRUE)
mzlim <- runSpatial(m, tavg=ta, prec=pr+50, ph=ph)
plot(mzlim[[7]])

control(m, lim_fact=FALSE)

# make the soil more acidic
ph2 <- ph - 1.25 
control(m, get_max=TRUE)
mzph <- runSpatial(m, tavg=ta, prec=pr, ph=ph2)
plot(mzph)

# more rainfall
control(m, get_max=TRUE)
pr2 <- pr + 30 # for each month
mzpr <- runSpatial(m, tavg=ta, prec=pr2, ph=ph)
plot(mzpr)

s <- c(mz, mzph, mzpr)
names(s) <- c("base", "ph", "prec")
plot(s)

## another crop
crop <- ecocropPars("pearl millet")
m <- ecocrop(crop)
control(m, get_max=TRUE)
pm <- runSpatial(m, prec=pr, tavg=ta, wopt=list(names="Pearl millet"))

crops <- c(mz, pm)
plot(crops)
}

