\name{predict}

\alias{predict}
\alias{predict,Rcpp_EcocropModel-method}


\title{
Spatial EcoCrop model predictions
}

\description{
Make spatial predictions with an EcoCrop model. First create a model, then provide that to the predict methods, together with SpatRaster objects as predictors.
}


\usage{
\S4method{predict}{Rcpp_EcocropModel}(object, ..., filename="", overwrite=FALSE, wopt=list())
}


\arguments{
  \item{object}{EcocropModel}
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
fta <- system.file("ex/ta.tif", package="Recocrop")
fpr <- system.file("ex/pr.tif", package="Recocrop")
fph <- system.file("ex/ph.tif", package="Recocrop")

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
mz <- predict(m, tavg=ta, prec=pr, ph=ph, wopt=list(names="maize"))
plot(mz)

# output by 15 day period
control(m, get_max=FALSE)
mz12 <- predict(m, tavg=ta, prec=pr, ph=ph)
plot(mz12[[14:15]])

control(m, lim_fact=TRUE)
mzlim <- predict(m, tavg=ta, prec=pr+50, ph=ph)
plot(mzlim[[14]])

control(m, get_max=TRUE)
# make the soil more acidic
ph2 <- ph - 1.25 
control(m, get_max=TRUE)
mzph <- predict(m, tavg=ta, prec=pr, ph=ph2)
plot(mzph)

# more rainfall
control(m, get_max=TRUE)
pr2 <- pr + 30 # for each month
mzpr <- predict(m, tavg=ta, prec=pr2, ph=ph)
plot(mzpr)

s <- c(mz, mzph, mzpr)
names(s) <- c("base", "ph", "prec")
plot(s)

## another crop
crop <- ecocropPars("pearl millet")
m <- ecocrop(crop)
control(m, get_max=TRUE)
pm <- predict(m, prec=pr, tavg=ta, wopt=list(names="Pearl millet"))

crops <- c(mz, pm)
plot(crops)
}

