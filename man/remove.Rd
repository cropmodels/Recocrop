\name{removeParameters}

\alias{removeParameters}
\alias{removePredictor}

\title{
Add or remove parameters and predictors
}

\description{
Remove parameters or predictors from an EcoCrop model
}

\usage{
removeParameters(x, name)
removePredictor(x, name)
}

\arguments{
\item{x}{EcocropModel object}
\item{name}{character. name of the variable to remove; or "ALL" to remove all parameters or predictors.}
}

\value{
Nothing is returned
}


\examples{
pot <- ecocropPars("potato")
m <- ecocrop(pot)

crop(m) <- cbind(clay = c(0, 10, 30, 40))
removeParameters(m, "ph")

dynamicPredictors(m) <- cbind(tavg=rep(15,12), prec=rep(100, 12))
removePredictor(m, "tavg")
}


