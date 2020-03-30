\name{remove}

\alias{removeParameters}
\alias{removePredictor}

\title{
Add or remove parameters and predictors
}

\description{
Add parameters or predictors to an EcoCrop model, or remove them
}

\usage{
removeParameters(x, name)
removePredictor(x, name)
}

\arguments{
\item{x}{EcocropModel object}
\item{name}{character. name of the variable}
}

\value{
EcocropModel
}


\examples{
potato <- ecocropPars("potato")
m <- ecocrop(potato)

parameters(m) <- cbind(clay = c(0, 10, 30, 40))
removeParameters(m, "ph")

dynamicPredictors(m) <- cbind(tavg=rep(15,12), prec=rep(100, 12))
removePredictor(m, "tavg")
}


