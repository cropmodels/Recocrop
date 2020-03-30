\name{add-remove}

\alias{addPars}
\alias{removePars}

\alias{addPred}
\alias{removePred}

\title{
Add or remove parameters and predictors
}

\description{
Add parameters or predictors to an EcoCrop model, or remove them
}

\usage{
addPars(x, name, pars)
addPred(x, name, pred)
removePars(x, name)
removePred(x, name)
}

\arguments{
\item{x}{EcocropModel object}
\item{name}{character. name of the variable}
\item{pars}{numeric vector. Must be four number is ascending order}
\item{pred}{numeric vector. Must be 1 or 12 (one for each month) numbers}
}

\value{
EcocropModel
}


\examples{
potato <- ecocropPars("potato")
m <- ecocrop(potato)
m <- addPars(m, "clay", c(0, 10, 30, 40))

predictors(m) <- cbind(tavg=rep(15,12), prec=rep(100, 12))
m <- addPred(m, "ph", 5)
m <- addPred(m, "clay", 35)

run(m)
}

