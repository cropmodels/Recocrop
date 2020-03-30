\name{ecocrop}


\alias{ecocropPars}

\alias{ecocrop}
\alias{ecocrop,Rcpp_EcocropModel-method}

\alias{run}
\alias{run,Rcpp_EcocropModel-method}

\alias{predictors<-}
\alias{predictors<-,Rcpp_EcocropModel,matrix-method}

\alias{[,Rcpp_EcocropModel,character,missing-method}
\alias{[,Rcpp_EcocropModel,character,character-method}
\alias{[<-,Rcpp_EcocropModel,character,missing-method}
\alias{[<-,Rcpp_EcocropModel,character,character-method}


\title{
EcoCrop model
}

\description{
Create a EcoCrop model, set parameters, and run it.
Default parameters are provided for 1710 taxa.
}


\usage{
ecocropPars(name)
ecocrop(crop)
predictors(x) <- value
run(x, ...)
}

\arguments{
 \item{name}{character. Common or scientific name of a plant (typically a crop plant). If missing, a data.frame with available names is returned} 
 \item{crop}{list with ecocrop parameters} 
 \item{x}{EcocropModel object}
 \item{value}{matrix with predictor variables}
 \item{...}{additional arguments. None implemented}
}


\details{

\tabular{lll}{
	\tab \bold{Input Parameters} \tab \bold{Explanation} \cr
}
}


\value{
vector with output variables as described in the Details
}


\examples{

# Get parameters
potato <- ecocropPars("potato")

# create a model
m <- ecocrop(potato)

# add predictors
predictors(m) <- cbind(tavg=rep(15,12), prec=rep(100, 12))

# run model
run(m)

}
