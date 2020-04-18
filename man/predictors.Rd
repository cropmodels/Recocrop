\name{predictors}

\alias{dynamicPredictors<-}
\alias{dynamicPredictors<-,Rcpp_EcocropModel,matrix-method}

\alias{staticPredictors<-}
\alias{staticPredictors<-,Rcpp_EcocropModel,matrix-method}

\title{
Environmental predictors
}

\description{

Set envoironmental predictors to an ecocrop model with the \code{staticPredictors} and/or \code{dynamicPredictors} method. Static predictors, such as soil pH, do not change throughout the year. In contrast, dynamic predictors, such as temperature and rainfall, vary over time. In the current implementation the time step is months. Therefore, dynamic variables must have 12 values, one for much month of the year, or multiples of 12 values, to represent multiple years or locations. 

The names of the predictors much match the names in the parameters, but not vice versa. That is, parameters that are not matched by a predictor are ignored.
}


\usage{
dynamicPredictors(x) <- value
staticPredictors(x) <- value
}

\arguments{
 \item{x}{EcocropModel object}

 \item{value}{
   matrix with environmental predictor variables. Each column represents an environmental variable, and must have a name that matches a predictor variable. For \code{dynamicPredictors}, the matrix must have 12 rows, or a multiple of 12 rows. For \code{staticPredictors}, the matrix can have any number of rows. The number of rows in \code{staticPredictors} must be 1/12 of the number of rows in the \code{dynamicPredictors}.
 }
}


\value{
None
}

\examples{
# Get parameters
potato <- ecocropPars("potato")
# create a model
m <- ecocrop(potato)

# add predictors
dp <- cbind(tavg=c(10,12,14,16,18,20,22,20,18,16,14,12), prec=seq(50,182,12))
t(dp)
dynamicPredictors(m) <- dp
staticPredictors(m) <- cbind(clay=12)
m
}
