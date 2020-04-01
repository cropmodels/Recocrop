\name{ecocrop}

\alias{Rcpp_EcocropModel-class}
\alias{show,Rcpp_EcocropModel-method}
\alias{plot,Rcpp_EcocropModel,ANY-method}

\alias{ecocrop}
\alias{ecocrop,Rcpp_EcocropModel-method}

\alias{parameters<-}
\alias{parameters<-,Rcpp_EcocropModel,matrix-method}

\alias{options<-}
\alias{options<-,Rcpp_EcocropModel,list-method}

\alias{dynamicPredictors<-}
\alias{dynamicPredictors<-,Rcpp_EcocropModel,matrix-method}

\alias{staticPredictors<-}
\alias{staticPredictors<-,Rcpp_EcocropModel,matrix-method}

\alias{run}
\alias{run,Rcpp_EcocropModel-method}


\title{
EcoCrop model
}

\description{

Create and run an EcoCrop model to asses the environmental suitability of a location for a (plant) species. 

First create a model object with the \code{ecocrop} method. Then set parameters describing the environmental requirements of a species or other taxon. The \code{\link{ecocropPars}} method provides default parameters for 1710 taxa.

Next, provide environmental data with the \code{staticPredictors} and/or \code{dynamicPredictors} method. Static predictors, such as soil pH, do not change throughout the year. In contrast, dynamic predictors, such as temperature and rainfall, vary over time. In the current implementation the time step is months. Therefore, dynamic variables must have 12 values, one for much month of the year, or multiples of 12 values, to represent multiple years or locations. 

The names of the predictors much match the names in the parameters, but not vice versa. That is, parameters that are not matched by a predictor are ignored.

The main purpose of implementing the model is to do spatial predictions (see \code{link{runSpatial}}).
}


\usage{
ecocrop(crop)
parameters(x) <- value
dynamicPredictors(x) <- value
staticPredictors(x) <- value
options(x) <- value 
run(x, ...)
}

\arguments{
 \item{crop}{list with ecocrop parameters. See \code{link[ecocropPars]}} 

 \item{x}{EcocropModel object}

 \item{value}{
 
 \code{parameters}: matrix with parameters. Each column represents parameters for an environmental variable, and must have a name. The matrix must have four rows that form a table-mountain shaped response function (see the plot in the examples). 

 \code{predictors}: matrix with environmental predictor variables. Each column represents an environmental variable, and must have a name that matches a predictor variable. For \code{dynamicPredictors}, the matrix must have 12 rows, or a multiple of 12 rows. For \code{staticPredictors}, the matrix can have any number of rows. The number of rows in \code{staticPredictors} must be 1/12 of the number of rows in the \code{dynamicPredictors}. 
 
 Parameters and predictors can be added sequentially; but the length of the predictor variables must match previously supplied data. If the parameter or predictor name already exists, the old values are overwritten. You can include any variable of interest, as long as you have matching names for parameters and predictor variables. You can also \code{\link{removeParameters}} or variables.
  
 \code{options}: named list with options. Thus far: \code{max}: if \code{TRUE}, the maximum value (across the 12 months of the year) is returned. Otherwise all monthly values are returned.
 }
  
 \item{...}{additional arguments. None implemented}
}


\value{
vector
}


\details{
The model computes a score for each variable in each month. It then takes the lowest (most limiting) score for each month. Then it takes the lowest score for each month and a number of months that follow (the growing season). This number of months is set by the \code{duration} parameter. 
}

\examples{

# Get parameters
potato <- ecocropPars("potato")

# create a model
m <- ecocrop(potato)

# add parameters
parameters(m) <- cbind(clay=c(0,0,10,20))

# inspect 
plot(m)

# add predictors
dp <- cbind(tavg=c(10,12,14,16,18,20,22,20,18,16,14,12), prec=seq(50,182,12))
dp
dynamicPredictors(m) <- dp

staticPredictors(m) <- cbind(clay=12)

# run model
run(m)

options(m) <- list(max=TRUE)
run(m)
}
