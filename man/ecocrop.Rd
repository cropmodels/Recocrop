\name{ecocrop}

\alias{Rcpp_EcocropModel-class}
\alias{show,Rcpp_EcocropModel-method}
\alias{plot,Rcpp_EcocropModel,ANY-method}

\alias{ecocrop}
\alias{ecocrop,Rcpp_EcocropModel-method}

\alias{crop<-}
\alias{crop<-,Rcpp_EcocropModel,matrix-method}

\alias{control}
\alias{control,Rcpp_EcocropModel-method}

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

The main purpose of implementing the model is to support making spatial predictions with \code{\link{predict}}.
}


\usage{
ecocrop(crop)
crop(x) <- value
dynamicPredictors(x) <- value
staticPredictors(x) <- value
\S4method{control}{Rcpp_EcocropModel}(x, get_max=FALSE, which_max=FALSE, count_max=FALSE, lim_fact=FALSE, ...)
run(x, ...)
}

\arguments{
 \item{crop}{list with ecocrop parameters. See \code{link[ecocropPars]}}

 \item{x}{EcocropModel object}

 \item{value}{
 
 \code{crop} method: matrix with crop parameters. Each column represents parameters for an environmental variable, and must have a name. The matrix must have four rows that represent the x-coordinates for a table-mountain shaped relative response function (y-coordinates: 0,1,1,0; see the plot in the examples). The only exception is the "duration" parameter with has a single value (in the first row). For functions where only "one tail" is relevant, the values for the other tail can be set to very high or low values, including Inf and -Inf.

 \code{predictors} method: matrix with environmental predictor variables. Each column represents an environmental variable, and must have a name that matches a predictor variable. For \code{dynamicPredictors}, the matrix must have 12 rows, or a multiple of 12 rows. For \code{staticPredictors}, the matrix can have any number of rows. The number of rows in \code{staticPredictors} must be 1/12 of the number of rows in the \code{dynamicPredictors}.
 
 Parameters and predictors can be added sequentially; but the length of the predictor variables must match previously supplied data. If the parameter or predictor name already exists, the old values are overwritten. You can include any variable of interest, as long as you have matching names for parameters and predictor variables. You can also \code{\link{removeParameters}} or variables.
 }

 \item{get_max}{logical. If \code{TRUE}, the maximum value (across the 12 months of the year) is returned.}

 \item{which_max}{logical. If \code{TRUE}, the first month with the maximum value is returned.}

 \item{count_max}{logical. If \code{TRUE}, the number of months with the maximum value is returned.}
  
 \item{lim_fact}{logical. If \code{TRUE}, the options above are ignored, the most-limiting factor for each month (or the one that is reached first if there are ties) is also returned.}
  
 \item{...}{additional arguments. None implemented}
}


\value{
vector
}


\details{
The model computes a score for each variable in each month. It then takes the lowest (most limiting) score for each month. Then it takes the lowest score for each month and a number of months that follow (the growing season). This number of months is set by the \code{duration} parameter (see \code{\link{ecocropPars}}). 

You can set the output variables with \code{options}. If all options are \code{FALSE}, the 12 monthly scores are returned.
}

\examples{

library(ecocrop)
# Get parameters
potato <- ecocropPars("potato")

# create a model
m <- ecocrop(potato)

# add parameters
crop(m) <- cbind(clay=c(0,0,10,20))

# inspect 
plot(m)

# add predictors
dp <- cbind(tavg=c(10,12,14,16,18,20,22,20,18,16,14,12), prec=seq(50,182,12))
dp
dynamicPredictors(m) <- dp

staticPredictors(m) <- cbind(clay=12)

# run model
run(m)

control(m, get_max=TRUE)
run(m)
}
