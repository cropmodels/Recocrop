\name{ecocrop_model}

\alias{Rcpp_EcocropModel-class}
\alias{show,Rcpp_EcocropModel-method}

\alias{ecocrop}
\alias{ecocrop,Rcpp_EcocropModel-method}

\alias{control}
\alias{control,Rcpp_EcocropModel-method}

\alias{run}
\alias{run,Rcpp_EcocropModel-method}


\title{
EcoCrop model
}

\description{

Create and run an EcoCrop model to asses the environmental suitability of a location for a (plant) species. 

First create a model object with the \code{ecocrop} method. Then set parameters describing the environmental requirements of a species or other taxon. The \code{\link{ecocropPars}} method provides default parameters for 1710 taxa.

Next, provide environmental data with the \code{\link[=staticPredictors<-]{staticPredictors}} and/or \code{\link[=dynamicPredictors<-]{dynamicPredictors}} method. Static predictors, such as soil pH, do not change throughout the year. In contrast, dynamic predictors, such as temperature and rainfall, vary over time. In the current implementation the time step is months. Therefore, dynamic variables must have 12 values, one for much month of the year, or multiples of 12 values, to represent multiple years or locations. 

The names of the predictors much match the names in the parameters, but not vice versa. That is, parameters that are not matched by a predictor are ignored.

The main purpose of implementing the model is to support making spatial predictions with \code{\link{predict}}.
}


\usage{
ecocrop(crop)
\S4method{control}{Rcpp_EcocropModel}(x, get_max=FALSE, which_max=FALSE, count_max=FALSE, lim_fact=FALSE, ...)
\S4method{run}{Rcpp_EcocropModel}(x, ...)
}

\arguments{
 \item{crop}{list with ecocrop parameters. See \code{link[ecocropPars]} and \code{link[crop]}}

 \item{x}{EcocropModel object}

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
The model computes a score for each variable for the 1st and 15th day of each month. It then takes the lowest (most limiting) score for each time period. After that, the minimum score for the time periods that follow (the growing season) is computed. The lenght of the growing season is by the \code{duration} parameter (see \code{\link{ecocropPars}}). 

You can set the output variables with \code{options}. If all options are \code{FALSE}, the 24 bi-monthly scores are returned.
}

\examples{
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
t(dp)
dynamicPredictors(m) <- dp

#staticPredictors(m) <- cbind(clay=12)

# run model
x <- run(m)
x

y <- matrix(round(x, 1), nrow=2)
colnames(y) <- month.abb
rownames(y) <- c("day1", "day15")
y

dates <- as.Date(paste0("2000-", rep(1:12, each=2), "-", rep(c(1,15), 12)))
plot(dates, x, las=1, ylab="suitability", xlab="")
lines(dates, x, col="red")


control(m, get_max=TRUE)
run(m)
}
