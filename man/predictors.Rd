\name{predictors}

\alias{dynamicPredictors<-}
\alias{dynamicPredictors<-,Rcpp_EcocropModel,matrix-method}

\alias{staticPredictors<-}
\alias{staticPredictors<-,Rcpp_EcocropModel,matrix-method}

\title{
Environmental predictors
}

\description{

Set environmental predictors to an ecocrop model. Use \code{staticPredictors} to set values for "static predictors", such as soil pH, that do not change throughout the year. 

Use \code{dynamicPredictors} to set dynamic predictors, such as temperature and rainfall, that vary throughout the year. The time step is months. Therefore, dynamic variables normally must have 12 values, one for much month of the year, or multiples of 12 values, to represent multiple years or locations. In this case the values are condsidered to represent climate, and time to be circular: that is, January follows December. If you want to supply weather data (monthly data for a particular year, you have to provide values for that year, and the next year --- that is, 24 values (and set parameter \code{nyears} to 2.

The 12 dynamic predictor values are interpolated to create 24 time periods for each year, centered around the 1st and 15 day (14th for February) of the month. For example, if the temperature in January is 10, and in Febrary it is 20, the value for January 15 is 10, for Feburary 1 (10+20)/2 = 15, and for February 15 it is 20. This approach works for average values, but not for totals. Precipitation (rainfall) is typically expressed as monthly totals; and if the numbers above were rainfall, the value for January 15 would be 10/2=5, for February 1 it would be (10+20)/4 = 7.5, and for March 15 it would be 20/2 = 10. 

It is thus important to recognize variables like this. The model keeps track of that based on the variable name. If the variable is "prec" or "rain" it is assumed to be a total, and otherwise it assumed to be an average.





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
