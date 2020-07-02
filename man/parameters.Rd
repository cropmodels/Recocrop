\name{ecocropPars}
\alias{ecocropPars}

\title{
Crop parameters
}

\description{
Default ecocrop parameters for 1710 taxa. The values are from (or derived from) the \href{http://www.fao.org/land-water/land/land-governance/land-resources-planning-toolbox/category/details/en/c/1027491/}{ecocrop database} of the Food and Agricultural Organization (FAO) of the United Nations.

User beware: These parameters are expert opinion and not necessarily optimal. 
}

\usage{
ecocropPars(name, ...)
}

\details{
The parameters returned are returned in a matrix of four rows.

\code{duration}: this is the only parameter that is represented by a single number, the length of the growing season in days. This number has to be in the first row of the matrix. The other rows are ignored. The ecocrop model is very sensitive to this parameter. This number is computed by adding one month to the minimum length of the growing season (GMIN in the FAO database), unless the maximum length (GMAX) was the same as GMIN (for example in the case of a 12 month growing season). GMIN and GMAX are provided, for reference, in row 3 and 4. \code{duration} was computed this way, because the difference between GMIN and GMAX was sometimes very large, and because it is the shorter growing season that is more relevant for determining suitability. (If a long season is possible, a short season should also be possible, but not vice versa). For example, for maize, GMIN is 65 (extremely short) and GMAX is 365 (extremely long). The returned value is 3 (months). That is still short, and 4 months may be more realistic.  

\code{prec}: The minimum, lower optimum, higher optimum, and maximum threshold for monthly precipitation. The FAO database has these numbers for annual precipitation. The monthly numbers were derived by dividing the annual precipitation by the (growth duration (in months, see below) plus one).

\code{tavg}: The minimum, lower optimum, higher optimum, and maximum threshold for average temperature. 

\code{ktmp}: The minimum and lower optimum threshold for extreme minimum temperature (killing temperature). Only these two parameters are relevant (the temperature below which the plant dies, and above which there is no frost damage.) The were estimated from KTMP in the FAO database, as KTMP-1 and KTMP+1. The higher optimum, and maximum temperature are set to \code{Inf}.

If you wanted to consider damage from high temperatures, you would need to create a new parameter. These cannot be combined, as \code{ktmp} requires extreme minimum temperature data, whereas for heat damage you would need extreme maximum temperatures.

\code{ph}: The minimum, lower optimum, higher optimum, and maximum threshold for soil pH (presumably ph(H2O), that is, measured in water). 
}

\arguments{
 \item{name}{character. Common or scientific name of a plant (typically a crop plant). If missing, a data.frame with available names is returned} 
 \item{...}{additional arguments. None implemented}
}


\seealso{\code{\link{crop}}}

\value{
list
}


\examples{
potato <- ecocropPars("potato")
str(potato)

ecocropPars("patato")

p <- ecocropPars("Solanum tuberosum L.")
p

pp <- ecocropPars()
head(pp)

# to see the entire database, including parameters 
# that are not automatically extracted do
fname <- system.file("parameters/ecocrop.rds", package="Recocrop")
d <- readRDS(fname)
head(d)
}

