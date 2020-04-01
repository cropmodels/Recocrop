\name{ecocropPars}
\alias{ecocropPars}

\title{
EcoCrop parameters
}

\description{
Default ecocrop parameters for 1710 taxa. The values are from (or derived from) the \url{http://www.fao.org/land-water/land/land-governance/land-resources-planning-toolbox/category/details/en/c/1027491/}{ecocrop database} of the United Nations Food and Agricultural Organization (FAO)

User beware. These parameters are expert opinion and sometimes wrong. 

The ecocrop model is very sensitive to the \code{duration} parameter. It expresses the growing season length in months. The defaults were computed by adding one month to the minimum length of the growing season (GMIN in the FAO database), unless the maximum length (GMAX) was the same as GMIN (for example in the case of a 12 month growing season). This was done this way, because the difference between GMIN and GMAX was sometimes very large, and because it is the shorter growing season that is more relevant for determining suitability. (If a long season is possible, a short season should also be possible, but not vice versa). For example, for maize, GMIN is 65 (extremely short) and GMAX is 365 (extremely long). The returned value is 3 (months). That is still short, and 4 months may be more realistic.  
}

\usage{
ecocropPars(name, ...)
}

\arguments{
 \item{name}{character. Common or scientific name of a plant (typically a crop plant). If missing, a data.frame with available names is returned} 
 \item{...}{additional arguments. None implemented}
}


\value{
list
}


\examples{
potato <- ecocropPars("potato")
str(potato)

ecocropPars("patato")

p <- ecocropPars("Solanum tuberosum L.")
str(p)

pp <- ecocropPars()
head(pp)

# to see the entire database, including parameters that 
# are not automatically extracted do
fname <- system.file("parameters/ecocrop.rds", package="ecocrop")
d <- readRDS(fname)

}
