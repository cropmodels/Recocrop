\name{ecocropPars}


\alias{ecocropPars}


\title{
EcoCrop parameters
}

\description{
Default parameters are provided for 1710 taxa.
}


\usage{
ecocropPars(name, ...)
}

\arguments{
 \item{name}{character. Common or scientific name of a plant (typically a crop plant). If missing, a data.frame with available names is returned} 
 \item{...}{additional arguments. None implemented}
}


\details{

\tabular{lll}{
\tab \bold{Input Parameters} \tab \bold{Explanation} \cr
}
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

}
