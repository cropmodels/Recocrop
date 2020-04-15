\name{ecocrop-package}

\alias{ecocrop-package}

\docType{package}

\title{
\packageTitle{ecocrop}
}

\description{

EcoCrop is a simple limiting-factor based model of plant adaptation to environmental conditions. The *model* was first implemented in DIVA-GIS 5.1 (Hijmans et al., 2005) and later in the R dismo package (Hijmans et al.) and elsewhere. The modeling approach is derived from the 'Plantgro' approach (Hackett, 1991). Plateau-shaped curves are used to quantify plant response (expressed as 0-1) to the environment. The environment can be represented by "dynamic" variables that vary with time, such as precipitation and temperature and "static" variables such as soil properties. The response is zero below a minimum and above a maximum threshold, and one between a minimum and maximum optimal value, and linearly interpolated in between these thresholds. Overall response across environmental variables follows the Sprengel-Liebig Law of the Minimum (Van der Ploeg et al., 1999), that is, the most limiting factor is used, and interactions are not considered. Model scores are calculated for 24 possible growing periods, starting at the first or the 15th day of each month, the highest score is retained (to be implemented, currently only 12 periods are considered). 

Default *parameters* are taken from the FAO EcoCrop database at \url{http://ecocrop.fao.org/ecocrop/srv/en/home}. 


Hackett, C, 1991. Mobilising environmental information about lesser-known plants: the value of two neglected levels of description. Agroforestery Systems 14, 131-143. https://doi.org/10.1007/BF00045728
Hijmans RJ., L. Guarino, C Bussink, P Mathur, M Cruz, I Barrentes, E Rojas, 2005. DIVA-GIS. A geographic information system for the analysis of species distribution data. https://diva-gis.org
Hijmans, RJ, S Phillips, J Leathwick and J Elith, 2017. dismo: Species Distribution Modeling. R package version 1.1-4. https://CRAN.R-project.org/package=dismo
Van der Ploeg, RR, W Bohm, MB Kirkham, 1999. On the origin of the theory of mineral nutrition of plants and the law of the minimum. Soil Science Society of America Journal 63: 1055-1062
}

