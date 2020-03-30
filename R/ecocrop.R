
if (!isGeneric("run")) { setGeneric("run", function(x, ...) standardGeneric("run")) }	
if (!isGeneric("predictors<-")) { setGeneric("predictors<-", function(x, value) standardGeneric("predictors<-")) }	


ecocrop <- function(crop) {
	m <- EcocropModel$new()
	if (missing(crop)) return(m)
	if (!is.list(crop)) stop('argument "crop" must be a list')
	duration <- FALSE
	
	if (!is.null(crop$duration)) {
		m$duration <- crop$duration
		duration <- TRUE
	}
	
	i <- (sapply(crop, length) == 4)
	
	if (length(i) == 0) {
		if (duration) { 
			return (m)
		} else {
			stop("no parameters found")
		}
	}
	crop <- crop[i]
	i <- sapply(crop, is.numeric)
	
	if (length(i) == 0) stop("no numeric parameters found")
	crop <- crop[i]
	nms <- trimws(names(crop))
	if (any(nms == "")) stop("all elements must be named")
	for (i in 1:length(crop)) m$setParameter(nms[i], unlist(crop[i])) 
	m
}

setMethod("run", signature("Rcpp_EcocropModel"), 
	function(x, ...) {
		x$run()
		if (x$hasError) {
			stop(paste(x$messages, collapse="\n"))
		}
		x$out
	}
)


addPars <- function(x, name, pars) {
	x$setParameter(name, pars)
	invisible(x)
}

removePars <- function(x, name) {
	x$removeParameter(name)
	invisible(x)
}


setMethod("predictors<-", signature("Rcpp_EcocropModel", "matrix"), 
	function(x, value) {
		nms <- colnames(value)
		for (i in 1:ncol(value)) {
			x$setPredictor(nms[i], value[,i])
		}
		x
	}
)


addPred <- function(x, name, pred) {
	x$setPredictor(name, pred)
	invisible(x)
}

removePred <- function(x, name) {
	x$removePredictor(name)
	invisible(x)
}


setMethod ("show" , "Rcpp_EcocropModel", 
	function(object) {
		utils::str(object)
	}
)	



