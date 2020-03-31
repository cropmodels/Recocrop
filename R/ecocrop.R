
if (!isGeneric("run")) { setGeneric("run", function(x, ...) standardGeneric("run")) }	

if (!isGeneric("dynamicPredictors<-")) { setGeneric("dynamicPredictors<-", function(x, value) standardGeneric("dynamicPredictors<-")) }	
if (!isGeneric("staticPredictors<-")) { setGeneric("staticPredictors<-", function(x, value) standardGeneric("staticPredictors<-")) }	

if (!isGeneric("parameters<-")) { setGeneric("parameters<-", function(x, value) standardGeneric("parameters<-")) }	
if (!isGeneric("options<-")) { setGeneric("options<-", function(x, value) standardGeneric("options<-")) }	


.hasError <- function(x, method) {
	if (x$hasError) {
		x$hasError = FALSE
		msg <- paste(x$messages, collapse="\n")
		x$messages <- ""
		stop(paste("Error in: ", method, "\n", msg))
	}
}


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
		.hasError(x, "run")
		x$out
	}
)

removeParameters <- function(x, name) {
	x$removeParameter(name)
	invisible(x)
}


setMethod("options<-", signature("Rcpp_EcocropModel", "list"), 
	function(x, value) {
		nms <- tolower(trimws(names(value)))
		i <- match(c("max"), nms)
		if (length(i) > 0) {
			value <- value[i]
			nms <- nms[i]
			for (i in 1:length(nms)) {
				if (isTRUE(nms[i] == "max")) x$max <- value[[i]]
			}
		} else {
			warning("no valid option names found")
		}
		x
	}
)



setMethod("parameters<-", signature("Rcpp_EcocropModel", "matrix"), 
	function(x, value) {
		stopifnot(nrow(value) == 4) 
		nms <- colnames(value)
		if (any(nms=="")) stop("all columns must have names")
		for (i in 1:ncol(value)) {
			x$setParameter(nms[i], value[,i])
			.hasError(x, "parameters")
		}
		x
	}
)


setMethod("dynamicPredictors<-", signature("Rcpp_EcocropModel", "matrix"), 
	function(x, value) {
		stopifnot(nrow(value) %% 12 == 0) 

		nms <- trimws(colnames(value))
		if (any(nms=="")) stop("all columns must have names")
		for (i in 1:ncol(value)) {
			x$setPredictor(nms[i], value[,i], TRUE)
			.hasError(x, "dynamicPredictors")
		}
		x
	}
)


setMethod("staticPredictors<-", signature("Rcpp_EcocropModel", "matrix"), 
	function(x, value) {
		nms <- colnames(value)
		if (any(nms=="")) stop("all columns must have names")
		for (i in 1:ncol(value)) {
			x$setPredictor(nms[i], value[,i], FALSE)
			.hasError(x, "staticPredictors")
		}
		x
	}
)

removePredictor <- function(x, name) {
	x$removePredictor(name)
	invisible(x)
}


setMethod ("show" , "Rcpp_EcocropModel", 
	function(object) {
		utils::str(object)
	}
)	


setMethod ("plot" , "Rcpp_EcocropModel", 
	function(x, ...) {
		pars <- x$parameters
		nms <- x$parameter_names
		n <- length(pars)
		nc <- ceiling(sqrt(n))
		nr <- ceiling(n / nc)

		old.par <- graphics::par(no.readonly = TRUE) 
		on.exit(graphics::par(old.par))
		graphics::par(mfrow=c(nr, nc), mar=c(2, 3, 2, 1))

		for (i in 1:n) {
			p <- pars[[i]]
			if (!is.finite(p[2])) {
				p[1] <- p[2] <- p[3] - 2*(p[4]-p[3])
			} else if (!is.finite(p[3])) {
				p[4] <- p[3] <- p[2] + 2*(p[2]-p[1])
			}
			d <- (p[4] - p[1]) / 4
			p <- c(p[1]-d, p, p[4]+d)
			xy <- cbind(p, c(0,0,1,1,0,0))
			plot(xy, ylab="suitability", las=1, type="l", lwd=2, col="red", axes=FALSE)
			points(xy[2:5,])
			graphics::axis(1, pos=-0.05)
			graphics::axis(2, las=2)
			graphics::title(nms[i])
		}
	}
)	

