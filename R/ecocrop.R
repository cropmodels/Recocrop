
if (!isGeneric("run")) { setGeneric("run", function(x, ...) standardGeneric("run")) }	
if (!isGeneric("predict")) { setGeneric("predict", function(x, ...) standardGeneric("predict")) }	

if (!isGeneric("dynamicPredictors<-")) { setGeneric("dynamicPredictors<-", function(x, value) standardGeneric("dynamicPredictors<-")) }	
if (!isGeneric("staticPredictors<-")) { setGeneric("staticPredictors<-", function(x, value) standardGeneric("staticPredictors<-")) }	

if (!isGeneric("crop<-")) { setGeneric("crop<-", function(x, value) standardGeneric("crop<-")) }	
#if (!isGeneric("parameters<-")) { setGeneric("parameters<-", function(x, value) standardGeneric("parameters<-")) }	
#if (!isGeneric("options<-")) { setGeneric("options<-", function(x, value) standardGeneric("options<-")) }	

if (!isGeneric("control")) { setGeneric("control", function(x, ...) standardGeneric("control")) }	


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
	if (!missing(crop)) {
		if (is.list(crop)) {
			crop <- crop$parameters
		}
		crop(m) <- crop
	}
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
}


# setMethod("options<-", signature("Rcpp_EcocropModel", "list"), 
	# function(x, value) {
		# nms <- tolower(trimws(names(value)))
		# i <- match(c("get_max", "which_max", "count_max", "lim_fact"), nms)
		# if (length(i) > 0) {
			# value <- value[i]
			# nms <- nms[i]
			# for (i in 1:length(nms)) {
				# if (isTRUE(nms[i] == "get_max")) { x$get_max <- value[[i]]
				# } else if (isTRUE(nms[i] == "which_max")) { x$which_max <- value[[i]]
				# } else if (isTRUE(nms[i] == "count_max")){ x$count_max <- value[[i]]
				# } else if (isTRUE(nms[i] == "lim_fact")){ x$lim_fact <- value[[i]] }
			# }
		# } else {
			# warning("no valid option names found")
		# }
		# x
	# }
# )


setMethod("control", signature("Rcpp_EcocropModel"), 
	function(x, get_max=FALSE, which_max=FALSE, count_max=FALSE, lim_fact=FALSE, ...) {
		if (lim_fact) {
			if (any(c(get_max, which_max, count_max))) {
				warning("if lim_fact=TRUE the *_max options are considered to be FALSE")
			}
		}
		x$lim_fact <- lim_fact
		x$which_max <- which_max
		x$get_max <- get_max
		x$count_max <- count_max
	}
)


setMethod("crop<-", signature("Rcpp_EcocropModel", "matrix"), 
	function(x, value) {
		stopifnot(nrow(value) == 4) 
		nms <- colnames(value)
		if (any(nms=="")) stop("all columns must have names")
		for (i in 1:ncol(value)) {
			if (nms[i] == "duration") {
				x$duration <- value[1,i]
			} else {
				x$setParameter(nms[i], value[,i])
			}
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
}


setMethod ("show" , "Rcpp_EcocropModel", 
	function(object) {
		utils::str(object)
	}
)	

