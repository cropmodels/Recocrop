

setMethod("predict", signature("Rcpp_EcocropModel"), 
function(object, ..., filename="", overwrite=FALSE, wopt=list())  {

	preds <- list(...)
	if (length(preds) == 0) {
		return(run(object))
	}
	nms <- trimws(names(preds))
	if (is.null(nms) | any(nms=="")) stop ("SpatRaster arguments must be named")
	# check if nms in model
	
	if (length(nms) > 1) {
		sapply(2:length(nms), function(i)compareGeom(preds[[1]], preds[[i]], lyrs=FALSE)) 
	}
	
	nlyrs <- sapply(preds, terra::nlyr)
	if (!all(nlyrs %in% c(1, 12))) stop("all SpatRaster objects must have either 1 or 12 layers")
	di <- nlyrs == 12
	dpreds <- preds[di]
	spreds <- preds[!di]

	# use collection?
	#e <- sapply(preds, function(i) as.vector(terra::ext(i)))
	#if (!all(apply(e == e[,1], 2, all))) {
	#	stop("extents are not equal")
	#}
	#r <- sapply(preds, terra::res)
	#if (!(all(apply(r == r[,1], 1, all)))) {
	#	stop("resolutions are not equal")	
	#}
	# just for checking extent/resolution
	
	out <- rast(preds[[1]])
	nms <- object$names()
	nlyr(out) <- length(nms)
	if (length(wopt$names) != length(nms)) {
		names(out) <- nms
		wopt$names <- NULL
	}
	## depends on future version of terra
	#if (object$lim_fact) {
    #	levels(out) <- object$parameter_names
	#}
	nc <- ncol(out)
	lapply(preds, terra::readStart)
	
	b <- terra::writeStart(out, filename, overwrite, wopt=wopt)
	for (i in 1:b$n) {
		removePredictor(object, "ALL")
		
		if (length(dpreds) > 0) {
			v <- lapply(dpreds, function(r) {
				as.vector(t(terra::readValues(r, b$row[i], b$nrows[i], 1, nc, TRUE)))
			})
			vv <- do.call(cbind, v)
			names(vv) <- nms[di]
			dynamicPredictors(object) <- vv
		}
		if (length(spreds) > 0) {
			v <- lapply(spreds, function(r) {
				as.vector(t(terra::readValues(r, b$row[i], b$nrows[i], 1, nc, TRUE)))
			})
			vv <- do.call(cbind, v)
			names(vv) <- nms[!di]
			staticPredictors(object) <- vv
		}
		
		eco <- run(object)
		removePredictor(object, "ALL")
		if (nlyr(out) > 1) {
			eco <- as.vector(matrix(eco, ncol=nlyr(out), byrow=TRUE))
		}
		terra::writeValues(out, eco, b$row[i], b$nrows[i])
	}
	object$out <- 0
	lapply(preds, terra::readStop)
	out <- terra::writeStop(out)
	return(out)
}
)

