
if (!isGeneric("runSpatial")) { setGeneric("runSpatial", function(x, ...) standardGeneric("runSpatial")) }	

setMethod("runSpatial", signature(x="Rcpp_EcocropModel"), 
function(x, ..., filename="", overwrite=FALSE, wopt=list())  {

	preds <- list(...)
	if (length(preds) == 0) stop("no predictor variables")
	
	e <- sapply(preds, function(i) as.vector(terra::ext(i)))
	if (!all(apply(e == e[,1], 1, all))) {
		stop("extents are not equal")
	}
	r <- sapply(preds, terra::res)
	if (!(all(apply(r == r[,1], 1, all)))) {
		stop("resolutions are not equal")	
	}

	nms <- trimws(names(preds))
	if (is.null(nms) | any(nms=="")) stop ("SpatRaster arguments must be named")
	# check if nms in model

	nlyrs <- sapply(preds, terra::nlyr)
	if (!all(nlyrs %in% c(1, 12))) stop("all SpatRaster objects must have either 1 or 12 layers")
	di <- nlyrs == 12
	dpreds <- preds[di]
	spreds <- preds[!di]
		
	out <- rast(preds[[1]])
	nlyr(out) <- ifelse(x$max, 1, 12)
	nc <- ncol(out)
	lapply(preds, terra::readStart)
	
	b <- terra::writeStart(out, filename, overwrite, wopt)
	for (i in 1:b$n) {
		removePredictor(x, "ALL")
		
		if (length(dpreds) > 0) {
			v <- lapply(dpreds, function(r) {
				as.vector(t(terra::readValues(r, b$row[i], b$nrows[i], 1, nc, TRUE)))
			})
			vv <- do.call(cbind, v)
			names(vv) <- nms[di]
			dynamicPredictors(x) <- vv
		}
		if (length(spreds) > 0) {
			v <- lapply(spreds, function(r) {
				as.vector(t(terra::readValues(r, b$row[i], b$nrows[i], 1, nc, TRUE)))
			})
			vv <- do.call(cbind, v)
			names(vv) <- nms[!di]
			staticPredictors(x) <- vv
		}
		
		eco <- run(x)
		removePredictor(x, "ALL")
		if (nlyr(out) > 1) {
			eco <- as.vector(matrix(eco, ncol=nlyr(out), byrow=TRUE))
		}
		terra::writeValues(out, eco, c(b$row[i], b$nrows[i]))
	}
	lapply(preds, terra::readStop)
	out <- terra::writeStop(out)
	return(out)
}
)


