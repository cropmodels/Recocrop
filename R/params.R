
ecocropPars <- function(name, ...) {
	fname <- system.file("parameters/ecocrop.rds", package="ecocrop")
	d <- readRDS(fname)
	d$NAME = gsub("\\*", "", d$NAME)
	d$NAME <- trimws(d$NAME)

	if (missing(name)) {
		return(d[,c(1,3)])	
	}
	nam <- tolower(trimws(name))
	if (nchar(nam) < 3) {
		stop("provide a longer name")
	}
	d1 <- tolower(d[,1])
	d2 <- tolower(d[,3])
	
	i <- which(d1==nam)
	if (length(i)==0) i <- which(d2==nam)
    if (length(i)==0) {
		d3 <- trimws(sapply(strsplit(d2, " "), function(i) paste(i[1], i[2], collapse=" ")))
		i <- which(d3==nam)
	}
	
	if (length(i) > 1) {
		cat("multiple matches found, use scientific name\n")
		d <- d[i, c(1,3)]
		rownames(d) <- NULL
		print(d)
	}

	if (length(i)==0) {
		a1 <- agrep(nam, d1)
		a2 <- agrep(nam, d2)
		a <- c(a1, a2)
		cat(paste0('"', name, '" not found. \n'))
		if (length(a) > 0) {
			cat("Did you mean one of these?\n")
			d <- d[a, c(1,3)]
			rownames(d) <- NULL
			print(d)
		} 
	}
	
	if (length(i)==1) {
		f <- d[i,]
		x <- as.list(f[1:3])
		x$duration <- floor((f$GMIN + f$GMAX) / 61)
		x$ktmp <- c(f$KTMP-1, f$KTMP+1, Inf, Inf)
		x$tavg <- c(f$TMIN, f$TOPMN, f$TOPMX, f$TMAX)
		
		prec <- c(f$RMIN, f$ROPMN, f$ROPMX, f$RMAX)
		x$prec <- prec / min(12, x$duration + 1)
		x$ph <-  c(f$PHMIN, f$PHOPMN, f$PHOPMX, f$PHMAX)
		return(x)
	}
}


#str(ecocropParas("potato"))
