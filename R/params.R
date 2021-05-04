
ecocropPars <- function(name, ...) {
	fname <- system.file("parameters/ecocrop.rds", package="Recocrop")
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
		print("multiple matches found, use scientific name\n")
		d <- d[i, c(1,3)]
		rownames(d) <- NULL
		print(d)
	}

	if (length(i)==0) {
		a1 <- agrep(nam, d1)
		a2 <- agrep(nam, d2)
		a3 <- agrep(nam, d3)
		a <- unique(c(a1, a2, a3))
		print(paste0('"', name, '" not found. \n'))
		if (length(a) > 0) {
			print("Did you mean one of these?\n")
			d <- d[a, c(1,3)]
			rownames(d) <- NULL
			print(d)
		} 
	}
	
	if (length(i)==1) {
		f <- d[i,]
		nms <- paste(f[1], "; ", f[3], paste0(" (", f[2], ")"), sep="") 
		
		duration <- f$GMIN + min(30, f$GMAX - f$GMIN, na.rm=TRUE)
		duration <- 15 * round(duration / 15)
		#nmonths <- round(duration / 30)
	
		ktmp <- c(f$KTMP-1, f$KTMP+1, Inf, Inf)
		tavg <- c(f$TMIN, f$TOPMN, f$TOPMX, f$TMAX)		
		prec <- c(f$RMIN, f$ROPMN, f$ROPMX, f$RMAX)
		diva <- (f$GMAX + f$GMIN) / 2
		if (f$GMAX > f$GMIN) {
			div1 <- (diva + 30)
			div2 <- (diva - 30)
		} else {
			div1 <- div2 <- diva
		}
		rdiv <- c(div1, mean(c(diva, div1)), mean(c(diva, div2)), div2) / 30
		prec <- round(prec / rdiv)
		ph <- c(f$PHMIN, f$PHOPMN, f$PHOPMX, f$PHMAX)
		
		list(
			name=nms, 
			parameters=cbind(duration=c(duration, NA, f$GMIN, f$GMAX), 
			ktmp=ktmp, tavg=tavg, prec=prec, ph=ph)
		)
	}
}
