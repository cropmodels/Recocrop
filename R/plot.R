
setMethod ("plot" , "Rcpp_EcocropModel", 
	function(x, nr, nc, col="red", ...) {
		pars <- x$parameters
		nms <- x$parameter_names

		n <- length(pars)
		if (missing(nc) & missing(nr)) {
			nc <- ceiling(sqrt(n))
			nr <- ceiling(n / nc)
		} else if (missing(nc)) {
			nc <- ceiling(n / nr)		
		} else if (missing(nr)) {
			nr <- ceiling(n / nc)
		}
		
		old.par <- graphics::par(no.readonly = TRUE) 
		on.exit(graphics::par(old.par))
		graphics::par(mfrow=c(nr, nc), mar=c(2.5, 3, 2.5, 1))

		for (i in 1:n) {
			p <- pars[[i]]
			y <- c(0,0,1,1,0,0)
			if (!is.finite(p[2])) {
				p[1] <- p[2] <- p[3] - 2*(p[4]-p[3])
				y <- c(1,1,1,1,0,0)
			} else if (!is.finite(p[3])) {
				p[4] <- p[3] <- p[2] + 2*(p[2]-p[1])
				y <- c(0,0,1,1,1,1)
			}
			d <- (p[4] - p[1]) / 4
			p <- c(p[1]-d, p, p[4]+d)
			xy <- cbind(p, y)
			plot(xy, ylab="suitability", las=1, type="l", lwd=2, col=col, yaxs="i", xaxs="i", bty="n")
			graphics::title(nms[i])
		}
	}
)	

