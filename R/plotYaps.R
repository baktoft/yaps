#' Basic plots of yaps output
#' 
#' @param inp Input object obtained using \code{getInp()}
#' @param yaps_out Output from succesful run of \code{runYaps()}
#' @param type Plot type. \code{type="map"} prodces a basic map of estimated track and hydrophones; \code{type="coord_X"}, \code{type="coord_Y"} produces plots of X and Y coordinated including +- 1 standard error.
#' @export
plotYaps <- function(inp, yaps_out, type="map"){
	pl <- yaps_out$pl
	plsd <- yaps_out$plsd

	pl$X <- pl$X + inp$inp_params$Hx0
	pl$Y <- pl$Y + inp$inp_params$Hy0
	pl$top <- pl$top + inp$inp_params$T0
	
	hydros <- data.frame(hx=cbind(inp$datTmb$H[,1] + inp$inp_params$Hx0, hy=inp$datTmb$H[,2] + inp$inp_params$Hy0))
	
	if(type=="map"){
		plot(hy~hx, data=hydros, col="green", pch=20, cex=2, asp=1, xlab="UTM_X", ylab="UTM_Y")
		lines(pl$Y~pl$X, col="red")
	} else if(type=="coord_X"){
		plot(pl$top, pl$X, type="l", col="red", ylab="UTM_X", xlab="TimeOfPing")
		lines(pl$top, pl$X-plsd$X, col="red", lty=3)
		lines(pl$top, pl$X+plsd$X, col="red", lty=3)
	} else if(type == "coord_Y"){
		plot(pl$top, pl$Y, type="l", col="red", ylab="UTM_Y", xlab="TimeOfPing")
		lines(pl$top, pl$Y-plsd$Y, col="red", lty=3)
		lines(pl$top, pl$Y+plsd$Y, col="red", lty=3)
	}

}
