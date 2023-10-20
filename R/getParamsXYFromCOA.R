#' Internal function - get initial values for X and Y based on Center Of Activity - i.e. hydrophones positions
#'
#' Attempts to give meaningful initial values for X and Y based on which hydros detected each ping
#' @inheritParams getInp
#' @noRd
getParamsXYFromCOA <- function(datTmb){
	toa <- datTmb$toa
	hydros <- datTmb$H

	toa_detect <- toa
	toa_detect[!is.na(toa_detect)] <- 1

	X <- zoo::na.approx(colMeans((toa_detect) * hydros[,1], na.rm=TRUE))
	Y <- zoo::na.approx(colMeans((toa_detect) * hydros[,2], na.rm=TRUE))
	
	return(list(X=X, Y=Y))

}
