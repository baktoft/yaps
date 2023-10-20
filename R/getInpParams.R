#' Get parameters for this specific data set
#'
#' Compile a list of relevant parameters (e.g. T0) to use later on
#' @inheritParams getInp
#' @noRd
getInpParams <- function(hydros, toa){
	T0 <- min(toa, na.rm=TRUE)
		
	Hx0 <- hydros[1,h_x]
	Hy0 <- hydros[1,h_y]

	return(list(T0=T0, Hx0=Hx0, Hy0=Hy0))

}