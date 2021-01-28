#' Check consistency of `inp` object obtained from `getInp()`
#' 
#' @param inp Object obtained using `getInp()`
#' @export
#' @example man/examples/example-yaps_ssu1.R
checkInp <- function(inp){
	
	stopifnot(ncol(inp$datTmb$toa) == inp$datTmb$np)
	stopifnot(nrow(inp$datTmb$toa) == inp$datTmb$nh)
	
	# if z_vec != NULL 
	if(inp$datTmb$how_3d != 'none'){
		stopifnot(dim(inp$datTmb$H)[2] == 3)
		stopifnot(length(inp$datTmb$z_vec) == inp$datTmb$np)
	}
	
	print("checkInp passed!")

}