#' Check consistency of `inp` object obtained from `getInp()`
#' 
#' @param inp Object obtained using `getInp()`
#' @export
#' @return No return value, but prints errors/warnings if issues with `inp` is detected.
#' @example man/examples/example-yaps_ssu1.R
checkInp <- function(inp){

	# check that all BIs are in range of values in the model
	# only relevant for ping_types 'rbi' and 'pbi'?
	if(inp$datTmb$pingType != 'sbi'){
		stopifnot(inp$datTmb$rbi_min <= min(diff(inp$params$top)))
		stopifnot(inp$datTmb$rbi_max >= max(diff(inp$params$top)))
	}
	
	stopifnot(ncol(inp$datTmb$toa) == inp$datTmb$np)
	stopifnot(nrow(inp$datTmb$toa) == inp$datTmb$nh)
	
	stopifnot(dim(inp$datTmb$H)[2] == 3)
	
	# if z_vec != NULL 
	if(inp$datTmb$how_3d != 'none'){
		stopifnot(length(inp$datTmb$z_vec) == inp$datTmb$np)
	}
	
	print("Pre-flight checkInp() passed!")

}