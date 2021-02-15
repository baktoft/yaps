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
	
	# check correct dimensions...
	stopifnot(ncol(inp$datTmb$toa) == inp$datTmb$np)
	stopifnot(nrow(inp$datTmb$toa) == inp$datTmb$nh)
	stopifnot(dim(inp$datTmb$H)[2] == 3)
	
	# check for correct number of lower and upper bounds
	stopifnot(nrow(inp$bounds) == length(inp$inits))
	
	# check that neither first nor last row in toa is all NAs
	stopifnot(sum(!is.na(inp$datTmb$toa[,1])) > 0)
	stopifnot(sum(!is.na(inp$datTmb$toa[,ncol(inp$datTmb$toa)])) > 0)
	
	# if z_vec != NULL 
	if(!inp$datTmb$how_3d %in% c('none', 'est')){
		stopifnot(length(inp$datTmb$z_vec) == inp$datTmb$np)
	}
	
	print("Pre-flight checkInp() passed!")

}