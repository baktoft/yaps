#' Check consistency of `inp` object obtained from `getInp()`
#' 
#' @param inp Object obtained using `getInp()`
#' @export
#' @return No return value, but prints errors/warnings if issues with `inp` is detected.
#' @example man/examples/example-yaps_ssu1.R
checkInp <- function(inp){

	# check that all BIs are in range of values in the model
	# only relevant for ping_types 'rbi' and 'pbi'?
	if(inp$dat_tmb$ping_type != 'sbi'){
		if(inp$dat_tmb$rbi_min > min(diff(inp$params$TOP)))	{
			cat(paste0("ERROR: inp$dat_tmb$rbi_min > min(diff(inp$params$top)) | ",inp$dat_tmb$rbi_min," > ",min(diff(inp$params$TOP)),"\n"))
			# inp$dat_tmb$rbi_min <- min(diff(inp$params$top)) * 0.90
			# cat("...inp$dat_tmb$rbi_min adjusted to ", inp$dat_tmb$rbi_min, "\n")
			stopSilent()
		}

		if(inp$dat_tmb$rbi_max < max(diff(inp$params$TOP))){
			cat(paste0("ERROR: inp$dat_tmb$rbi_max < max(diff(inp$params$top)) | ",inp$dat_tmb$rbi_max," < ",max(diff(inp$params$TOP)),"\n"))
			# inp$dat_tmb$rbi_max <- max(diff(inp$params$top)) * 1.10
			# cat("...inp$dat_tmb$rbi_max adjusted to ", inp$dat_tmb$rbi_max, "\n")
			stopSilent()
		}
	}
	
	# check correct dimensions...
	stopifnot(nrow(inp$dat_tmb$toa) == inp$dat_tmb$np)
	stopifnot(ncol(inp$dat_tmb$toa) == inp$dat_tmb$nh)
	stopifnot(dim(inp$dat_tmb$H)[2] == 3)
	
	# check for correct number of lower and upper bounds
	stopifnot(nrow(inp$bounds) == length(inp$inits))
	
	# check that neither first nor last row in toa is all NAs
	stopifnot(sum(!is.na(inp$dat_tmb$toa[1,])) > 0)
	stopifnot(sum(!is.na(inp$dat_tmb$toa[,ncol(inp$dat_tmb$toa)])) > 0)
	
	stopifnot(dim(inp$dat_tmb$H)[2] == 3)
	
	# if z_vec != NULL 
	if(!inp$dat_tmb$how_3d %in% c('none', 'est')){
		stopifnot(length(inp$dat_tmb$z_vec) == inp$dat_tmb$np)
	}
	
	print("Pre-flight checkInp() passed!")
}