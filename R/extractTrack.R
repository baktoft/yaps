#' Internal function to extract the estimated track
#'
#' @param inp input
#' @param pl input
#' @param plsd input
#'
#' @export

extractTrack <- function(inp, pl, plsd){

	# extract track in user friendly format
	track <- data.table::data.table(
		top=as.POSIXct(pl$TOP + inp$inp_params$T0, origin="1970-01-01", tz="UTC"), top_sd=plsd$TOP,
		x=pl$X+inp$inp_params$Hx0, y=pl$Y+inp$inp_params$Hy0, 
		x_sd=plsd$X, y_sd=plsd$Y)
	if(inp$dat_tmb$how_3d == 'est'){
		track[, z := pl$Z]
		track[, z_sd := plsd$Z]
	} else if(inp$dat_tmb$how_3d == 'data'){
		track[, z := inp$dat_tmb$z_vec]
		track[, z_sd := NA]
	} else {
		track[, z:=NA]
		track[, z_sd:=NA]
	}
	
	
	track[, nobs := apply(inp$dat_tmb$toa, 1, function(k) sum(!is.na(k)))]
	
	track <- track[, c('top', 'x', 'y', 'z', 'top_sd', 'x_sd', 'y_sd', 'z_sd', 'nobs')]

	return(track)
}