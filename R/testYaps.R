#' Test YAPS core functionality
#'
#' Run `testYaps()` to check that the core functions of YAPS is working correctly.
#' Output should be a random simulated (black) and estimated (red) track.
#' @param silent Logical whether to print output to the console
#' @param est_ss Logical whether to test using ss_data_what = 'est' (est_ss = TRUE) or ss_data_what = 'data' (est_ss = FALSE)
#' @param return_yaps Logical whether to return the fitted yaps model. Default=FALSE.
#' @inheritParams getInp
#' @inheritParams runYaps
#' @export
#' @return If `return_yaps == TRUE`, the fitted `yaps` object. See `?runYaps` for further info.

#' @examples
#' #' # To test  basic functionality of yaps using simulated data
#' testYaps()
#' # # # Three pingTypes are availabe: 
#' # # #	fixed burst interval (testYaps(pingType='sbi')), 
#' # # # 	random burst interval with UNKNOWN burst interval sequence('testYaps(pingType='rbi')), 
#' # # # 	random burst interval with KNOWN burst interval sequence (testYaps(pingType='pbi'))
testYaps <- function(silent=TRUE, pingType='sbi', est_ss=TRUE, opt_fun='nlminb', opt_controls=list(), return_yaps=FALSE, tmb_smartsearch=TRUE){
	set.seed(42)
	trueTrack <- simTrueTrack(model='crw', n = 2500, deltaTime=1, shape=1, scale=0.5, addDielPattern=FALSE, ss='rw')
	if(pingType == 'sbi'){
		sbi_mean <- 20; sbi_sd <- 1e-3;
		rbi_min <- sbi_mean; 
		rbi_max <- sbi_mean;
		teleTrack <- simTelemetryTrack(trueTrack, pingType=pingType, sbi_mean=sbi_mean, sbi_sd=sbi_sd)
	} else {
		rbi_min = 30
		rbi_max = 90
		if(pingType == 'rbi'){
			teleTrack <- simTelemetryTrack(trueTrack, pingType=pingType, rbi_min=rbi_min, rbi_max=rbi_max)
		} else {
			teleTrack_list <- simTelemetryTrack(trueTrack, pingType=pingType, rbi_min=rbi_min, rbi_max=rbi_max)
			teleTrack <- teleTrack_list[['out']]
			biTable   <- teleTrack_list[['biTable']]
		}
	}
	hydros <- data.table::data.table(hx=c(-250,-250,250,250), hy=c(-250,250,-250,250), hz=c(5,5,5,5))
	toa_list <- simToa(teleTrack, hydros, pingType, sigmaToa=1e-4, pNA=0.25, pMP=0.01)
	toa <- toa_list$toa
	
	if(est_ss){
		ss_data_what <- 'est'
		ss_data <- c(0)
	} else {
		ss_data_what <- 'data'
		ss_data <- teleTrack$ss
	}
	inp <- getInp(hydros, toa, E_dist="Mixture", n_ss=5, pingType=pingType, sdInits=0, ss_data_what=ss_data_what, ss_data=ss_data, rbi_min=rbi_min, rbi_max=rbi_max, biTable=biTable)
	maxIter <- 500
	yaps <- runYaps(inp, maxIter=maxIter, getPlsd=TRUE, getRep=TRUE, silent=silent, opt_fun=opt_fun, opt_controls, tmb_smartsearch)
	pl <- yaps$pl
	yaps_out <- data.table::data.table(X=pl$X + inp$inp_params$Hx0, Y=pl$Y + inp$inp_params$Hy0)
	plsd <- yaps$plsd
	plot(y~x, data=trueTrack, type="l", xlim=range(hydros$hx), ylim=range(hydros$hy), asp=1, lwd=2)
	lines(y~x, data=teleTrack)
	points(hy~hx, data=hydros, col="green", pch=20, cex=3)
	lines(Y~X, data=yaps_out, col="red")

	if(!silent) {cat("You should now see a plot of a simulted track - if so YAPS core functions are working \n")}
	
	if(return_yaps) {return(yaps)}
}
