#' Test YAPS core functionality
#'
#' Run `testYaps()` to check that the core functions of YAPS is working correctly.
#' Output should be a random simulated (black) and estimated (red) track.
#' @param silent Logical whether to print output to the console
#' @export
testYaps <- function(silent=FALSE){
	set.seed(42)
	trueTrack <- simTrueTrack(model='crw', n = 2000, deltaTime=1, shape=1, scale=0.5, addDielPattern=TRUE, ss='rw')
	pingType <- 'sbi'
	sbi_mean <- 20; sbi_sd <- 1e-4;
	teleTrack <- simTelemetryTrack(trueTrack, pingType=pingType, sbi_mean=sbi_mean, sbi_sd=sbi_sd)
	hydros <- data.table::data.table(hx=c(-250,-250,250,250), hy=c(-250,250,-250,250))
	toa_list <- simToa(teleTrack, hydros, pingType, sigmaToa=1e-4, pNA=0.25, pMP=0.01)
	toa <- toa_list$toa
	ss_data_what <- 'data'
	ss_data <- teleTrack$ss
	inp <- getInp(hydros, toa, E_dist="t", n_ss=2, pingType=pingType, sdInits=0, ss_data_what=ss_data_what, ss_data=ss_data)
	maxIter <- 100
	suppressWarnings(outTmb <- runTmb(inp, maxIter=maxIter, getPlsd=TRUE, getRep=TRUE))
	# print(str(outTmb))
	pl <- outTmb$pl
	yaps_out <- data.table::data.table(X=pl$X + inp$inp_params$Hx0, Y=pl$Y + inp$inp_params$Hy0)
	plsd <- outTmb$plsd
	plot(y~x, data=trueTrack, type="l", xlim=range(hydros$hx), ylim=range(hydros$hy), asp=1, lwd=2)
	lines(y~x, data=teleTrack)
	points(hy~hx, data=hydros, col="green", pch=20, cex=3)
	lines(Y~X, data=yaps_out, col="red")

	if(!silent) {cat("You should now see a plot of a simulted track - if so YAPS core functions are working \n")}
}