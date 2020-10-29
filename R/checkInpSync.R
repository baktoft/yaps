#' Check consistency of `inp_sync` object obtained from `getInpSync()`
#' 
#' @inheritParams getInpSync
#' @param inp_sync Object obtained using `getInpSync()`
#' @export
checkInpSync <- function(inp_sync, silent_check){
	# speed of sound stuff
	if(!silent_check & inp_sync$dat_tmb_sync$ss_data_what != "data"){cat("WARNING: getSyncModel() will estimate speed of sound. It is strongly advised to use data instead!\n")}
	if(inp_sync$dat_tmb_sync$ss_data_what == "data" ){
		stopifnot(length(inp_sync$dat_tmb_sync$ss_data_vec) == inp_sync$dat_tmb_sync$np)
	}

	# is timekeeper a fixed hydro?
	if(!silent_check & inp_sync$dat_tmb_sync$fixed_hydros_vec[inp_sync$dat_tmb_sync$tk ] != 1){
		cat("NOTE: The designated time-keeper is not a fixed hydrophone - is this intentional?\n")
	}

	# are dimensions correct?
	stopifnot(ncol(inp_sync$dat_tmb_sync$toa) == inp_sync$dat_tmb_sync$nh)
	stopifnot(nrow(inp_sync$dat_tmb_sync$toa) == inp_sync$dat_tmb_sync$np)

	# check that all offset_idx are present in the offset_idx vector
	stopifnot(inp_sync$dat_tmb_sync$n_offset_idx == length(unique(inp_sync$dat_tmb_sync$offset_idx)))

	# check that all hydro x offset_idx combination have at least some data...
	# trigger thresholds for N is somewhat arbitrary..
	sync_coverage <- getSyncCoverage(inp_sync, plot=FALSE)
	if(!silent_check & min(sync_coverage$N) <= 5) {
		cat("WARNING: At least one hydro x offset_idx combination has less than 5 observations. This hydro cannot be synced in that period!\n")
		print(sync_coverage[N <= 5])
	}
	if(!silent_check & min(sync_coverage$N) < 10) {
		cat("WARNING: At least one hydro has less than 10 pings in an offset_idx - try getSyncCoverage(inp_sync, plot=TRUE) for visual\n and rerun getInpSync() with increased keep_rate\n")
	} else if(!silent_check & min(sync_coverage$N) < 50) {
		cat("NOTE: At least one hydro has less than 50 pings in an offset_idx - try getSyncCoverage(inp_sync, plot=TRUE) for visual\n and rerun getInpSync() with increased keep_rate\n")
	}
}

#' Quick overview to check if all hydros have enough data within each offset period.
#'
#' @inheritParams checkInpSync
#' @param plot Logical indicating whether to plot a visual or not.
#' @export
getSyncCoverage <- function(inp_sync, plot=FALSE){
	toa <- inp_sync$dat_tmb_sync$toa
	nh <- ncol(toa)
	offset_idx <- inp_sync$dat_tmb_sync$offset_idx

	toa_long <- data.table::data.table(reshape2::melt(toa))
	colnames(toa_long) <- c('ping', 'h','toa')
	toa_long[, offset_idx := rep(offset_idx, nh)]
	sync_coverage <- data.table::data.table(reshape2::melt(with(toa_long[!is.na(toa)], table(h, offset_idx))))
	colnames(sync_coverage) <- c('h', 'offset_idx' ,'N')
	
	if(plot){
		p <- ggplot2::ggplot(sync_coverage) + geom_point(aes(offset_idx, N)) 
		p <- p + geom_point(data=sync_coverage[N < 50], aes(offset_idx, N), col="red", size=2)
		p <- p + facet_wrap(~h) + ylim(0, max(sync_coverage$N))
		print(p)
	}
	
	return(sync_coverage)
}