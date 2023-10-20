#' Quick overview to check if all hydros have enough data within each offset period.
#'
#' @inheritParams checkInpSync
#' @param plot Logical indicating whether to plot a visual or not.
#' @export
#' @return A data.table containing number of pings included in each hydro x offset combination.
#' @example man/examples/example-yaps_ssu1.R
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
		p <- ggplot2::ggplot(sync_coverage) 
		p <- p + geom_point(aes(offset_idx, N), col="steelblue") 
		p <- p + geom_point(data=sync_coverage[N < 50], aes(offset_idx, N), col="blue", size=2)
		p <- p + geom_point(data=sync_coverage[N < 10], aes(offset_idx, N), col="orange", size=2)
		p <- p + geom_point(data=sync_coverage[N <= 5], aes(offset_idx, N), col="red", size=2)
		p <- p + facet_wrap(~h) + ylim(0, max(sync_coverage$N))
		print(p)
	}
	
	return(sync_coverage)
}