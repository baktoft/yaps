#' Plot sync coverage 

#' @export
plotSyncCov <- function(inp_sync){

	dat_sync_cov <- getDatSyncCov(inp_sync)
	ylim <- c(0, max(dat_sync_cov$N))
	
	p1 <- ggplot2::ggplot(dat_sync_cov, aes(x=offset_idx, y=N)) + geom_point() + facet_wrap(~h_sn) + ylim(ylim)
	p1 <- p1 + geom_point(data=dat_sync_cov[N <= 20], col="blue")
	p1 <- p1 + geom_point(data=dat_sync_cov[N <= 10], col="red")
	p1
}
