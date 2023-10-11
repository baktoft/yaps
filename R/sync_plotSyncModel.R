#' Plot various sync_model outputs
#' @export
plotSyncModel <- function(sync_model){
	eps_long <- getEpsLong(sync_model$report, sync_model$pl, sync_model$inp_synced)
	
	par(mfrow=c(2,2))
	matplot(t(sync_model$pl$OFFSET), type="l", main="sync model offsets", ylab="offset (seconds)", xlab="offset_idx")

	hist(eps_long$E_m, breaks=100, main="Res (m)", xlab="res (m)")
	
	if(is.null(nrow(sync_model$inp_synced$inp_params$dat_ss))){
		plot(sync_model$pl$SS, type="b", main="Estimated SS", ylab="SS (m/s)", xlab="offset_idx")
		lines(sync_model$pl$SS + sync_model$plsd$SS, type="l", lty=2, col="red")
		lines(sync_model$pl$SS - sync_model$plsd$SS, type="l", lty=2, col="red")
	}
}