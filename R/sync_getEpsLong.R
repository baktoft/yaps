
#' Internal function to get residuals from sync_model in long format
#' @inheritParams getInpSync
#' @noRd
#' @export
getEpsLong <- function(report, pl, inp_synced){
	
	if(is.null(nrow(inp_synced$inp_params$dat_ss))){
		ss_vec <- pl$SS[inp_synced$dat_tmb_sync$offset_idx]
	} else {
		ss_vec <- inp_synced$dat_tmb_sync$ss_vec
	}
	
	
	eps <- report$eps
	eps[which(eps==0)] <- NA
	eps_long <- data.table::data.table(reshape2::melt(eps))

	colnames(eps_long) <- c('ping', 'hydro_idx', 'E')
	eps_long[, sync_tag_idx:=rep(inp_synced$dat_tmb_sync$sync_tag_idx_vec, times=ncol(eps))]
	eps_long[, ss:=rep(ss_vec, times=ncol(eps))]

	eps_long[, E_m:=E*ss]
	
	eps_long <- eps_long[!is.na(E)]
	
	return(eps_long)
}

