#' Internal function. Extract speed of sounds for each timestamp used in sync-process from supplied data.
#' @inheritParams getInpSync
#' @noRd
getSsDataVec <- function(inp_toa_list, ss_data){
	roll <- data.table::data.table(ts = as.POSIXct(inp_toa_list$epo_self_vec, origin="1970-01-01", tz="UTC"))
	data.table::setkey(ss_data, ts)
	data.table::setkey(roll, ts)
	ss_data_vec <- ss_data[roll, roll="nearest"]$ss
	return(ss_data_vec)
}

# #' Internal function. Get toa for sync from sync_dat
# #' @inheritParams getInpSync
# #' @noRd
# getInpSyncToaList <- function(sync_dat, max_epo_diff, min_hydros, excl_self_detect, keep_rate, lin_corr_coeffs){
	# toa_list_gross   		<- buildToaListGross(sync_dat, excl_self_detect)
	# toa_list_pruned 		<- pruneToaListGross(toa_list_gross, max_epo_diff, min_hydros)
	
	# return(toa_list_pruned)
# }

#' Internal function to get info relating to sync ss per day
#' @inheritParams getInpSync
#' @noRd
getSsVals <- function(inp_toa_list, n_ss_day){
	epo_self_vec <- inp_toa_list$epo_self_vec
	epo_start <- min(epo_self_vec)-10
	epo_end   <- max(epo_self_vec)+10
	
	n_ss_idx <- ceiling((epo_end - epo_start)/(24*60*60)) * n_ss_day
	ss_cuts <- cut(epo_self_vec, breaks=n_ss_idx, dig.lab=10)
	ss_idx <- as.numeric(ss_cuts)
	ss_labs <- levels(ss_cuts)
	ss_levels <- cbind(lower = as.numeric( sub("\\((.+),.*", "\\1", ss_labs) ),	  upper = as.numeric( sub("[^,]*,([^]]*)\\]", "\\1", ss_labs) ))

	ss_levels[1,1] 				<- epo_start
	ss_levels[n_ss_idx,2] 		<- epo_end

	dimnames(ss_levels) <- NULL

	return(list(n_ss_idx=n_ss_idx, ss_idx=ss_idx, ss_levels=ss_levels))
}

#' Internal function to get residuals from sync_model in long format
#' @inheritParams getInpSync
#' @noRd
getEpsLong <- function(report, pl, inp_sync){
	
	if(inp_sync$dat_tmb_sync$ss_data_what == "est"){
		ss_vec <- pl$SS[inp_sync$dat_tmb_sync$ss_idx]
	} else {
		ss_vec <- inp_sync$dat_tmb_sync$ss_data_vec
	}
	
	
	eps <- report$eps
	eps[which(eps==0)] <- NA
	eps_long <- data.table::data.table(reshape2::melt(eps))
	if(inp_sync$sync_type == 'top'){
		colnames(eps_long) <- c('ping', 'hydro_idx', 'E')
		eps_long[, sync_tag_idx:=rep(inp_sync$dat_tmb_sync$sync_tag_idx_vec, times=ncol(eps))]
		eps_long[, ss:=rep(ss_vec, times=ncol(eps))]
	} else {
		colnames(eps_long) <- c('E')
		eps_long[ , ping := inp_sync$dat_tmb_sync$toa_delta[,'ping_idx']]
		eps_long[ , h1_idx := inp_sync$dat_tmb_sync$toa_delta[,'h1']]
		eps_long[ , h2_idx := inp_sync$dat_tmb_sync$toa_delta[,'h2']]
		eps_long[ , sync_tag_idx := inp_sync$dat_tmb_sync$toa_delta[,'sync_tag_idx']]
		eps_long[ , ss := ss_vec[inp_sync$dat_tmb_sync$toa_delta[,'ping_idx']]]
	}
	eps_long[, E_m:=E*ss]
	
	eps_long <- eps_long[!is.na(E)]
	
	return(eps_long)
}

