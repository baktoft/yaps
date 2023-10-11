
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
