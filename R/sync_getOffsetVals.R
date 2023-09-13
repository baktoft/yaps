
#' Internal function to get info relating to sync offsets per day
#' @inheritParams getInpSync
#' @noRd
#' @export
getOffsetVals <- function(inp_toa_list, sync_params){
	epo_self_vec <- inp_toa_list$epo_self_vec
	epo_start <- min(epo_self_vec)-10
	epo_end   <- max(epo_self_vec)+10
	
	n_offset_idx <- ceiling((epo_end - epo_start)/(24*60*60)) * sync_params$n_per_day
	offset_cuts <- cut(epo_self_vec, breaks=n_offset_idx, dig.lab=10)
	offset_idx <- as.numeric(offset_cuts)
	offset_labs <- levels(offset_cuts)
	offset_levels <- cbind(lower = as.numeric( sub("\\((.+),.*", "\\1", offset_labs) ),	  upper = as.numeric( sub("[^,]*,([^]]*)\\]", "\\1", offset_labs) ))
	
	offset_levels[1,1] 				<- epo_start
	offset_levels[n_offset_idx,2] 	<- epo_end
	
	dimnames(offset_levels) <- NULL
	return(list(n_offset_idx=n_offset_idx, offset_idx=offset_idx, offset_levels=offset_levels))
}

