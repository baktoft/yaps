
#' Internal warpper function to do downsampling of the toa
#' @inheritParams getInpSync
#' @noRd
#' @export
getDownsampledToaList <- function(inp_toa_list_all, offset_vals_all, keep_rate){
	if(keep_rate > 0 & keep_rate <= 1){
		toa_list_downsampled 	<- downsampleToaList_random(inp_toa_list_all, keep_rate)
	} else if(keep_rate >= 10){
		toa_list_downsampled 	<- downsampleToaList_selective(inp_toa_list_all, offset_vals_all, keep_rate)
	}
	return(toa_list_downsampled)
}

# Internal function to selectively downsample the toa-matrix for inp_sync
#' @param inp_toa_list_all Output from `getInpSyncToaList`
#' @inheritParams getInpSync
#' @noRd
#' @export
downsampleToaList_selective <- function(inp_toa_list_all, offset_vals_all, keep_rate){
	toa <- inp_toa_list_all$toa
	offset_idx <- offset_vals_all$offset_idx
	
	toa_long <- data.table::data.table(reshape2::melt(toa), rep(offset_idx, times=ncol(toa)))
	colnames(toa_long) <- c('ping', 'h_idx','toa','offset_idx')
	# nobs_per_offset <- toa_long[!is.na(toa), .N, by=c('h_idx' ,'offset_idx')]
	
	nobs_per_offset <- data.table::data.table(reshape2::melt(with(toa_long[!is.na(toa)], table(h_idx, offset_idx)), value.name="N"))
	
	
	keep_pings <- c()
	for(i in 1:length(unique(offset_idx))){
		toa_long_i <- toa_long[offset_idx == i]
		keep_pings_i <- c()
		# h_order <- order(nobs_per_offset[offset_idx == i, N])

		h_dets <- nobs_per_offset[offset_idx == i, N, by=h_idx]
		data.table::setorder(h_dets, N)
		h_order <- h_dets$h_idx

		for(h in 1:length(h_order)){
			already_in_keeps <- nrow(toa_long_i[!is.na(toa) & ping %in% keep_pings_i & offset_idx == i & h_idx==h_order[h]])
			if(already_in_keeps >= keep_rate){
				# already enough with this hydro...
				next
			} else {
				need <- keep_rate - already_in_keeps + 1
				pings_h <- toa_long_i[!is.na(toa) & offset_idx == i & h_idx==h_order[h], ping]
				if(length(pings_h) < need){
					keep_pings_h <- pings_h
				} else {
					keep_pings_h <- sample(pings_h, size=need)
				}
				keep_pings_i <- c(keep_pings_i, keep_pings_h)
			}
		}
		keep_pings <- c(keep_pings, keep_pings_i)
	}
	
	toa_list_downsampled <- list(	toa 				= inp_toa_list_all$toa[keep_pings, ], 
									sync_tag_idx_vec 	= inp_toa_list_all$sync_tag_idx[keep_pings], 
									epo_self_vec 		= inp_toa_list_all$epo_self_vec[keep_pings])
	return(toa_list_downsampled)
}


# Internal function to randomly downsample the toa-matrix for inp_sync
#' @param inp_toa_list_all Output from `getInpSyncToaList`
#' @inheritParams getInpSync
#' @noRd
#' @export
downsampleToaList_random <- function(inp_toa_list_all, keep_rate){
	toa_list_downsampled <- list()
	keeps_idx <- which(stats::rbinom(nrow(inp_toa_list_all$toa), 1, keep_rate) == 1)
	toa_list_downsampled$toa <- inp_toa_list_all$toa[keeps_idx,]
	toa_list_downsampled$epo_self_vec <- inp_toa_list_all$epo_self_vec[keeps_idx]
	toa_list_downsampled$sync_tag_idx_vec <- inp_toa_list_all$sync_tag_idx_vec[keeps_idx]

	return(toa_list_downsampled)
}

