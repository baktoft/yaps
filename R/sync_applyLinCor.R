
#' Internal function. Apply linear correction matrix to epofrac before sync
#' @inheritParams getInpSync
#' @noRd
#' @export
applyLinCor <- function(hydros, dat_sync, sync_params){
	if(is.na(sync_params$lin_corr[1])){
		lin_corr <- matrix(0, nrow=nrow(hydros), ncol=2, byrow=TRUE)
		rownames(lin_corr) <- hydros$h_sn
		attr(dat_sync, 'lin_corrected') <- FALSE
	} else {
		lin_corr <- sync_params$lin_corr
		attr(dat_sync, 'lin_corrected') <- TRUE
	}
	
	h_sns <- hydros$h_sn
	
	for(h in 1:length(h_sns)){
		sn_h <- h_sns[h]
		dat_sync[h_sn == sn_h, epofrac :=  epofrac - lin_corr[rownames(lin_corr) == sn_h, 1] - epofrac * lin_corr[rownames(lin_corr) == sn_h, 2]]
	}
	
	
	dat_sync[]
	return(dat_sync)
}

