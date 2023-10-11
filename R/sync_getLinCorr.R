#' Get a matrix of coefficients for linear models used for linear correction prior to run getSyncModel()
#' To be used as input in getInpSync() parameter lin_corr_coeffs
#' @export
getLinCorr <- function(sync_model){
	hydros <- sync_model$inp_synced$inp_params$hydros
	time_keeper_idx <- sync_model$inp_synced$dat_tmb_sync$tk
	
	epo_offset <- rowMeans(sync_model$inp_synced$inp_params$offset_levels)

	lin_corr_coefs <- c()
	for(i in 1:nrow(hydros)){
		if(i == time_keeper_idx){
			coefs <- c(0,0)
		} else {
			off_i <- sync_model$pl$OFFSET[i,]
			if(sum(!is.na(off_i)) == 0){
				coefs <- c(0,0)
			} else {
				m_i <- lm(off_i ~ epo_offset)
				coefs <- coef(m_i)
				names(coefs) <- NULL
			}
		}
		lin_corr_coefs <- rbind(lin_corr_coefs, coefs)
	}
	rownames(lin_corr_coefs) <- hydros[, h_sn]
	return(lin_corr_coefs)
}

