
#' Internal function to get params for TMB sync
#' @inheritParams getInpSync
#' @noRd
#' @export
getParamsTmbSync <- function(dat_tmb_sync, ss_data_vec){
	params_tmb_sync <- list()
	
	params_tmb_sync$OFFSET <- matrix(rnorm(dat_tmb_sync$nh*dat_tmb_sync$n_offset_idx, 0, 3), nrow=dat_tmb_sync$nh, ncol=dat_tmb_sync$n_offset_idx)
	params_tmb_sync$TRUE_H <- as.matrix(cbind(dat_tmb_sync$H[,1], dat_tmb_sync$H[,2], dat_tmb_sync$H[,3]))
	params_tmb_sync$LOG_SIGMA_TOA <- 0
	params_tmb_sync$LOG_SIGMA_OFFSET <- 0

	if(ss_data_vec[1] == 0){
		params_tmb_sync$SS <- rnorm(dat_tmb_sync$n_offset_idx, 1420, 1)
	}
	
	params_tmb_sync$TOP <- rowMeans(dat_tmb_sync$toa_offset, na.rm=TRUE)
	
	return(params_tmb_sync)
}

