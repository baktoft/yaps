#' Fine-tune an already fitted sync_model
#' Wrapper function to re-run getSyncModel() using the same data, but excluding outliers. Note dimensions of data might change if eps_threshold results in empty rows in the TOA-matrix.
#' @param sync_model sync_model obtained using getSyncModel()
#' @param eps_threshold Maximum value of residual measured in meter assuming speed of sound = 1450 m/s
#' @param silent logical whether to make getSyncModel() silent
#' @export
fineTuneSyncModel <- function(sync_model, eps_threshold, silent=TRUE){
	# original inp_sync
	inp_sync <- sync_model$inp_synced

	# getting resids, identify outliers and get rid of them everywhere in inp_sync
	resids <- sync_model$report$eps
	resids[resids == 0] <- NA
	outliers  <- which(abs(resids)*1450 > eps_threshold)
	inp_sync$dat_tmb_sync$toa_offset[outliers] <- NA
	inp_sync$inp_params$toa[outliers] <- NA

	# check if any empty rows now exists - if so get rid of them entirely
	nobs <- apply(inp_sync$dat_tmb_sync$toa_offset, 1, function(k) sum(!is.na(k)))
	empty_rows <- which(nobs <= inp_sync$inp_params$min_hydros)

	if(length(empty_rows) > 0){
		inp_sync$dat_tmb_sync$toa_offset <- inp_sync$dat_tmb_sync$toa_offset[-empty_rows, ]
		inp_sync$dat_tmb_sync$sync_tag_idx_vec <- inp_sync$dat_tmb_sync$sync_tag_idx_vec[-empty_rows]
		inp_sync$dat_tmb_sync$offset_idx <- inp_sync$dat_tmb_sync$offset_idx[-empty_rows]
		inp_sync$dat_tmb_sync$ss_idx <- inp_sync$dat_tmb_sync$ss_idx[-empty_rows]
		inp_sync$dat_tmb_sync$np <- inp_sync$dat_tmb_sync$np - length(empty_rows)
		
		inp_sync$params_tmb_sync$TOP <- inp_sync$params_tmb_sync$TOP[-empty_rows]
		
		inp_sync$inp_params$toa <- inp_sync$inp_params$toa[-empty_rows, ]
	}

	# # # attempt to speed up next getSyncModel() by setting initial param values to relevant values
	# # # If NAs in OFFSET or SLOPEs it will not work...
	# inp_sync$params_tmb_sync$OFFSET 				<- sync_model$pl$OFFSET
	# inp_sync$params_tmb_sync$SLOPE1 				<- sync_model$pl$SLOPE1
	# inp_sync$params_tmb_sync$SLOPE2 				<- sync_model$pl$SLOPE2
	inp_sync$params_tmb_sync$SS 					<- sync_model$pl$SS
	inp_sync$params_tmb_sync$TRUE_H					<- sync_model$pl$TRUE_H
	inp_sync$params_tmb_sync$LOG_SIGMA_TOA			<- sync_model$pl$LOG_SIGMA_TOA
	inp_sync$params_tmb_sync$LOG_SIGMA_HYDROS_XY	<- sync_model$pl$LOG_SIGMA_HYDROS_XY

	# run getSyncModel() using the tuned inp_sync
	sync_model_tuned <- getSyncModel(inp_sync, silent=silent)
	
	return(sync_model_tuned)
}
