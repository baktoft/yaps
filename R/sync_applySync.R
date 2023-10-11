#' Apply sync_model to detection data
#' @export
applySync <- function(dat_sync, sync_model){
	dat_temp <- copy(dat_sync)

	lin_corr 	<- sync_model$inp_synced$sync_params$lin_corr
	gams 		<- sync_model$gams
	hydros 		<- sync_model$inp_synced$inp_params$hydros
	epo_range 	<- range(sync_model$inp_synced$inp_params$offset_levels)

	tk <- sync_model$inp_synced$sync_params$time_keeper
	dat_sync_cov <- getDatSyncCov(sync_model$inp_synced)


	dat_synced <- data.table()
	cat("Applying sync_model to hydro data...\n")

	for(h in 1:nrow(hydros)){
		gams[[h]]$newdata <- data.table(epofrac=dat_temp[h_sn == hydros[h, h_sn], epofrac])
	}

	n_cores <- parallel::detectCores()-1
	cat("...running parallel using ",n_cores," cores\n")
	cl <- parallel::makeCluster(n_cores)
	pred_list <- parallel::parLapplyLB(cl, gams,  fun=function(k){
		mgcv::predict.gam(k, newdata=k$newdata)
	})
	parallel::stopCluster(cl)
	
	for(h in 1:nrow(hydros)){
		cat(".",h,".")
		dat_temp_h <- dat_temp[h_sn == hydros[h_idx == h, h_sn]]
		dat_temp_h[, lin_corr := lin_corr[h, 1] + epofrac * lin_corr[h, 2]]
		# dat_temp_h[, pred := pred_h <- mgcv::predict.gam(gams[[h]], newdata=dat_temp_h)]
		dat_temp_h[, pred := pred_list[[h]]]
		dat_temp_h[, eposync := epofrac - lin_corr - pred]
		
		dat_temp_h[, lin_corr := NULL]
		dat_temp_h[, pred := NULL]
		
		dat_synced <- rbind(dat_synced, dat_temp_h)
		
		if(h %% 10 == 0){ cat("\n")}

	}
	cat("\n")
	setorder(dat_synced, eposync)
	
	# # # NA'ing data outside sync period
	dat_synced[!epofrac %between% epo_range, eposync := NA]
	n_outside_sync_range <- nrow(dat_synced[is.na(eposync)])
	if(n_outside_sync_range > 0){
		cat("NOTE: ",n_outside_sync_range," rows are outside the period synced by the model - these are returned as eposync=NA. \n")
	}
	
	# # # NA'ing data where time keeper had no data 
	nas_tk <- dat_sync_cov[h_sn == tk & N == 0]
	if(nrow(nas_tk) > 0){
		for(i in 1:nrow(nas_tk)){
			na_range_i <- sync_model$inp_synced$inp_params$offset_levels[nas_tk[i, offset_idx], ]
			dat_synced[epofrac %between% na_range_i, eposync := NA]
		}
	}
	n_tk_nas <- nrow(dat_synced[is.na(eposync)]) - n_outside_sync_range
	if(n_tk_nas > 0){
		cat("NOTE: ",n_tk_nas," rows are not synced because the time keeper had too few data - these are returned as eposync=NA. This affects all hydros. \n")
	}
	
	# # # NAing data where individual hydros had no sync data...
	nas_non_tk <- dat_sync_cov[h_sn != tk & N == 0]
	if(nrow(nas_non_tk) > 0){
		for(i in 1:nrow(nas_non_tk)){
			na_range_i <- sync_model$inp_synced$inp_params$offset_levels[nas_non_tk[i, offset_idx], ]
			dat_synced[epofrac %between% na_range_i, eposync := NA]
		}
	}
	n_non_tk_nas <- nrow(dat_synced[is.na(eposync)]) - n_outside_sync_range - n_tk_nas
	if(n_non_tk_nas > 0){
		cat("NOTE: ",n_non_tk_nas," rows are not synced because the specific hydro(s) had too few data - these are returned as eposync=NA. \n")
		cat(paste0("... These hydros are affected: ", paste0(unique(nas_non_tk$h_sn), collapse=","),"\n"))
	}
	
	dat_temp <- NULL
	
	return(dat_synced)
}
