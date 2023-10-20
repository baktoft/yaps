#' Get object inp for synchronization
#'
#' @param sync_dat List containing data.tables with hydrophone information and detections. See e.g. `?ssu1` for example
#' @param max_epo_diff Sets the upper threshold for differences in TOA of sync tags. Best parameter value depends on burst rate of sync tags and how far apart the internal clocks of the hydros are prior to synchronization. A bit less than half of minimum sync tag burst rate is a good starting choice.
#' @param min_hydros Sets the lower threshold of how many hydrophones need to detect each sync tag ping in order to be included in the sync process. Should be as high as possible while observing that all hydrosphones are contributing. If too low, isolated hydrophones risk falling out completely. Future versions will work towards automising this.
#' @param time_keeper_idx Index of the hydrophone to use as time keeper. Could e.g. be the one with smallest overall clock-drift.
#' @param fixed_hydros_idx Vector of hydro idx's for all hydrophones where the position is assumed to be known with adequate accuracy and precission. Include as many as possible as fixed hydros to reduce overall computation time and reduce overall variability. As a bare minimum two hydros need to be fixed, but we strongly advice to use more than two.
#' @param n_offset_day Specifies the number of hydrophone specific quadratic polynomials to use per day. For PPM based systems, 1 or 2 is often adeqaute.
#' @param n_ss_day Specifies number of speed of sound to estimate per day if no ss data is supplied. It is recommended to use logged water temperature instead. However, estimating SS gives an extra option for sanity-checking the final sync-model.
#' @param ss_data_what Indicates whether to estimate ("est") speed of sound or to use data based on logged water temperature ("data").
#' @param ss_data data.table containing timestamp and speed of sound for the entire period to by synchronised. Must contain columns 'ts' (POSIXct timestamp) and 'ss' speed of sound in m/s (typical values range 1400 - 1550).
#' @param keep_rate Syncing large data sets can take a really long time. However, there is typically an excess number of sync tag detections 
#'			and a sub-sample is typically enough for good synchronization. 
#'			This parameter EITHER specifies a proportion (0-1) of data to keep when sub-sampling 
#'			OR (if keep_rate > 10) number of pings (approximate) to keep in each hydro X offset_idx combination if enough exists.
#' @param excl_self_detect Logical whether to excluded detections of sync tags on the hydros they are co-located with. Sometimes self detections can introduce excessive residuals in the sync model in which case they should be excluded.
#' @param lin_corr_coeffs Matrix of coefficients used for pre-sync linear correction. `dim(lin_corr_coeffs)=(#hydros, 2)`. 
#' @param silent_check Logical whether to get output from `checkInpSync()`. Default is FALSE
#' @param sync_type String specifying which type og sync model to use. Default sync_type='top' is the original included in yaps. Experimental sync_type='delta' is the ealternative - seems to works just as good, but might be much faster.
#'
#' @export
#' @return List of input data ready for use in `getSyncModel()`
#' @example man/examples/example-yaps_ssu1.R
# getInpSync <- function(sync_dat, max_epo_diff, min_hydros, time_keeper_idx, fixed_hydros_idx, n_offset_day, n_ss_day, keep_rate=1, 
	# excl_self_detect=TRUE, lin_corr_coeffs=NA, ss_data_what="est", ss_data=c(0), silent_check=FALSE, sync_type='top', Edist_sync){

getInpSync <- function(hydros, dat_sync, dat_ss=NA, sync_params, plot=TRUE){
	
	dat_sync_temp <- copy(dat_sync)
	
	# add defaults and calculated values to sync_params
	sync_params <- prepSyncParams(hydros, dat_sync_temp, sync_params)
		
	# run some checks on the data going in...
	checkInpSyncData(hydros, dat_sync_temp, dat_ss, sync_params)
	
	if(!is.na(sync_params$trial_days)){
		cat("NOTE: Using first ",sync_params$trial_days," days as a trial run... \n")
		cat("...run sync_params$trial_days <- NA  to skip trial mode\n")
		dat_sync_temp <- dat_sync_temp[ts <= min(ts) + sync_params$trial_days*24*60*60]
	}

	# apply linear corrections if corrections values are provided - otherwise ignore
	dat_sync_temp <- applyLinCor(hydros, dat_sync_temp, sync_params)
	
	# get time 0
	t0 <- min(dat_sync_temp$epo)
	
	
	inp_toa_list_all		<- getSyncToa(hydros, dat_sync_temp, sync_params)
	offset_vals_all			<- getOffsetVals(inp_toa_list = inp_toa_list_all, sync_params)
	
	inp_toa_list 			<- getDownsampledToaList(inp_toa_list_all, offset_vals_all, keep_rate=sync_params$keep_rate)
	offset_vals				<- getOffsetVals(inp_toa_list, sync_params)


	# check if any hydros are completely missing...
	nobs_h <- apply(inp_toa_list$toa, 2, function(k) sum(!is.na(k)))
	if(sum(nobs_h == 0) > 0){
		cat("ERROR: At least one hydros gets no detection is TOA matrix! This/these cannot be synced and must be removed!\n... h_sn ", names(nobs_h)[(nobs_h == 0)], "\n")
		stopSilent()
	}
	
	# tk_idx <- hydros[h_sn == sync_params$time_keeper, h_idx]
	# init_offsets <- colMeans(inp_toa_list$toa - inp_toa_list$toa[, tk_idx], na.rm=TRUE)
	
	# gnu <- t(t(inp_toa_list$toa) - init_offsets)
	# inp_toa_list$toa <- gnu
	
	if(is.null(nrow(dat_ss))){
		ss_vec <- c(0)
	} else {
		ss_vec			<- getSsVec(inp_toa_list, ss_data) # not upgraded to ver2 yet!!!
	}

	dat_tmb_sync 		<- getDatTmbSync(hydros, dat_sync_temp, sync_params, inp_toa_list, offset_vals, t0, ss_vec)
	params_tmb_sync 	<- getParamsTmbSync(dat_tmb_sync, ss_vec)
	random_tmb_sync 	<- getRandomTmbSync(dat_tmb_sync, ss_vec)

	inits_tmb_sync <- c(-3, -3) # inits for LOG_SIGMA_TOA and LOG_SIGMA_OFFSET
	
	if(ss_vec[1] == 0){
		inits_tmb_sync <- c(inits_tmb_sync, -1)
	}

	inp_params <- list(hydros=hydros, toa=inp_toa_list$toa, offset_levels=offset_vals$offset_levels, dat_ss=dat_ss, t0 = t0)
	
	inp_sync <- list(dat_tmb_sync=dat_tmb_sync, params_tmb_sync=params_tmb_sync, random_tmb_sync=random_tmb_sync, inits_tmb_sync=inits_tmb_sync, inp_params=inp_params, sync_params=sync_params)
	
	if(plot){
		p0 <- plotSyncNetwork(hydros, dat_sync_temp)
		p1 <- plotSyncCov(inp_sync)
		print(cowplot::plot_grid(p0, p1, ncol=1))
	}

	# check to ensure that the timekeeper has data in all offset_idxs
	dat_tk <- data.table(cbind(toa=inp_toa_list$toa[, colnames(inp_toa_list$toa) == sync_params$time_keeper], offset_idx = factor(dat_tmb_sync$offset_idx)))
	toa <- inp_toa_list$toa
	if(nrow(dat_tk[!is.na(toa), .N, by=offset_idx]) != length(unique(dat_tk$offset_idx)) | nrow(dat_tk[!is.na(toa), .N, by=offset_idx][N < 10]) > 0){
		cat("WARNING: The time keeper should have data (N >= 10) in all offset_idx!\n")
		cat("...Data in these periods will not be synced!\n")
		# stopSilent()
	}


	
	# inp_sync$inp_params$sync_coverage <- checkInpSync(inp_sync, silent_check)
	return(inp_sync)
	
}
