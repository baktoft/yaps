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
getInpSync <- function(sync_dat, max_epo_diff, min_hydros, time_keeper_idx, fixed_hydros_idx, n_offset_day, n_ss_day, keep_rate=1, excl_self_detect=TRUE, lin_corr_coeffs=NA, ss_data_what="est", ss_data=c(0), silent_check=FALSE, sync_type='top'){
	if(length(unique(sync_dat$hydros$serial)) != nrow(sync_dat$hydros)){
		print(sync_dat$hydros[, .N, by=serial][N>=2])
		stop("ERROR: At least one hydrophone serial number is used more than once in sync_dat$hydros!\n")
	}
	
	if(keep_rate <=0 | (keep_rate > 1 & keep_rate < 10) | (keep_rate >= 10 & keep_rate %% 1 != 0)){
		stop("ERROR: Invalid keep_rate! Must be either ]0;1] or integer >= 10\n")
	}
	
	if(sum(!sync_dat$hydros$serial %in% unique(sync_dat$detections$serial)) != 0){
		det_hydros <- unique(sync_dat$detections$serial)
		not_dets <- sync_dat$hydros$serial[!sync_dat$hydros$serial %in% det_hydros]
		cat(paste0("WARNING: These hydro serials were not found in the detection tabel. They cannot be synced. Please fix or remove. \n",paste(not_dets, collapse=", "),"\n"))
	}
	
	if(sum(!unique(sync_dat$detections$serial) %in% sync_dat$hydros$serial) != 0){
		det_hydros <- unique(sync_dat$detections$serial)
		not_dets <- det_hydros[!sync_dat$hydros$serial %in% det_hydros]
		cat(paste0("WARNING: These hydro serials are present in the detection tabel, but not in hydros. Please fix or remove. \n",paste(not_dets, collapse=", "),"\n"))
	}
	
	sync_dat <- appendDetections(sync_dat)
		
	if(is.na(lin_corr_coeffs[1])){
		lin_corr_coeffs <- matrix(0, nrow=nrow(sync_dat$hydros), ncol=2, byrow=TRUE)
	}

	sync_dat <- applyLinCorCoeffsInpSync(sync_dat, lin_corr_coeffs)
	
	T0 <- min(sync_dat$detections$epo)

	inp_H_info <- getInpSyncHInfo(sync_dat)

	inp_toa_list_all		<- getInpSyncToaList(sync_dat, max_epo_diff, min_hydros, excl_self_detect)
	fixed_hydros_vec 		<- getFixedHydrosVec(sync_dat, fixed_hydros_idx)
	offset_vals_all			<- getOffsetVals(inp_toa_list_all, n_offset_day)
	inp_toa_list 			<- getDownsampledToaList(inp_toa_list_all, offset_vals_all, keep_rate)
	offset_vals				<- getOffsetVals(inp_toa_list, n_offset_day)
	ss_vals 				<- getSsVals(inp_toa_list, n_ss_day)
	if(ss_data_what == "data"){
		ss_data_vec		<- getSsDataVec(inp_toa_list, ss_data)
	} else {
		ss_data_vec <- c(0)
	}

	dat_tmb_sync 		<- getDatTmbSync(sync_type, sync_dat, keep_rate, time_keeper_idx, inp_toa_list, fixed_hydros_vec, offset_vals, ss_vals, inp_H_info, T0, ss_data_what, ss_data_vec)
	params_tmb_sync 	<- getParamsTmbSync(dat_tmb_sync, ss_data_what)
	random_tmb_sync 	<- getRandomTmbSync(dat_tmb_sync, ss_data_what)
	# inits_tmb_sync <- c(3, rep(-3,dat_tmb_sync$nh))
	inits_tmb_sync <- c(-3)
	inp_params <- list(toa=inp_toa_list$toa, T0=T0, Hx0=inp_H_info$Hx0, Hy0=inp_H_info$Hy0, offset_levels=offset_vals$offset_levels, 
		ss_levels=ss_vals$ss_levels, max_epo_diff=max_epo_diff, hydros=sync_dat$hydros,
		lin_corr_coeffs=lin_corr_coeffs, min_hydros=min_hydros, ss_data=ss_data
	)
	
	inp_sync <- list(sync_type=sync_type, dat_tmb_sync=dat_tmb_sync, params_tmb_sync=params_tmb_sync, random_tmb_sync=random_tmb_sync, inits_tmb_sync=inits_tmb_sync, inp_params=inp_params)
	inp_sync$inp_params$sync_coverage <- checkInpSync(inp_sync, silent_check)
	return(inp_sync)
	
}
