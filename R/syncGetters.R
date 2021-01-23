#' Get sync model from inp_sync object obtained by getInpSync()
#' @param inp_sync Input data prepared for the sync model using `getInpSync()`
#' @param silent Keep TMB quiet
#' @param fine_tune Logical. Whether to re-run the sync model excluding residual outliers. Consider to use fineTuneSyncModel() instead.
#' @param max_iter Max number of iterations to run TMB. Default=100 seems to work in most cases.
#' @export
getSyncModel <- function(inp_sync, silent=TRUE, fine_tune=FALSE, max_iter=100, tmb_smartsearch=TRUE){
	inp_sync$inp_params$tmb_smartsearch <- tmb_smartsearch
	
	dat_tmb <- inp_sync$dat_tmb_sync
	params <- inp_sync$params_tmb_sync
	random <- inp_sync$random_tmb_sync
	inits <- inp_sync$inits_tmb_sync
	inp_params <- inp_sync$inp_params

	cat(paste0(Sys.time(), " \n"))
	cat(". Running optimization of the sync model. Please be patient - this can take a long time. \n")
	if(fine_tune){cat(".... fine tuning is enabled, but is getting deprecated in this function. Consider to use the function fineTuneSyncModel() instead. See ?fineTuneSyncModel for info. \n")}

	tictoc::tic("Fitting sync model: ")
	opt <- c()
	pl <- c()
	plsd <- c()
	obj <- c()

	tictoc::tic()
	obj <- c()
	opt <- c()
	report <- c()
	gc()
	
	# config(DLL="yaps_sync")
	# ## Reduce memory peak of a parallel model by creating tapes in serial
	# config(tape.parallel=0, DLL="yaps_sync")
	obj <- TMB::MakeADFun(data = dat_tmb, parameters = params, random = random, DLL = "yaps", inner.control = list(maxit = max_iter), silent=silent)
	obj$fn(obj$par) 
	
	if(!tmb_smartsearch){
		TMB::newtonOption(obj, smartsearch=FALSE)
	}

	
	if(silent){
		opt <- suppressWarnings(stats::nlminb(inits,obj$fn,obj$gr))
	} else {
		opt <- stats::nlminb(inits,obj$fn,obj$gr)
	}

	pl <- obj$env$parList()   # List of estimates
	obj_val <- opt$objective
	cat(paste0(".. ", Sys.time()), " \n")
	cat(".... obj = ", obj_val, " \n")
	report <- obj$report()

	crazy_outliers <- which(abs(report$eps_toa)*1450 > 10000)
	fine_outliers  <- which(abs(report$eps_toa)*1450 > 1000)
	if(length(crazy_outliers > 0)){
		cat(".... some extreme outliers potentially affecting the model where identified \n Consider to run fineTuneSyncModel(sync_model, eps_threshold=10000). See ?fineTuneSyncModel for more info. \n")
		# dat_tmb$toa_offset[crazy_outliers] <- NA
	} else if(fine_tune){
		cat(".... fine tuning is enabled, but is deprecated. Use the function fineTuneSyncModel() instead. See ?fineTuneSyncModel for info. \n")
		# dat_tmb$toa_offset[fine_outliers] <- NA
	} 

	tictoc::toc()

	jointrep <- try(TMB::sdreport(obj, getJointPrecision=TRUE), silent=silent)
	param_names <- rownames(summary(jointrep))
	sds <- summary(jointrep)[,2]
	summ <- data.frame(param=param_names, sd=sds)
	plsd <- split(summ[,2], f=summ$param)
	
	pl$TRUE_H[,1] <- pl$TRUE_H[,1] + inp_params$Hx0
	pl$TRUE_H[,2] <- pl$TRUE_H[,2] + inp_params$Hy0
	eps_long <- yaps:::getEpsLong(report, pl, inp_sync)
	
	offset_nas <- which(pl$OFFSET == 0)
	pl$OFFSET[offset_nas] <- NA
	pl$SLOPE1[offset_nas] <- NA
	pl$SLOPE2[offset_nas] <- NA
	
	cat("Sync model done \n")
	cat("Consider saving the sync model for later use - e.g. save(sync_model, file='path_to_sync_save'). \n")
	tictoc::toc()
	return(list(pl=pl, plsd=plsd, report=report, obj_val=obj_val, eps_long=eps_long, inp_synced=inp_sync))
}




#' Get object inp for synchronization
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
#' @param lin_corr_coeffs Matrix of coefficients used for pre-sync linear correction. dim(lin_corr_coeffs)=(#hydros, 2). 
#' @param silent_check Logical whether to get output from checkInpSync(). Default is FALSE
#' @export
getInpSync <- function(sync_dat, max_epo_diff, min_hydros, time_keeper_idx, fixed_hydros_idx, n_offset_day, n_ss_day, keep_rate=1, excl_self_detect=TRUE, lin_corr_coeffs=NA, ss_data_what="est", ss_data=c(0), silent_check=FALSE){
	if(length(unique(sync_dat$hydros$serial)) != nrow(sync_dat$hydros)){
		print(sync_dat$hydros[, .N, by=serial][N>=2])
		stop("ERROR: At least one hydrophone serial number is used more than once in sync_dat$hydros!\n")
	}
	
	if(keep_rate <=0 | (keep_rate > 1 & keep_rate < 10) | (keep_rate >= 10 & keep_rate %% 1 != 0)){
		stop("ERROR: Invalid keep_rate! Must be either ]0;1] or integer >= 10\n")
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

	dat_tmb_sync 		<- getDatTmbSync(sync_dat, time_keeper_idx, inp_toa_list, fixed_hydros_vec, offset_vals, ss_vals, inp_H_info, T0, ss_data_what, ss_data_vec)
	params_tmb_sync 	<- getParamsTmbSync(dat_tmb_sync, ss_data_what)
	if(ss_data_what == "est"){
		random_tmb_sync <- c("TOP", "OFFSET", "SLOPE1", "SLOPE2", "SS", "TRUE_H")
	} else {
		random_tmb_sync <- c("TOP", "OFFSET", "SLOPE1", "SLOPE2", "TRUE_H")
	}
	# inits_tmb_sync <- c(3, rep(-3,dat_tmb_sync$nh))
	inits_tmb_sync <- c(3)
	inp_params <- list(toa=inp_toa_list$toa, T0=T0, Hx0=inp_H_info$Hx0, Hy0=inp_H_info$Hy0, offset_levels=offset_vals$offset_levels, 
		ss_levels=ss_vals$ss_levels, max_epo_diff=max_epo_diff, hydros=sync_dat$hydros,
		lin_corr_coeffs=lin_corr_coeffs, min_hydros=min_hydros, ss_data=ss_data
	)
	
	inp_sync <- list(dat_tmb_sync=dat_tmb_sync, params_tmb_sync=params_tmb_sync, random_tmb_sync=random_tmb_sync, inits_tmb_sync=inits_tmb_sync, inp_params=inp_params)
	inp_sync$inp_params$sync_coverage <- checkInpSync(inp_sync, silent_check)
	return(inp_sync)
	
}

#' Internal function. Extract speed of sounds for each timestamp used in sync-process from supplied data.
#' @inheritParams getInpSync
#' @noRd
getSsDataVec <- function(inp_toa_list, ss_data){
	roll <- data.table::data.table(ts = as.POSIXct(inp_toa_list$epo_self_vec, origin="1970-01-01", tz="UTC"))
	data.table::setkey(ss_data, ts)
	data.table::setkey(roll, ts)
	ss_data_vec <- ss_data[roll, roll="nearest"]$ss
	return(ss_data_vec)
}

#' Internal function. Apply linear correction matrix to epofrac before sync
#' @inheritParams getInpSync
#' @noRd
applyLinCorCoeffsInpSync <- function(sync_dat, lin_corr_coeffs){
	h_idxs <- sync_dat$hydros$idx
	
	for(h in 1:length(h_idxs)){
		h_idx <- h_idxs[h]
		# sync_dat$detections[]
		sync_dat$detections[hydro_idx == h_idx, epofrac :=  epofrac - lin_corr_coeffs[h_idx, 1] - epofrac * lin_corr_coeffs[h_idx, 2]]
	}
	return(sync_dat)
}


#' Internal function. Get toa for sync from sync_dat
#' @inheritParams getInpSync
#' @noRd
getInpSyncToaList <- function(sync_dat, max_epo_diff, min_hydros, excl_self_detect, keep_rate, lin_corr_coeffs){
	toa_list_gross   		<- buildToaListGross(sync_dat, excl_self_detect)
	toa_list_pruned 		<- pruneToaListGross(toa_list_gross, max_epo_diff, min_hydros)
	
	return(toa_list_pruned)
}

#' Internal warpper function to do downsampling of the toa
#' @inheritParams getInpSync
#' @noRd
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
		h_order <- order(nobs_per_offset[offset_idx == i, N])
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
downsampleToaList_random <- function(inp_toa_list_all, keep_rate){
	toa_list_downsampled <- list()
	keeps_idx <- which(stats::rbinom(nrow(inp_toa_list_all$toa), 1, keep_rate) == 1)
	toa_list_downsampled$toa <- inp_toa_list_all$toa[keeps_idx,]
	toa_list_downsampled$epo_self_vec <- inp_toa_list_all$epo_self_vec[keeps_idx]
	toa_list_downsampled$sync_tag_idx_vec <- inp_toa_list_all$sync_tag_idx_vec[keeps_idx]

	return(toa_list_downsampled)
}


#' Internal function to get vector of which hydros are fixed
#' @inheritParams getInpSync
#' @noRd
getFixedHydrosVec <- function(sync_dat, fixed_hydros_idx){
	fixed_hydros_vec <- rep(0, times=nrow(sync_dat$hydros))
	fixed_hydros_vec[fixed_hydros_idx] <- 1
	
	return(fixed_hydros_vec)
}


#' Internal function to get info relating to sync offsets per day
#' @inheritParams getInpSync
#' @noRd
getOffsetVals <- function(inp_toa_list, n_offset_day){
	epo_self_vec <- inp_toa_list$epo_self_vec
	epo_start <- min(epo_self_vec)-10
	epo_end   <- max(epo_self_vec)+10
	
	n_offset_idx <- ceiling((epo_end - epo_start)/(24*60*60)) * n_offset_day
	offset_cuts <- cut(epo_self_vec, breaks=n_offset_idx, dig.lab=10)
	offset_idx <- as.numeric(offset_cuts)
	offset_labs <- levels(offset_cuts)
	offset_levels <- cbind(lower = as.numeric( sub("\\((.+),.*", "\\1", offset_labs) ),	  upper = as.numeric( sub("[^,]*,([^]]*)\\]", "\\1", offset_labs) ))
	
	offset_levels[1,1] 				<- epo_start
	offset_levels[n_offset_idx,2] 	<- epo_end
	
	dimnames(offset_levels) <- NULL
	return(list(n_offset_idx=n_offset_idx, offset_idx=offset_idx, offset_levels=offset_levels))
}


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

#' Internal function to get dat for TMB sync
#' @inheritParams getInpSync
#' @noRd
getDatTmbSync <- function(sync_dat, time_keeper_idx, inp_toa_list, fixed_hydros_vec, offset_vals, ss_vals, inp_H_info, T0, ss_data_what, ss_data_vec){
	H <- as.matrix(inp_H_info$inp_H)
	dimnames(H) <- NULL
	
	toa_offset <- inp_toa_list$toa - offset_vals$offset_levels[offset_vals$offset_idx]

	dat_tmb_sync <- list(
		model = "yaps_sync",
		H=H,
		toa_offset=toa_offset,
		sync_tag_idx_vec = inp_toa_list$sync_tag_idx_vec,
		np = nrow(inp_toa_list$toa),
		nh = ncol(inp_toa_list$toa),
		tk = time_keeper_idx,
		fixed_hydros_vec = fixed_hydros_vec,
		offset_idx = offset_vals$offset_idx,
		n_offset_idx = offset_vals$n_offset_idx,
		ss_idx = ss_vals$ss_idx,
		n_ss_idx = ss_vals$n_ss_idx,
		ss_data_what = ss_data_what,
		ss_data_vec = ss_data_vec
	)
	return(dat_tmb_sync)
}


#' Internal function to get params for TMB sync
#' @inheritParams getInpSync
#' @noRd
getParamsTmbSync <- function(dat_tmb_sync, ss_data_what){
	params_tmb_sync <- list(
		TOP = 	rowMeans(dat_tmb_sync$toa, na.rm=TRUE),
		OFFSET = matrix(rnorm(dat_tmb_sync$nh*dat_tmb_sync$n_offset_idx, 0, 3), nrow=dat_tmb_sync$nh, ncol=dat_tmb_sync$n_offset_idx),
		SLOPE1 = matrix(rnorm(dat_tmb_sync$nh*dat_tmb_sync$n_offset_idx, 0, 3), nrow=dat_tmb_sync$nh, ncol=dat_tmb_sync$n_offset_idx),
		SLOPE2 = matrix(rnorm(dat_tmb_sync$nh*dat_tmb_sync$n_offset_idx, 0, 3), nrow=dat_tmb_sync$nh, ncol=dat_tmb_sync$n_offset_idx),
		TRUE_H = as.matrix(cbind(dat_tmb_sync$H[,1], dat_tmb_sync$H[,2], dat_tmb_sync$H[,3])),
		LOG_SIGMA_TOA = 0
		# LOG_SIGMA_HYDROS_XY = rnorm(dat_tmb_sync$nh,-3,1)
	)
	if(ss_data_what == "est"){
		params_tmb_sync[['SS']] <- rnorm(dat_tmb_sync$n_ss_idx, 1420, 1)
	}
	return(params_tmb_sync)
}


#' Internal function to get residuals from sync_model in long format
#' @inheritParams getInpSync
#' @noRd
getEpsLong <- function(report, pl, inp_sync){
	
	if(inp_sync$dat_tmb_sync$ss_data_what == "est"){
		ss_vec <- pl$SS[inp_sync$dat_tmb_sync$ss_idx]
	} else {
		ss_vec <- inp_sync$dat_tmb_sync$ss_data_vec
	}
	
	
	eps <- report$eps_toa
	eps[which(eps==0)] <- NA
	eps_long <- data.table::data.table(reshape2::melt(eps))
	colnames(eps_long) <- c('ping', 'hydro_idx', 'E')
	eps_long[, sync_tag_idx:=rep(inp_sync$dat_tmb_sync$sync_tag_idx_vec, times=ncol(eps))]
	eps_long[, ss:=rep(ss_vec, times=ncol(eps))]
	eps_long[, E_m:=E*ss]
	
	eps_long <- eps_long[!is.na(E)]
	
	return(eps_long)
}


#' Internal function to get data for checking sync_model
#' @param extreme_threshold Ignore delta values larger than this threshold. 
#' @inheritParams getInpSync
#' @noRd
getSyncCheckDat <- function(sync_model, extreme_threshold=1000){
	toa <- sync_model$inp_synced$inp_params$toa
	toa_sync <- applySync(toa, sync_model=sync_model)

	sync_tag_idx_vec <- sync_model$inp_synced$dat_tmb_sync$sync_tag_idx_vec
	ss_idx <- findInterval(rowMeans(toa, na.rm=TRUE), sync_model$inp_synced$inp_params$ss_levels[,1])
	offset_idx <- findInterval(rowMeans(toa, na.rm=TRUE), sync_model$inp_synced$inp_params$offset_levels[,1])

	true_x <- sync_model$pl$TRUE_H[,1]
	true_y <- sync_model$pl$TRUE_H[,2]
	true_z <- sync_model$pl$TRUE_H[,3]

	if(sync_model$inp_synced$dat_tmb_sync$ss_data_what == "est"){
		ss_long <- sync_model$pl$SS[ss_idx]
	} else {
		ss_long <- sync_model$inp_synced$dat_tmb_sync$ss_data_vec
	}

	toa_sync_long <- data.table::data.table(reshape2::melt(toa_sync))
	colnames(toa_sync_long) <- c('ping_idx','hydro_idx', 'toa_sync')
	toa_sync_long[, sync_tag_idx:=rep(sync_tag_idx_vec, times=ncol(toa))]
	toa_sync_long[, dist_to_sync_tag:= sqrt((true_x[hydro_idx] - true_x[sync_tag_idx])^2 + (true_y[hydro_idx] - true_y[sync_tag_idx])^2 + (true_z[hydro_idx] - true_z[sync_tag_idx])^2)]
	toa_sync_long[, ss:=rep(ss_long, times=ncol(toa))]
	toa_sync_long[, offset_idx:=rep(offset_idx, times=ncol(toa))]
	
	sync_check_dat <- c()
	for(i in 1:ncol(toa)){
		# sync_check_dat_i <- toa_sync_long[, .(focal_hydro_idx=i, hydro_idx, ping_idx, delta=abs(((toa_sync - toa_sync[hydro_idx==i])*ss) - (dist_to_sync_tag - dist_to_sync_tag[hydro_idx==i]))), by=c('sync_tag_idx')]
		sync_check_dat_i <- toa_sync_long[, .(focal_hydro_idx=i, hydro_idx, offset_idx, ping_idx, delta=abs(((toa_sync - toa_sync[hydro_idx==i])*ss) - (dist_to_sync_tag - dist_to_sync_tag[hydro_idx==i]))), by=c('sync_tag_idx')]
		sync_check_dat_i <- sync_check_dat_i[delta!= 0]
		sync_check_dat <- rbind(sync_check_dat, sync_check_dat_i)
	}
	n_extreme <- nrow(sync_check_dat[delta >= extreme_threshold])
	if(n_extreme > 0){
		sync_check_dat <- sync_check_dat[delta < extreme_threshold]
		print(paste0("NOTE: ",n_extreme," extreme outlier(s) (i.e. >= ",extreme_threshold," m) were ignored"))
	}
	
	return(sync_check_dat)

}
