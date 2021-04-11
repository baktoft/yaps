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
		# h_order <- order(nobs_per_offset[offset_idx == i, N])
		h_dets <- nobs_per_offset[offset_idx == i, N, by=h_idx]
		setorder(h_dets, N)
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
		
		
		gnu <- toa_long_i[ping %in% keep_pings_i & !is.na(toa)]
		
		table(gnu$h_idx)
		
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

#' Internal function to get params for TMB sync
#' @inheritParams getInpSync
#' @noRd
getParamsTmbSync <- function(dat_tmb_sync, ss_data_what){
	params_tmb_sync <- list()
	# if(dat_tmb_sync$model == 'yaps_sync_top'){
		params_tmb_sync$TOP <- rowMeans(dat_tmb_sync$toa, na.rm=TRUE)
	# }
	
	params_tmb_sync$OFFSET <- matrix(rnorm(dat_tmb_sync$nh*dat_tmb_sync$n_offset_idx, 0, 3), nrow=dat_tmb_sync$nh, ncol=dat_tmb_sync$n_offset_idx)
	params_tmb_sync$SLOPE1 <- matrix(rnorm(dat_tmb_sync$nh*dat_tmb_sync$n_offset_idx, 0, 3), nrow=dat_tmb_sync$nh, ncol=dat_tmb_sync$n_offset_idx)
	params_tmb_sync$SLOPE2 <- matrix(rnorm(dat_tmb_sync$nh*dat_tmb_sync$n_offset_idx, 0, 3), nrow=dat_tmb_sync$nh, ncol=dat_tmb_sync$n_offset_idx)
	params_tmb_sync$SLOPE3 <- matrix(rnorm(dat_tmb_sync$nh*dat_tmb_sync$n_offset_idx, 0, 3), nrow=dat_tmb_sync$nh, ncol=dat_tmb_sync$n_offset_idx)
	params_tmb_sync$TRUE_H <- as.matrix(cbind(dat_tmb_sync$H[,1], dat_tmb_sync$H[,2], dat_tmb_sync$H[,3]))
	params_tmb_sync$LOG_SIGMA_TOA <- 0
	# LOG_SIGMA_HYDROS_XY = rnorm(dat_tmb_sync$nh,-3,1)

	if(ss_data_what == "est"){
		params_tmb_sync$SS <- rnorm(dat_tmb_sync$n_ss_idx, 1420, 1)
	}
	return(params_tmb_sync)
}

#' Internal function to get random params for TMB sync
#' @inheritParams getInpSync
#' @noRd
getRandomTmbSync <- function(dat_tmb_sync, ss_data_what){
	if(dat_tmb_sync$model == "yaps_sync_top"){
		random_tmb_sync <- c("TOP")
	} else {
		random_tmb_sync <- c("TOP")
	}
	
	random_tmb_sync <- c(random_tmb_sync, "OFFSET", "SLOPE1", "SLOPE2", "SLOPE3", "TRUE_H")
	
	if(ss_data_what == "est"){
		random_tmb_sync <- c(random_tmb_sync, "SS")
	}
	return(random_tmb_sync)
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
