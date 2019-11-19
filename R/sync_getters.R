#' Get object inp for synchronization
#' @param sync_dat List containing data.tables with hydrophone information and detections.
#' @export
getInpSync <- function(sync_dat, max_epo_diff, min_hydros, time_keeper_idx, fixed_hydros_idx, n_offset_day, n_ss_day){
	sync_dat <- appendDetections(sync_dat)
	T0 <- min(sync_dat$detections$epo)

	inp_H_info 			<- getInpSyncHInfo(sync_dat)
	inp_toa_list 		<- getInpSyncToaList(sync_dat, max_epo_diff, min_hydros)
	fixed_hydros_vec 	<- getFixedHydrosVec(sync_dat, fixed_hydros_idx)
	offset_vals 		<- getOffsetVals(inp_toa_list, n_offset_day)
	ss_vals 			<- getSsVals(inp_toa_list, n_ss_day)

	dat_tmb_sync <- getDatTmbSync(inp_H_info, time_keeper_idx, inp_toa_list, fixed_hydros_vec, offset_vals, ss_vals, T0)
	params_tmb_sync <- getParamsTmbSync(dat_tmb_sync)
	random_tmb_sync <- c("TOP", "OFFSET", "SLOPE1", "SLOPE2", "SS", "TRUE_H")
	inits_tmb_sync <- c(6, rep(-3,dat_tmb_sync$nh))
	inp_params <- list(T0=T0)

	return(list(dat_tmb_sync=dat_tmb_sync, params_tmb_sync=params_tmb_sync, random_tmb_sync=random_tmb_sync, inits_tmb_sync=inits_tmb_sync, inp_params=inp_params))
	
}

#' Internal function. Get hydros for sync from sync_dat
#' @inheritParams getInpSync
getInpSyncHInfo <- function(sync_dat){
	Hx0 <- sync_dat$hydros[1,x]
	Hy0 <- sync_dat$hydros[1,y]

	inp_H <- sync_dat$hydros[, c('x','y','z')]
	inp_H[, x:=x-Hx0]
	inp_H[, y:=y-Hy0]
	inp_H[]
	
	return(list(inp_H=inp_H, Hx0=Hx0, Hy0=Hy0))
}


#' Internal function. Get toa for sync from sync_dat
#' @inheritParams getInpSync
getInpSyncToaList <- function(sync_dat, max_epo_diff, min_hydros){
	toa_list_gross <- buildToaListGross(sync_dat)
	toa_list_pruned <- pruneToaListGross(toa_list_gross, max_epo_diff, min_hydros)

	return(toa_list_pruned)
}



#' Internal function to get vector of which hydros are fixed
getFixedHydrosVec <- function(sync_dat, fixed_hydros_idx){
	fixed_hydros_vec <- rep(0, times=nrow(sync_dat$hydros))
	fixed_hydros_vec[fixed_hydros_idx] <- 1
	
	return(fixed_hydros_vec)
}


#' Internal function to get info relating to sync offsets per day
getOffsetVals <- function(inp_toa_list, n_offset_day){
	epo_self_vec <- inp_toa_list$epo_self_vec
	epo_start <- min(epo_self_vec)
	epo_end   <- max(epo_self_vec)
	
	n_offset_idx <- ceiling((epo_end - epo_start)/(24*60*60)) * n_offset_day
	offset_cuts <- cut(epo_self_vec, breaks=n_offset_idx)
	offset_idx <- as.numeric(offset_cuts)
	offset_labs <- levels(offset_cuts)
	offset_levels <- 	cbind(lower = as.numeric( sub("\\((.+),.*", "\\1", offset_labs) ),	  upper = as.numeric( sub("[^,]*,([^]]*)\\]", "\\1", offset_labs) ))
	
	return(list(n_offset_idx=n_offset_idx, offset_idx=offset_idx, offset_levels=offset_levels))
}


#' Internal function to get info relating to sync ss per day
getSsVals <- function(inp_toa_list, n_ss_day){
	epo_self_vec <- inp_toa_list$epo_self_vec
	epo_start <- min(epo_self_vec)
	epo_end   <- max(epo_self_vec)
	
	n_ss_idx <- ceiling((epo_end - epo_start)/(24*60*60)) * n_ss_day
	ss_cuts <- cut(epo_self_vec, breaks=n_ss_idx)
	ss_idx <- as.numeric(ss_cuts)
	ss_labs <- levels(ss_cuts)
	ss_levels <- 	cbind(lower = as.numeric( sub("\\((.+),.*", "\\1", ss_labs) ),	  upper = as.numeric( sub("[^,]*,([^]]*)\\]", "\\1", ss_labs) ))
	
	return(list(n_ss_idx=n_ss_idx, ss_idx=ss_idx, ss_levels=ss_levels))
}

#' Internal function to get dat for TMB sync
getDatTmbSync <- function(inp_H_info, time_keeper_idx, inp_toa_list, fixed_hydros_vec, offset_vals, ss_vals, T0){
	dat_tmb_sync <- list(
		H=as.matrix(inp_H_info$inp_H),
		toa=inp_toa_list$toa - T0,
		sync_tag_idx_vec = inp_toa_list$sync_tag_idx_vec-1,
		np = nrow(inp_toa_list$toa),
		nh = ncol(inp_toa_list$toa),
		tk = time_keeper_idx,
		fixed_hydros_vec = fixed_hydros_vec,
		offset_idx = offset_vals$offset_idx-1,
		n_offset_idx = offset_vals$n_offset_idx,
		ss_idx = ss_vals$ss_idx-1,
		n_ss_idx = ss_vals$n_ss_idx
	)
	return(dat_tmb_sync)
}


#' Internal function to get params for TMB sync
getParamsTmbSync <- function(dat_tmb_sync){
	params_tmb_sync <- list(
		TOP = 	rowMeans(dat_tmb_sync$toa, na.rm=TRUE),
		OFFSET = matrix(rnorm(dat_tmb_sync$nh*dat_tmb_sync$n_offset_idx, 0, 3), nrow=dat_tmb_sync$nh, ncol=dat_tmb_sync$n_offset_idx),
		SLOPE1 = matrix(rnorm(dat_tmb_sync$nh*dat_tmb_sync$n_offset_idx, 0, 3), nrow=dat_tmb_sync$nh, ncol=dat_tmb_sync$n_offset_idx),
		SLOPE2 = matrix(rnorm(dat_tmb_sync$nh*dat_tmb_sync$n_offset_idx, 0, 3), nrow=dat_tmb_sync$nh, ncol=dat_tmb_sync$n_offset_idx),
		SS = rnorm(dat_tmb_sync$n_ss_idx, 1420, 1),
		TRUE_H = as.matrix(cbind(dat_tmb_sync$H[,1], dat_tmb_sync$H[,2])),
		LOG_SIGMA_TOA = 0,
		LOG_SIGMA_HYDROS_XY = rnorm(dat_tmb_sync$nh,-3,1)
	)
	return(params_tmb_sync)
}


