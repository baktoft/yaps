#' Get sync model from inp_sync object obtained by getInpSync()
getSyncModel <- function(inp_sync, silent=TRUE){
	dat_tmb <- inp_sync$dat_tmb_sync
	params <- inp_sync$params_tmb_sync
	random <- inp_sync$random_tmb_sync
	inits <- inp_sync$inits_tmb_sync

	opt <- c()
	pl <- c()
	plsd <- c()
	obj <- c()

	obj <- TMB::MakeADFun(data = dat_tmb, parameters = params, random = random, DLL = "yaps_sync", inner.control = list(maxit = 100), silent=silent)
	opt <- stats::nlminb(inits,obj$fn,obj$gr)

	obj$fn()
	pl <- obj$env$parList()   # List of estimates
	obj_val <- opt$objective

	# jointrep <- try(TMB::sdreport(obj, getJointPrecision=TRUE), silent=silent)
	# param_names <- rownames(summary(jointrep))
	# sds <- summary(jointrep)[,2]
	# summ <- data.frame(param=param_names, sd=sds)
	# plsd <- split(summ[,2], f=summ$param)
	
	report <- obj$report()
	
	eps_long <- getEpsLong(report, pl, inp_sync)
	
	return(list(pl=pl, report=report, obj_val=obj_val, eps_long=eps_long, inp_synced=inp_sync))
}




#' Get object inp for synchronization
#' @param sync_dat List containing data.tables with hydrophone information and detections.
#' @export
getInpSync <- function(sync_dat, max_epo_diff, min_hydros, time_keeper_idx, fixed_hydros_idx, n_offset_day, n_ss_day){
	sync_dat <- appendDetections(sync_dat)
	T0 <- min(sync_dat$detections$epo)

	inp_toa_list 		<- getInpSyncToaList(sync_dat, max_epo_diff, min_hydros)
	fixed_hydros_vec 	<- getFixedHydrosVec(sync_dat, fixed_hydros_idx)
	offset_vals 		<- getOffsetVals(inp_toa_list, n_offset_day)
	ss_vals 			<- getSsVals(inp_toa_list, n_ss_day)

	dat_tmb_sync <- getDatTmbSync(time_keeper_idx, inp_toa_list, fixed_hydros_vec, offset_vals, ss_vals, T0)
	params_tmb_sync <- getParamsTmbSync(dat_tmb_sync)
	random_tmb_sync <- c("TOP", "OFFSET", "SLOPE1", "SLOPE2", "SS", "TRUE_H")
	inits_tmb_sync <- c(3, rep(-3,dat_tmb_sync$nh))
	inp_params <- list(toa=inp_toa_list$toa, T0=T0, offset_levels=offset_vals$offset_levels, ss_levels=ss_vals$ss_levels, max_epo_diff=max_epo_diff)

	return(list(dat_tmb_sync=dat_tmb_sync, params_tmb_sync=params_tmb_sync, random_tmb_sync=random_tmb_sync, inits_tmb_sync=inits_tmb_sync, inp_params=inp_params))
	
}


#' Internal function. Get toa for sync from sync_dat
#' @inheritParams getInpSync
getInpSyncToaList <- function(sync_dat, max_epo_diff, min_hydros, excl_self_detect=TRUE){
	toa_list_gross <- buildToaListGross(sync_dat, excl_self_detect)
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
	epo_start <- min(epo_self_vec)-10
	epo_end   <- max(epo_self_vec)+10
	
	n_offset_idx <- ceiling((epo_end - epo_start)/(24*60*60)) * n_offset_day
	offset_cuts <- cut(epo_self_vec, breaks=n_offset_idx, dig.lab=10)
	offset_idx <- as.numeric(offset_cuts)
	offset_labs <- levels(offset_cuts)
	offset_levels <- cbind(lower = as.numeric( sub("\\((.+),.*", "\\1", offset_labs) ),	  upper = as.numeric( sub("[^,]*,([^]]*)\\]", "\\1", offset_labs) ))
	dimnames(offset_levels) <- NULL
	return(list(n_offset_idx=n_offset_idx, offset_idx=offset_idx, offset_levels=offset_levels))
}


#' Internal function to get info relating to sync ss per day
getSsVals <- function(inp_toa_list, n_ss_day){
	epo_self_vec <- inp_toa_list$epo_self_vec
	epo_start <- min(epo_self_vec)-10
	epo_end   <- max(epo_self_vec)+10
	
	n_ss_idx <- ceiling((epo_end - epo_start)/(24*60*60)) * n_ss_day
	ss_cuts <- cut(epo_self_vec, breaks=n_ss_idx, dig.lab=10)
	ss_idx <- as.numeric(ss_cuts)
	ss_labs <- levels(ss_cuts)
	ss_levels <- 	cbind(lower = as.numeric( sub("\\((.+),.*", "\\1", ss_labs) ),	  upper = as.numeric( sub("[^,]*,([^]]*)\\]", "\\1", ss_labs) ))
	dimnames(ss_levels) <- NULL

	return(list(n_ss_idx=n_ss_idx, ss_idx=ss_idx, ss_levels=ss_levels))
}

#' Internal function to get dat for TMB sync
getDatTmbSync <- function(time_keeper_idx, inp_toa_list, fixed_hydros_vec, offset_vals, ss_vals, T0){
	inp_H_info <- getInpSyncHInfo(sync_dat)
	H <- as.matrix(inp_H_info$inp_H)
	dimnames(H) <- NULL
	
	toa_offset <- inp_toa_list$toa - offset_vals$offset_levels[offset_vals$offset_idx]

	dat_tmb_sync <- list(
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
		TRUE_H = as.matrix(cbind(dat_tmb_sync$H[,1], dat_tmb_sync$H[,2], dat_tmb_sync$H[,3])),
		LOG_SIGMA_TOA = 0,
		LOG_SIGMA_HYDROS_XY = rnorm(dat_tmb_sync$nh,-3,1)
	)
	return(params_tmb_sync)
}


#' Internal function to get residuals from sync_model in long format
getEpsLong <- function(report, pl, inp_sync){
	eps <- report$eps_toa
	eps[which(eps==0)] <- NA
	eps_long <- data.table::data.table(reshape2::melt(eps))
	colnames(eps_long) <- c('ping', 'hydro_idx', 'E')
	eps_long[, sync_tag_idx:=rep(inp_sync$dat_tmb_sync$sync_tag_idx_vec, times=ncol(eps))]
	eps_long[, ss:=rep(pl$SS[inp_sync$dat_tmb_sync$ss_idx], times=ncol(eps))]
	eps_long[, E_m:=E*ss]
	
	eps_long <- eps_long[!is.na(E)]
	
	return(eps_long)
}


#' Internal function to get data for checking sync_model
getSyncCheckDat <- function(sync_model){
	toa <- sync_model$inp_synced$inp_params$toa
	toa_sync <- applySync(toa, sync_model, type="toa_matrix")

	sync_tag_idx_vec <- sync_model$inp_synced$dat_tmb_sync$sync_tag_idx_vec
	ss_idx <- findInterval(rowMeans(toa, na.rm=TRUE), sync_model$inp_synced$inp_params$ss_levels[,1])

	true_x <- sync_model$pl$TRUE_H[,1]
	true_y <- sync_model$pl$TRUE_H[,2]
	true_z <- sync_model$pl$TRUE_H[,3]

	ss_long <- sync_model$pl$SS[ss_idx]

	toa_sync_long <- data.table::data.table(reshape2::melt(toa_sync))
	colnames(toa_sync_long) <- c('ping_idx','hydro_idx', 'toa_sync')
	toa_sync_long[, sync_tag_idx:=rep(sync_tag_idx_vec, times=ncol(toa))]
	toa_sync_long[, dist_to_sync_tag:= sqrt((true_x[hydro_idx] - true_x[sync_tag_idx])^2 + (true_y[hydro_idx] - true_y[sync_tag_idx])^2 + (true_z[hydro_idx] - true_z[sync_tag_idx])^2)]
	toa_sync_long[, ss:=rep(ss_long, times=ncol(toa))]
	
	sync_check_dat <- c()
	for(i in 1:19){
		sync_check_dat_i <- toa_sync_long[, .(focal_hydro_idx=i, hydro_idx, ping_idx, delta=abs(((toa_sync - toa_sync[hydro_idx==i])*ss) - (dist_to_sync_tag - dist_to_sync_tag[hydro_idx==i]))), by=c('sync_tag_idx')]
		sync_check_dat_i <- sync_check_dat_i[delta!= 0]
		sync_check_dat <- rbind(sync_check_dat, sync_check_dat_i)
	}
	
	return(sync_check_dat)

}
