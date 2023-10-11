#' Get data for check the sync model from sync_model
#' @export
getSyncCheckDat <- function(sync_model){
	# extract from sync model
	pl 			<- sync_model$pl
	inp_params 	<- sync_model$inp_synced$inp_params
	sync_params <- sync_model$inp_synced$sync_params
	gams 		<- sync_model$gams
	toa 		<- sync_model$inp_synced$inp_params$toa
	hydros 		<- sync_model$inp_synced$inp_params$hydros

	cat("Getting data to check the sync model... \n")

	if(is.na(gams[1])){
		cat("ERROR: Smoothed offsets are not estimated yet - run e.g. sync_model[['gams']] <- getSmoothOffsets(sync_model) before continuing. \n")
		stopSilent()
	}
	
	
	if(!is.null(sync_model$sync_check_dat)){
		return(sync_model)
	} 

	for(h in 1:ncol(toa)){
		gams[[h]]$newdata <- data.table(epofrac=toa[, h])
	}

	n_cores <- parallel::detectCores()-1
	cat("...running parallel using ",n_cores," cores\n")
	cl <- parallel::makeCluster(n_cores)
	pred_list <- parallel::parLapplyLB(cl, gams,  fun=function(k){
		mgcv::predict.gam(k, newdata=k$newdata)
	})
	parallel::stopCluster(cl)
	
	pred <- (matrix(unlist(pred_list), ncol=length(pred_list)))
	
	# predict(gams[h][[1]], newdata=data.table(epofrac_vec=toa[, h]))
	
	# get toa_synced matrix
	toa_synced <- toa - pred

	# get hydro-to-hydro distance matrix
	dist_mat <- getDistMat(hydros)

	# make toa_long using synced data
	toa_long <- data.table(reshape2::melt(toa_synced))
	colnames(toa_long) <- c('ping_idx','h_sn','eposync')
	toa_long <- merge(toa_long, hydros[, .(h_sn, h_idx)])
	toa_long[, sync_tag_idx := rep(sync_model$inp_synced$dat_tmb_sync$sync_tag_idx_vec, ncol(toa_synced))]
	toa_long[, offset_idx := rep(sync_model$inp_synced$dat_tmb_sync$offset_idx, ncol(toa_synced))]
	toa_long[, dist := dist_mat[cbind(h_idx, sync_tag_idx)]]
	
	if(sync_model$inp_synced$dat_tmb_sync$ss_vec[1] == 0){
		toa_long[, ss := sync_model$pl$SS[offset_idx]]
	} else {
		toa_long[, ss:= rep(sync_model$inp_synced$dat_tmb_sync$ss_vec, ncol(toa_synced))]
	}
	
	

	# create sync_check_dat
	sync_check_dat <- c()
	for(i in 1:ncol(toa)){
		sync_check_dat_i <- toa_long[, .(focal_h_idx=i, h_idx, offset_idx, ping_idx, delta=(((eposync - eposync[h_idx==i])*ss) - (dist - dist[h_idx==i]))), by=c('sync_tag_idx')]
		sync_check_dat_i <- sync_check_dat_i[delta!= 0]
		sync_check_dat   <- rbind(sync_check_dat, sync_check_dat_i)
	}
	
	sync_check_dat <- merge(sync_check_dat, hydros[, .(focal_hsn=h_sn, h_idx)], by.x='focal_h_idx', by.y='h_idx')
	sync_check_dat <- merge(sync_check_dat, hydros[, .(sync_tag, h_idx)], by.x='sync_tag_idx', by.y='h_idx')
	
	# sync_model$sync_check_dat <- sync_check_dat
	
	return(sync_check_dat)
}
