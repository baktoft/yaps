
#' Internal function to get data for checking sync_model
#' @param extreme_threshold Ignore delta values larger than this threshold. 
#' @inheritParams getInpSync
#' @noRd
getSyncCheckDat_old <- function(sync_model, extreme_threshold=1000){
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
