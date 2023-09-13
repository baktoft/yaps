
#' Internal function to build sync TOA matrix, sync_tag_vec and epo_self_vec - the matrix needs pruning before use in sync function
#' @inheritParams getInpSync
#' @noRd
#' @export
# getSyncToa <- function(hydros, dat_sync, excl_self_detect, max_epo_diff, min_hydros){
getSyncToa <- function(hydros, dat_sync, sync_params){
	
	hs 		<- yapsifyHydros(hydros)
	dets 	<- dat_sync
	
	dets <- merge(dets, hs[, .(h_sn, h_idx)])
		
	sync_tags <- hs[!is.na(sync_tag), sync_tag]

	toa <- matrix(nrow=0, ncol=nrow(hs))
	colnames(toa) <- hs$h_sn
	sync_tag_idx_vec <- c()
	epo_self_vec <-c()
	for(i in 1:length(sync_tags)){
		hydro_i_idx <- 	hs[sync_tag == sync_tags[i], h_idx]
		sync_tag_i_idx <- hydro_i_idx
		# hydro_st_serial <- hs[sync_tag == sync_tags[st], serial]
		self_detections <- dets[tag==sync_tags[i] & h_idx==hydro_i_idx]
		# detects_on_others <- detections[tag==sync_tags[i] & hydro_idx!=hydro_i_idx]
		
		# hack added to support ref tags to serve as beacons without a hydro
		if(nrow(self_detections) == 0){
			num_detects_on_hydro <- dets[tag==sync_tags[i], .N, by=h_idx]
			best_hydro_idx <- num_detects_on_hydro[which.max(num_detects_on_hydro$N), h_idx]
			if(length(best_hydro_idx) == 0){
				self_detections <- NA #ugly hack added to support situations where hydros are completely ignored - e.g. due to very crappy performance
			} else {
				best_other_detections <- dets[tag==sync_tags[i] & h_idx==best_hydro_idx]
				self_detections <- best_other_detections
			}
		}
		
		other_detections <- dets[tag==sync_tags[i] & h_idx!=hydro_i_idx]
		
		# if no other hydro detected the sync_tag it is useless...
		if(nrow(other_detections) == 0){
			next
		}
		
		self_detections[, epo_roll:=epofrac]
		other_detections[, epo_roll:=epofrac]
		toa_i <- t(plyr::daply(.data=other_detections, .variables="h_idx", .fun=function(k){
			k <- data.table::data.table(k)
			return(as.numeric(k[self_detections, roll="nearest", on=.(epo_roll)]$epofrac))
		}))
		
		if(nrow(toa_i) == 1 ){
			toa_i <- t(toa_i)
		}

		# idx <- unique(other_detections$hydro_idx)
		# toa_i <- matrix(nrow=nrow(self_detections), ncol=length(idx))
		# colnames(toa_i) <- idx
		# for(j in 1:length(idx)){
			# k <- other_detections[hydro_idx == idx[j]]
			# toa_i[, j] <- 
			# as.numeric(k[self_detections, roll="nearest", on=.(epo_roll)]$epofrac)
		# }
		
		toa_i_mat <- matrix(nrow=nrow(toa_i), ncol=nrow(hs))
		toa_i_mat[, as.numeric(colnames(toa_i))] <- toa_i
		toa_i_mat[, hydro_i_idx] <- self_detections$epofrac

		if(sync_params$excl_self_detect){
			toa_i_mat[, hydro_i_idx] <- NA
		}


		toa <- rbind(toa, toa_i_mat)
		sync_tag_idx_vec <- c(sync_tag_idx_vec, rep(sync_tag_i_idx, nrow(toa_i_mat)))
		epo_self_vec <- c(epo_self_vec, self_detections$epofrac)
	}

	# pruning the gross list...
	toa[which(abs(toa - epo_self_vec) > sync_params$max_epo_diff)] <- NA

	nobs <- apply(toa, 1, function(k) sum(!is.na(k)))
	keepers <- which(nobs >= sync_params$min_hydros)
	toa <- toa[keepers ,]
	sync_tag_idx_vec <- sync_tag_idx_vec[keepers]
	epo_self_vec <- epo_self_vec[keepers]
	
	return(list(toa=toa, sync_tag_idx_vec=sync_tag_idx_vec, epo_self_vec=epo_self_vec))
}
