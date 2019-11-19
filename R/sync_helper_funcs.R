
#' Internal function to append needed columns to table detections 
#' @inheritParams getInpSync
appendDetections <- function(sync_dat){
	sync_dat$detections[, hydro_idx := data.table::merge.data.table(sync_dat$detections, sync_dat$hydros[, c('serial','idx')], by='serial', sort=FALSE)$idx]
	sync_dat$detections[, epo := as.numeric(ts)]
	sync_dat$detections[, epofrac := epo+frac]
	return(sync_dat)
}

#' Internal function to build gross TOA matrix, sync_tag_vec and epo_self_vec - the matrix needs pruning before use in sync function
#' @inheritParams getInpSync
buildToaListGross <- function(sync_dat){
	hydros <- sync_dat$hydros
	detections <- sync_dat$detections
	sync_tags <- hydros[!is.na(sync_tag), sync_tag]

	toa <- matrix(nrow=0, ncol=nrow(hydros))
	sync_tag_idx_vec <- c()
	epo_self_vec <-c()
	for(i in 1:length(sync_tags)){
		hydro_i_idx <- hydros[sync_tag == sync_tags[i], idx]
		sync_tag_i_idx <- hydro_i_idx
		# hydro_st_serial <- hydros[sync_tag == sync_tags[st], serial]
		self_detections <- detections[tag==sync_tags[i] & hydro_idx==hydro_i_idx]
		other_detections <- detections[tag==sync_tags[i] & hydro_idx!=hydro_i_idx]
		self_detections[, roll:=epo]
		other_detections[, roll:=epo]

		toa_i <- t(plyr::daply(.data=other_detections, .variables="hydro_idx", .fun=function(k){
			k <- data.table::data.table(k)
			return(as.numeric(k[self_detections, roll="nearest", on=.(roll)]$epofrac))
		}))

		# hydros[serial %in% as.numeric(colnames(toa_i)), idx]
		
		toa_i_mat <- matrix(nrow=nrow(toa_i), ncol=nrow(hydros))
		toa_i_mat[, as.numeric(colnames(toa_i))] <- toa_i
		toa_i_mat[, hydro_i_idx] <- self_detections$epofrac

		toa <- rbind(toa, toa_i_mat)
		sync_tag_idx_vec <- c(sync_tag_idx_vec, rep(sync_tag_i_idx, nrow(toa_i_mat)))
		epo_self_vec <- c(epo_self_vec, self_detections$epofrac)
	}
	
	return(list(toa = toa, sync_tag_idx_vec=sync_tag_idx_vec, epo_self_vec=epo_self_vec))
}

#' Internal function to prune the object obtained by buildToaListGross
#' @inheritParams getInpSync
pruneToaListGross <- function(toa_list_gross, max_epo_diff, min_hydros){
	toa <- toa_list_gross$toa
	epo_self_vec <- toa_list_gross$epo_self_vec
	sync_tag_idx_vec <- toa_list_gross$sync_tag_idx_vec
	
	toa[which(abs(toa - epo_self_vec) > max_epo_diff)] <- NA

	nobs <- apply(toa, 1, function(k) sum(!is.na(k)))
	keepers <- which(nobs >= min_hydros)
	toa <- toa[keepers ,]
	sync_tag_idx_vec <- sync_tag_idx_vec[keepers]
	epo_self_vec <- epo_self_vec[keepers]
	
	return(list(toa=toa, sync_tag_idx_vec=sync_tag_idx_vec, epo_self_vec=epo_self_vec))

}
