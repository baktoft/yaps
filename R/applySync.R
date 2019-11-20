#' Apply sync model to toa matrix to obtain synced data
applySync <- function(toa, sync_model, type=""){
	inp_synced <- sync_model$inp_synced

	ks <- inp_synced$inp_params$offset_levels[, 1]
	ks[1] <- ks[1] - inp_synced$inp_params$max_epo_diff

	if(type=="toa_matrix"){
		offset_idx_mat <- matrix(findInterval(toa, ks), ncol=ncol(toa))
		offset_level_mat <- matrix(inp_synced$inp_params$offset_levels[offset_idx_mat, 1], ncol=ncol(offset_idx_mat))
		
		toa_offset <- toa - offset_level_mat
		offset_hydro_idx <- as.matrix(reshape2::melt(offset_idx_mat, value.name="idx")[,c(2,3)])

		offset_mat <- matrix(sync_model$pl$OFFSET[offset_hydro_idx], ncol=ncol(toa))
		slope_mat <- matrix(sync_model$pl$SLOPE1[offset_hydro_idx], ncol=ncol(toa))
		slope2_mat <- matrix(sync_model$pl$SLOPE2[offset_hydro_idx], ncol=ncol(toa))

		toa_synced <- offset_level_mat + toa_offset - offset_mat - slope_mat*toa_offset/1E6 - slope2_mat*(toa_offset/1E6)^2
	}

	if(type=="detections_table"){
		offset_idx <- findInterval(toa$epofrac, ks)
		offset_idx <- NULL
		sync_dt <- data.table::data.table()
		sync_dt[, epofrac := toa$epofrac]
		sync_dt[, offset_idx:=findInterval(toa$epofrac, ks)]
		# NA those epofracs outside sync_period, i.e. offset_idx outside range 1:length(ks)
		sync_dt[!offset_idx %in% 1:length(ks), 'offset_idx'] <- NA
		sync_dt[, offset_level:= inp_synced$inp_params$offset_levels[offset_idx,1] ]
		sync_dt[, offset_hydro_idx:=toa$hydro_idx]
		sync_dt[, OFFSET:=sync_model$pl$OFFSET[offset_hydro_idx]]
		sync_dt[, SLOPE1:=sync_model$pl$SLOPE1[offset_hydro_idx]]
		sync_dt[, SLOPE2:=sync_model$pl$SLOPE2[offset_hydro_idx]]
		sync_dt[, eposync := epofrac - OFFSET - SLOPE1*(epofrac - offset_level)/1E6 - SLOPE2*((epofrac - offset_level)/1E6)^2]
		
		toa[, eposync := sync_dt[, eposync]]
		
		toa_synced <- toa
		toa_synced[]
	}
	
	return(toa_synced)
}

