#' Apply sync model to toa matrix to obtain synced data
#'
#' @param toa Object containing data to be synchronized. Typically a `data.table` as e.g. `ssu1$detections`, but can also be a matrix dim=(n_ping, n_hydo).
#' @param hydros data.table formatted as `ssu1$hydros`
#' @param sync_model Synchronization model obtained using `getSyncModel()`
#'
#' @export
#' @return A `data.table` with the now synchronized time-of-arrivals in column `eposync`. 
#' @example man/examples/example-yaps_ssu1.R
applySync <- function(toa, hydros="", sync_model){
	if(is.matrix(toa)) {type <- "toa_matrix"
	} else if(data.table::is.data.table(toa)) {type <- "detections_table"}

	inp_synced <- sync_model$inp_synced

	ks <- inp_synced$inp_params$offset_levels[, 1]
	ks[1] <- ks[1] - inp_synced$inp_params$max_epo_diff
	
	
	if(type=="toa_matrix"){
		# # # 2021-09-14 - DOH! the toa from sync_model already has lin_corr_coeffs applied...
		# if(sum(inp_synced$inp_params$lin_corr_coeffs != 0)){
			# stop("ERROR: Use of linear correction is not yet implemented in applying sync to a matrix!\n If linear corrections are used in sync, these are ignored in this step and results will be wrong!\n")
		# }
		
		# lin_corr_coeffs <- sync_model$inp_synced$inp_params$lin_corr_coeffs

		# subtract the intercept using sweep
		# ...and multiply the slope
		# toa_lin_corr <- sweep(toa, 2, lin_corr_coeffs[,1]) - t(t(toa) * lin_corr_coeffs[,2])
		
		offset_idx_mat <- matrix(findInterval(toa, ks), ncol=ncol(toa))
		offset_level_mat <- matrix(inp_synced$inp_params$offset_levels[offset_idx_mat, 1], ncol=ncol(offset_idx_mat))
		
		toa_offset <- toa - offset_level_mat
		# toa_offset <- toa_lin_corr - offset_level_mat
		offset_hydro_idx <- as.matrix(reshape2::melt(offset_idx_mat, value.name="idx")[,c(2,3)])

		offset_mat <- matrix(sync_model$pl$OFFSET[offset_hydro_idx], ncol=ncol(toa))
		slope_mat <- matrix(sync_model$pl$SLOPE1[offset_hydro_idx], ncol=ncol(toa))
		slope2_mat <- matrix(sync_model$pl$SLOPE2[offset_hydro_idx], ncol=ncol(toa))

		toa_synced <- offset_level_mat + toa_offset - offset_mat - slope_mat*toa_offset/1E6 - slope2_mat*(toa_offset/1E6)^2
		
		# sync_dt[, epofrac_lin_corr := epofrac - lin_corr_coeffs_offset - lin_corr_coeffs_slope*epofrac]
		# sync_dt[, eposync := epofrac_lin_corr - OFFSET - SLOPE1*(epofrac_lin_corr - offset_level)/1E6 - SLOPE2*(((epofrac_lin_corr - offset_level)/1E6)^2)]

	}

	if(type=="detections_table"){

		# if(!'epofrac' %in% colnames(toa)) {toa[, epofrac:=epo+frac]}
		# resetting epofrac - this might have been changed by other function previously, e.g. related to lin_corr_coeffs
		toa[, epofrac:=epo+frac]
		if(!'hydro_idx' %in% colnames(toa)){
			toa[, hydro_idx := merge(toa, hydros[, c('serial','idx')], by='serial', sort=FALSE)$idx]
		}

		sync_dt <- data.table::data.table()
		sync_dt[, epofrac := toa$epofrac]
		sync_dt[, hydro_idx := toa$hydro_idx]
		sync_dt[, id:=1:.N]
		sync_dt[, offset_idx:=findInterval(toa$epofrac, ks)]
		# NA those epofracs outside sync_period, i.e. offset_idx outside range 1:length(ks)
		sync_dt[!offset_idx %in% 1:length(ks), 'offset_idx'] <- NA
		sync_dt[, offset_level:= inp_synced$inp_params$offset_levels[offset_idx,1] ]
		# sync_dt[, offset_hydro_idx:=toa$hydro_idx]
		
		lin_corr_coeffs <- data.table::data.table(sync_model$inp_synced$inp_params$lin_corr_coeffs)
		colnames(lin_corr_coeffs) <- c('lin_corr_coeffs_offset','lin_corr_coeffs_slope')
		lin_corr_coeffs[, hydro_idx:=1:.N]

		OFFSET_long <- data.table::data.table(reshape2::melt(sync_model$pl$OFFSET))
		colnames(OFFSET_long) <- c('hydro_idx', 'offset_idx', 'OFFSET')
		SLOPE1_long <- data.table::data.table(reshape2::melt(sync_model$pl$SLOPE1))
		colnames(SLOPE1_long) <- c('hydro_idx', 'offset_idx', 'SLOPE1')
		SLOPE2_long <- data.table::data.table(reshape2::melt(sync_model$pl$SLOPE2))
		colnames(SLOPE2_long) <- c('hydro_idx', 'offset_idx', 'SLOPE2')
		
		sync_dt <- merge(sync_dt, lin_corr_coeffs, sort=FALSE, all.x=TRUE)
		
		sync_dt <- merge(sync_dt, OFFSET_long, sort=FALSE, all.x=TRUE)
		sync_dt <- merge(sync_dt, SLOPE1_long, sort=FALSE, all.x=TRUE)
		sync_dt <- merge(sync_dt, SLOPE2_long, sort=FALSE, all.x=TRUE)

		# sync_dt[, sync_model$pl$OFFSET[hydro_idx, offset_idx]]

		# sync_dt[, SLOPE1:=sync_model$pl$SLOPE1[hydro_idx, offset_idx]]
		# sync_dt[, SLOPE2:=sync_model$pl$SLOPE2[hydro_idx, offset_idx]]

		# table(sync_dt$hydro_idx)
		# table(sync_dt$offset_idx)
		# table(sync_dt$OFFSET)
		# table(sync_dt$OFFSET, sync_dt$hydro_idx)
		
		sync_dt[, epofrac_lin_corr := epofrac - lin_corr_coeffs_offset - lin_corr_coeffs_slope*epofrac]
		sync_dt[, eposync := epofrac_lin_corr - OFFSET - SLOPE1*(epofrac_lin_corr - offset_level)/1E6 - SLOPE2*(((epofrac_lin_corr - offset_level)/1E6)^2)]
		sync_dt[, slope1 := SLOPE1*(epofrac_lin_corr - offset_level)/1E6]
		sync_dt[, slope2 := SLOPE2*(((epofrac_lin_corr - offset_level)/1E6)^2)]
		
		
		toa[, eposync := sync_dt[, eposync]]
		# toa[tag==5138]
		
		toa_synced <- toa
		toa_synced[]
	}
	
	return(toa_synced)
}

