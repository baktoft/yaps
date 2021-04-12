#' Internal function to get dat for TMB sync
#' @inheritParams getInpSync
#' @noRd
getDatTmbSync <- function(sync_type, sync_dat, keep_rate, time_keeper_idx, inp_toa_list, fixed_hydros_vec, offset_vals, ss_vals, inp_H_info, T0, ss_data_what, ss_data_vec){
	H <- as.matrix(inp_H_info$inp_H)
	dimnames(H) <- NULL
	
	toa <- inp_toa_list$toa - T0
	toa_offset <- inp_toa_list$toa - offset_vals$offset_levels[offset_vals$offset_idx]
	
	if(sync_type == 'delta'){
		dat_delta <- getDatDelta(toa, inp_toa_list, keep_rate, time_keeper_idx, offset_vals, ss_vals)
		model <- "yaps_sync_delta"
	} else {
		dat_delta <- c(0)
		
		model <- "yaps_sync_top"
	}

	dat_tmb_sync <- list(
		model = model,
		H=H,
		toa = toa,
		toa_offset=toa_offset,
		dat_delta = dat_delta,
		sync_tag_idx_vec = inp_toa_list$sync_tag_idx_vec,
		np = nrow(inp_toa_list$toa),
		nh = ncol(inp_toa_list$toa),
		ndelta = nrow(dat_delta),
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

