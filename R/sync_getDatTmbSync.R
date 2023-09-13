#' Internal function to get dat for TMB sync
#' @inheritParams getInpSync
#' @noRd
#' @export
getDatTmbSync <- function(hydros, dat_sync, sync_params, inp_toa_list, offset_vals, T0, ss_data_vec){
	# H <- as.matrix(inp_H_info$inp_H)
	H <- as.matrix(hydros[, .(x,y,z)])
	dimnames(H) <- NULL
	
	toa <- inp_toa_list$toa - T0
	toa_offset <- inp_toa_list$toa - offset_vals$offset_levels[offset_vals$offset_idx]
	
	if(sync_params$E_dist == 'Gaus' | sync_params$E_dist == 'gaus'){
		E_model <- 1
	} else if(sync_params$E_dist == 't'){
		E_model <- 2
	}
	
	time_keeper_idx <- hydros[h_sn == sync_params$time_keeper, h_idx]

	fixed_hydros_vec 		<- getFixedHydrosVec(hydros, sync_params)
	
	if(ss_data_vec[1] == 0){
		ss_data_how <- "est"
	} else{
		ss_data_how <- 'data'
	}


	dat_tmb_sync <- list(
		model = 'yaps_sync', 
		H=H,
		toa = toa,
		toa_offset = toa_offset,
		sync_tag_idx_vec = inp_toa_list$sync_tag_idx_vec,
		np = nrow(inp_toa_list$toa),
		nh = ncol(inp_toa_list$toa),
		tk = time_keeper_idx,
		fixed_hydros_vec = fixed_hydros_vec,
		offset_idx = offset_vals$offset_idx,
		n_offset_idx = offset_vals$n_offset_idx,
		ss_data_vec = ss_data_vec,
		E_model = E_model
	)
	return(dat_tmb_sync)
}

