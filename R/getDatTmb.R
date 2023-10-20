#' Internal function - get data for input to TMB
#'
#' Compile data for input to TMB.
#' @param inp_params Selection of parameters used to setup and run YAPS.
#' @inheritParams getInp
#'
#' @return List for use in TMB.
#' @noRd
# getDatTmb <- function(hydros, toa, E_dist, n_ss, pingType, rbi_min, rbi_max, ss_data_what, ss_data, biTable, inp_params, z_vec, bbox){
getDatTmb <- function(hydros, toa, ss_data, yaps_params, inp_params){
	
	T0 <- inp_params$T0
	Hx0 <- inp_params$Hx0
	Hy0 <- inp_params$Hy0
	
	toa <- toa - T0
	
	# allowing slight out-of-bounds BIs
	rbi_min <- yaps_params$bi[1] - yaps_params$bi[1] * 0.05
	rbi_max <- yaps_params$bi[2] + yaps_params$bi[2] * 0.05
	
	
	if(yaps_params$n_ss > 1){
		ss_idx <- cut(1:nrow(toa), yaps_params$n_ssn_ss, labels=FALSE) - 1 #-1 because zero-indexing in TMB
	} else {
		ss_idx <- rep(0, nrow(toa))
	}
	
	approx_bi <- mean(diff(rowMeans(toa, na.rm=TRUE), na.rm=TRUE), na.rm=TRUE)

	if(ss_data != 'none' & length(ss_data) != nrow(toa) ){ 
		cat("ERROR: Seems like ss_data is provided, but length(ss_data) != nrow(toa) \n")
		cat("...getInp() found ss_data != 'none' and ",length(ss_data)," != ",nrow(toa)," \n")
		stopSilent()
	}

	E_dist_vec <- rep(0,3)
	if(yaps_params$E_dist == "Gaus") 	{E_dist_vec[1] <- 1}
	if(yaps_params$E_dist == "Mixture") {E_dist_vec[2] <- 1}
	if(yaps_params$E_dist == "t") 		{E_dist_vec[3] <- 1}
	
	if(z_data[1] == 'none'){
		how_3d <- 'none'
	} else if(z_data[1] == 'est'){
		how_3d <- 'est'
	} else {
		how_3d <- 'data'
	}
	
	if(yaps_params$bbox != TRUE){
		bbox <- NA
	} else {
		bbox <- getBbox(hydros)
		bbox[1] <- bbox[1] - inp_params$Hx0
		bbox[2] <- bbox[2] - inp_params$Hx0
		bbox[3] <- bbox[3] - inp_params$Hy0
		bbox[4] <- bbox[4] - inp_params$Hy0
	}

	datTmb <- list(
		model = "yaps_track",
		H = matrix(c(hydros$h_x, hydros$h_y, hydros$h_z), ncol=3),
		nh = nrow(hydros),
		np = nrow(toa),
		E_dist_vec = E_dist_vec,
		toa = toa,
		bi_epsilon = 1E-6,
		bi_penalty = 1E9,
		rbi_min = rbi_min,
		rbi_max = rbi_max,
		ping_type = yaps_params$ping_type,
		n_ss = yaps_params$n_ss,
		ss_idx = ss_idx,
		ss_data = ss_data,
		approx_bi = approx_bi,
		# biTable = c(1), # NOT IMPLEMENTED YET
		how_3d = how_3d,
		z_data = z_data,
		bbox = bbox
	)
	# if(pingType == 'pbi') {datTmb$biTable = biTable} # NOT IMPLEMENTED YET

	return(datTmb)
}
