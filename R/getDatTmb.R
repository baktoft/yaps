#' Internal function - get data for input to TMB
#'
#' Compile data for input to TMB.
#' @param inp_params Selection of parameters used to setup and run YAPS.
#' @inheritParams getInp
#'
#' @return List for use in TMB.
#' @noRd
getDatTmb <- function(hydros, toa, E_dist, n_ss, pingType, rbi_min, rbi_max, ss_data_what, ss_data, biTable, inp_params, z_vec, bbox){
	T0 <- inp_params$T0
	Hx0 <- inp_params$Hx0
	Hy0 <- inp_params$Hy0
	
	toa <- toa - T0
	
	# allowing slight out-of-bounds BIs
	rbi_min <- rbi_min - rbi_min * 0.05
	rbi_max <- rbi_max + rbi_max * 0.05
	
	# attempting to make sure toa is oriented correct
	if(!nrow(toa) == nrow(hydros)){
		toa <- t(toa)
	}

	if(n_ss > 1){
		ss_idx <- cut(1:ncol(toa), n_ss, labels=FALSE) - 1 #-1 because zero-indexing in TMB
	} else {
		ss_idx <- rep(0, ncol(toa))
	}
	approxBI <- mean(diff(colMeans(toa, na.rm=TRUE), na.rm=TRUE), na.rm=TRUE)

	if(ss_data_what == 'data') { stopifnot(length(ss_data) == ncol(toa))}

	Edist <- rep(0,3)
	if(E_dist == "Gaus") {Edist[1] <- 1}
	if(E_dist == "Mixture") {Edist[2] <- 1}
	if(E_dist == "t") {Edist[3] <- 1}
	
	if(is.null(z_vec)){
		how_3d <- 'none'
		z_vec <- c(1)
	} else if(z_vec[1] == "est") {
		how_3d <- 'est'
		z_vec <- c(1)
	} else {
		how_3d <- 'data'
	}
	
	if(is.null(bbox)){
		bbox <- NA
	} else {
		bbox[1] <- bbox[1] - inp_params$Hx0
		bbox[2] <- bbox[2] - inp_params$Hx0
		bbox[3] <- bbox[3] - inp_params$Hy0
		bbox[4] <- bbox[4] - inp_params$Hy0
	}

	datTmb <- list(
		model = "yaps_track",
		H = matrix(c(hydros$h_x-Hx0, hydros$h_y-Hy0, hydros$h_z), ncol=3),
		nh = nrow(hydros),
		np = ncol(toa),
		Edist = Edist,
		toa = toa,
		bi_epsilon = 1E-6,
		bi_penalty = 1E9,
		rbi_min = rbi_min,
		rbi_max = rbi_max,
		pingType = pingType,
		n_ss = n_ss,
		ss_idx = ss_idx,
		ss_data_what = ss_data_what,
		ss_data = ss_data,
		approxBI = approxBI,
		biTable = c(1),
		how_3d = how_3d,
		z_vec = z_vec,
		bbox = bbox
	)
	if(pingType == 'pbi') {datTmb$biTable = biTable}

	return(datTmb)
}
