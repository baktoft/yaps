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
		H = matrix(c(hydros$hx-Hx0, hydros$hy-Hy0, hydros$hz), ncol=3),
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

#' Internal function - get params-list for use in TMB
#'
#' Compile a list of parameters for use in TMB.
#' @param datTmb Object obtained using getDatTmb()
#' @return List of params for use in TMB
#' @noRd
getParams <- function(datTmb){
	params_XY <- getParamsXYFromCOA(datTmb)
	out <- list(
		  X = params_XY$X + stats::rnorm(ncol(datTmb$toa), sd=10)
		, Y = params_XY$Y + stats::rnorm(ncol(datTmb$toa), sd=10)
		  # X = 0 + stats::rnorm(ncol(datTmb$toa), sd=10)
		# , Y = 0 + stats::rnorm(ncol(datTmb$toa), sd=10)
		, top = zoo::na.approx(apply(datTmb$toa, 2, function(k) {stats::median(k, na.rm=TRUE)}), rule=2)	#time of ping
		# , ss=stats::rnorm(datTmb$n_ss, 1450, 5) 	#speed of sound
		, logD_xy = 0				#diffusivity of transmitter movement (D_xy in ms)
		# , logD_v = 0				#diffusivity of speed of sound (D_v in ms)
		# , logSigma_toa = 0			#sigma for Gaussian
		# , logScale = 0				#scale parameter for t-distribution
		# , log_t_part = 0				#Mixture ratio between Gaussian and t
	)
	
	if(datTmb$how_3d == "est"){
		out$Z <- stats::runif(ncol(datTmb$toa), -10, 0)
	}
	
	# # # ss related
	if(datTmb$ss_data_what == 'est'){
		out$logD_v <- 0				#diffusivity of speed of sound (D_v in ms)
		out$ss <- stats::rnorm(datTmb$n_ss, 1450, 5) 	#speed of sound
	}
	
	# # # Edist related
	if(datTmb$Edist[1] == 1){
		out$logSigma_toa = 0			#sigma for Gaussian
	} else if(datTmb$Edist[2] == 1){
		out$logSigma_toa = 0			#sigma for Gaussian
		out$logScale = 0				#scale parameter for t-distribution
		out$log_t_part = 0				#Mixture ratio between Gaussian and t
	} else if(datTmb$Edist[3] == 1){
		out$logScale = 0				#scale parameter for t-distribution
	}

	# # # Ping type related
	if(datTmb$pingType %in% c('sbi', 'sbi_double')){
		out$logSigma_bi <- 0			#sigma  burst interval (sigma_bi in ms)
	}
	
	if(datTmb$pingType == 'pbi'){
		out$logSigma_bi <- 0			#sigma  burst interval (sigma_bi in ms)
		out$tag_drift <- stats::rnorm(datTmb$np, 0, 1e-2)
	}
	
	
	
	return(out)
}

#' Internal function - get initial values for X and Y based on Center Of Activity - i.e. hydrophones positions
#'
#' Attempts to give meaningful initial values for X and Y based on which hydros detected each ping
#' @inheritParams getInp
#' @noRd
getParamsXYFromCOA <- function(datTmb){
	toa <- datTmb$toa
	hydros <- datTmb$H

	toa_detect <- toa
	toa_detect[!is.na(toa_detect)] <- 1

	X <- zoo::na.approx(colMeans((toa_detect) * hydros[,1], na.rm=TRUE))
	Y <- zoo::na.approx(colMeans((toa_detect) * hydros[,2], na.rm=TRUE))
	
	return(list(X=X, Y=Y))

}

#' Get inits for use in TMB
#'
#' Compile a vector of initial values to use in TMB. One value for each estimated parameter (not random effects).
#' Should all be in a credible range.
#' @param datTmb Object obtained using getDatTmb()
#' @inheritParams getInp
#' @return Vector of initial values to use in TMB
#' @noRd
getInits <- function(datTmb, sdInits=1) {
	init_logD_xy <- -1

	if(datTmb$pingType == 'sbi') {
		init_logSigma_bi <- -6
	} else if(datTmb$pingType == 'rbi'){
		init_logSigma_bi <- 4 # not used in rbi
	} else if(datTmb$pingType == 'pbi'){
		init_logSigma_bi <- -5
	}
	
	init_logD_v <- 0
	init_logSigma_toa <- -3 # used in Gaussian and mixture
	init_logScale <- 1		# used in mixture and pure t
	init_log_t_part <- -4	# only used in mixture

	inits <- c(init_logD_xy)
	
	if(datTmb$ss_data_what == 'est'){
		inits <- c(inits, init_logD_v)
	}
	

	if(datTmb$Edist[1] == 1){
		inits <- c(inits, init_logSigma_toa)
	} else if(datTmb$Edist[2] == 1){
		inits <- c(inits, init_logSigma_toa, init_logScale, init_log_t_part)
	} else if(datTmb$Edist[3] == 1){
		inits <- c(inits, init_logScale)
	}
	
	if(datTmb$pingType == 'sbi'){
		inits <- c(inits, init_logSigma_bi)#,  init_logD_v)#, init_logSigma_toa, init_logScale, init_log_t_part)
	} else if (datTmb$pingType == 'rbi'){
		inits <- c(inits)#, 					init_logD_v)#, init_logSigma_toa, init_logScale, init_log_t_part)
	} else if (datTmb$pingType == 'pbi'){
		inits <- c(inits, init_logSigma_bi)#,  init_logD_v)#, init_logSigma_toa, init_logScale, init_log_t_part)
	}

	
	inits <- stats::rnorm(length(inits), mean=inits, sd=sdInits)
	return(inits)
}

#' Get bounds restricting the optimizer
#* 
#' Compile a matrix of lower (bounds[,1]) and upper (bounds[,2]) bounds for the parameters to be estimated.
#' @param datTmb Object obtained using getDatTmb()
#' @return Matrix of bounds restricting the optimizer when running runYaps().
#' @noRd
getBounds <- function(datTmb) {
	lu_logD_xy 			<- c(-50,  2)
	lu_logSigma_toa 	<- c(-12, -2)
	if(datTmb$Edist[2] == 1){ # mixture
		lu_logScale 	<- c(-30, 10)
	} else if (datTmb$Edist[3] == 1) { # t
		lu_logScale 	<- c(-10,2)
	}
	lu_log_t_part 		<- c(-100, 100)
	lu_logSigma_bi 		<- c(-20, -2)
	lu_logD_v 			<- c(-20,  2)

	bounds <- c()
	bounds <- rbind(bounds, lu_logD_xy)

	if(datTmb$ss_data_what == 'est'){
		bounds <- rbind(bounds, lu_logD_v)
	}

	if(datTmb$Edist[1] == 1){
		bounds <- rbind(bounds, lu_logSigma_toa)
	} else if(datTmb$Edist[2] == 1){
		bounds <- rbind(bounds, lu_logSigma_toa, lu_logScale, lu_log_t_part)
	} else if(datTmb$Edist[3] == 1){
		bounds <- rbind(bounds, lu_logScale)
	}
	
	if(datTmb$pingType == 'sbi'){
		bounds <- rbind(bounds, lu_logSigma_bi)
	} else if (datTmb$pingType == 'rbi'){
		bounds <- bounds
	} else if (datTmb$pingType == 'pbi'){
		bounds <- rbind(bounds, lu_logSigma_bi)
	}
	
	return(bounds)
}

#' Get parameters for this specific data set
#'
#' Compile a list of relevant parameters (e.g. T0) to use later on
#' @inheritParams getInp
#' @noRd
getInpParams <- function(hydros, toa, pingType){
	T0 <- min(toa, na.rm=TRUE)
		
	Hx0 <- hydros[1,hx]
	Hy0 <- hydros[1,hy]

	return(list(T0=T0, Hx0=Hx0, Hy0=Hy0))

}