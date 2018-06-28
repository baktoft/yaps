#' Get prepared inp-object for use in TMB-call
#'
#' Wrapper-function to compile a list of inoput needed to run TMB
#' @param hydros Data frame from simHydros()
#' @param toa TOA-matrix
#' @param E_dist Which distribution to use in the model - 0=Gaussian, 1=Mixture of Gauss and t
#' @param n_ss Number of speed of sound to estimate
#' @param pingType Type of transmitter to simulate - either stable burst interval (sbi) or random burst interval (rbi)
#' @param rbi_min,rbi_max Minimum and maximum BI for random burst interval transmitters
#' @param sdInits If >0 initial values will be randomized around the normally fixed value using rnorm(length(inits), mean=inits, sd=sdInits)


#' @return List of input data ready for use in TMB-call
#' @export
getInp <- function(hydros, toa, E_dist, n_ss, pingType, sdInits=1, rbi_min=0, rbi_max=0){
	datTmb <- getDatTmb(hydros, toa, E_dist, n_ss, pingType, rbi_min, rbi_max)
	params <- getParams(datTmb)
	inits <- getInits(pingType, sdInits)
	return(list(
		datTmb = datTmb,
		params= params,
		inits = inits)
	)
}


#' Get data for input to TMB
#'
#' Compile data for input to TMB.
#' @inheritParams getInp
#'
#' @return List for use in TMB.
#' @export
getDatTmb <- function(hydros, toa, E_dist, n_ss, pingType, rbi_min, rbi_max){
	if(n_ss > 1){
		ss_idx <- cut(1:ncol(toa), n_ss, labels=FALSE) - 1 #-1 because zero-indexing in TMB
	} else {
		ss_idx <- rep(0, ncol(toa))
	}
	approxBI <- mean(diff(toa[1,]), na.rm=TRUE)
	
	Edist <- rep(0,2)
	if(E_dist == "Gaus") {Edist[1] <- 1}
	if(E_dist == "Mixture") {Edist[2] <- 1}
	
	datTmb <- list(
		H = matrix(c(hydros$hx, hydros$hy), ncol=2),
		nh = nrow(hydros),
		np = ncol(toa),
		Edist = Edist,
		toa = toa,
		bi_epsilon = 1e-6,
		bi_penalty = 1*1e9,
		rbi_min = rbi_min, 
		rbi_max = rbi_max,
		pingType = pingType,
		n_ss = n_ss,
		ss_idx = ss_idx,
		approxBI = approxBI
	)
	return(datTmb)
}

#' Get params-list for use in TMB
#'
#' Compile a list of parameters for use in TMB.
#' @param datTmb Object obtained using getDatTmb()
#' @return List of params for use in TMB
#' @export
getParams <- function(datTmb){
	list(
		  X = 0 + stats::rnorm(ncol(datTmb$toa), sd=10)
		, Y = 0 + stats::rnorm(ncol(datTmb$toa), sd=10)
		, top = zoo::na.approx(apply(datTmb$toa, 2, function(k) {stats::median(k, na.rm=TRUE)}), rule=2)	#time of ping
		, ss=stats::rnorm(datTmb$n_ss, 1412, 2) 	#speed of sound
		, logD_xy = 0				#diffusivity of transmitter movement (D_xy in ms)
		, logSigma_bi = 0			#sigma  burst interval (sigma_bi in ms)
		, logD_v = 0				#diffusivity of speed of sound (D_v in ms)
		, logSigma_toa = 0			#sigma for Gaussian 
		, logScale = 0				#scale parameter for t-distribution
		, log_t_part = 0				#Mixture ratio between Gaussian and t
	)
}

#' Get inits for use in TMB
#'
#' Compile a vector of initial values to use in TMB. One value for each estimated parameter (not random effects).
#' Should all be in a credible range.
#' @inheritParams getInp
#' @return Vector of initial values to use in TMB
#' @export
getInits <- function(pingType, sdInits=1) {
	init_logD_xy <- 1
	if(pingType == 'sbi') {
		init_logSigma_bi <- -6
	} else if(pingType == 'rbi'){
		init_logSigma_bi <- 4
	} else if(pingType == 'pbi'){
		init_logSigma_bi <- -5
	}
	init_logD_v <- 0
	init_logSigma_toa <- -3
	init_logScale <- 1
	init_log_t_part <- -4
	inits <- c(init_logD_xy, init_logSigma_bi,  init_logD_v, init_logSigma_toa, init_logScale, init_log_t_part)

	inits <- stats::rnorm(length(inits), mean=inits, sd=sdInits)
	return(inits)
}
