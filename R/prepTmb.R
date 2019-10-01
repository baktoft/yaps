#' Get prepared inp-object for use in TMB-call
#'
#' Wrapper-function to compile a list of input needed to run TMB
#' @param hydros Dataframe from simHydros() or Dataframe with columns hx and hy containing positions of the receivers. Translate the coordinates to get the grid centre close to (0;0).
#' @param toa TOA-matrix: matrix with receivers in rows and detections in columns. Make sure that the receivers are in the same order as in hydros, and that the matrix is very regular: one ping per column (inlude empty columns if a ping is not detected).
#' @param E_dist Which distribution to use in the model - "Gaus" = Gaussian, "Mixture" = mixture of Gaussian and t
#' @param n_ss Number of soundspeed estimates: one estimate per hour is usually enough
#' @param pingType Type of transmitter to simulate - either stable burst interval ('sbi'), random burst interval ('rbi') or pseudo-random burst interval ('pbi')
#' @param rbi_min,rbi_max Minimum and maximum BI for random burst interval transmitters
#' @param sdInits If >0 initial values will be randomized around the normally fixed value using rnorm(length(inits), mean=inits, sd=sdInits)
#' @param ss_data_what What speed of sound (ss) data to be used. Default ss_data_what='est': ss is estimated by the model. Alternatively, if ss_data_what='data': ss_data must be provided and length(ss_data) == ncol(toa)
#' @param ss_data Vector of ss-data to be used if ss_data_what = 'est'. Otherwise ss_data <- 0 (default)
#' @param biTable Table of known burst intervals. Only used when pingType == "pbi". Default=NULL


#' @return List of input data ready for use in TMB-call
#' @export
getInp <- function(hydros, toa, E_dist, n_ss, pingType, sdInits=1, rbi_min=0, rbi_max=0, ss_data_what='est', ss_data=0, biTable=NULL){
	datTmb <- getDatTmb(hydros, toa, E_dist, n_ss, pingType, rbi_min, rbi_max, ss_data_what, ss_data, biTable)
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
getDatTmb <- function(hydros, toa, E_dist, n_ss, pingType, rbi_min, rbi_max, ss_data_what, ss_data, biTable){
	if(n_ss > 1){
		ss_idx <- cut(1:ncol(toa), n_ss, labels=FALSE) - 1 #-1 because zero-indexing in TMB
	} else {
		ss_idx <- rep(0, ncol(toa))
	}
	approxBI <- mean(diff(toa[1,]), na.rm=TRUE)
	
	if(ss_data_what == 'data') { stopifnot(length(ss_data) == ncol(toa))}
	
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
		ss_data_what = ss_data_what,
		ss_data = ss_data,
		approxBI = approxBI,
		biTable = c(1)
	)
	if(pingType == 'pbi') {datTmb$biTable = biTable}

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
		, tag_drift = stats::rnorm(datTmb$np, 0, 1e-2)
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
