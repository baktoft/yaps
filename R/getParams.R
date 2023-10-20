#' Internal function - get params-list for use in TMB
#'
#' Compile a list of parameters for use in TMB.
#' @param datTmb Object obtained using getDatTmb()
#' @return List of params for use in TMB
#' @noRd
getParams <- function(datTmb){
	params_XY <- yaps:::getParamsXYFromCOA(datTmb)
	out <- list(
		  X = params_XY$X + stats::rnorm(ncol(datTmb$toa), sd=10)
		, Y = params_XY$Y + stats::rnorm(ncol(datTmb$toa), sd=10)
		  # X = 0 + stats::rnorm(ncol(datTmb$toa), sd=10)
		# , Y = 0 + stats::rnorm(ncol(datTmb$toa), sd=10)
		, top = zoo::na.approx(apply(datTmb$toa, 2, function(k) {stats::median(k, na.rm=TRUE)}), rule=2)	#time of ping
		# , SS=stats::rnorm(datTmb$n_ss, 1450, 5) 	#speed of sound
		, logD_xy = 0				#diffusivity of transmitter movement (D_xy in ms)
		# , logD_v = 0				#diffusivity of speed of sound (D_v in ms)
		# , logSigma_toa = 0			#sigma for Gaussian
		# , logScale = 0				#scale parameter for t-distribution
		# , log_t_part = 0				#Mixture ratio between Gaussian and t
	)
	
	# # # If estimating 3D	
	if(datTmb$how_3d == "est"){
		out$Z <- stats::runif(ncol(datTmb$toa), -10, 0)
		out$logD_z <- 0				#diffusivity of transmitter vertical movement (D_z in ms)
	}
	

	# # # ss related
	if(datTmb$ss_data_what == 'est'){
		out$logD_v <- 0				#diffusivity of speed of sound (D_v in ms)
		out$SS <- stats::rnorm(datTmb$n_ss, 1450, 5) 	#speed of sound
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
	if(datTmb$pingType %in% c('sbi', 'sbi_double', 'rbi')){
		out$logSigma_bi <- 0			#sigma  burst interval (sigma_bi in ms)
	}
	
	if(datTmb$pingType == 'pbi'){
		out$logSigma_bi <- 0			#sigma  burst interval (sigma_bi in ms)
		out$tag_drift <- stats::rnorm(datTmb$np, 0, 1e-2)
	}

	
	return(out)
}
