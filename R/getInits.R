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
		if(datTmb$rbi_max >= 10){
			init_logSigma_bi <- 4 
		} else {
			init_logSigma_bi <- -3
		}
	} else if(datTmb$pingType == 'pbi'){
		init_logSigma_bi <- -5
	}
	
	init_logD_v <- 0
	init_logSigma_toa <- -3 # used in Gaussian and mixture
	init_logScale <- 1		# used in mixture and pure t
	init_log_t_part <- -4	# only used in mixture

	inits <- c(init_logD_xy)

	if(datTmb$how_3d == 'est'){
		init_logD_z <- 0
		inits <- c(inits, init_logD_z)
	}
	
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
		inits <- c(inits, init_logSigma_bi)#, 					init_logD_v)#, init_logSigma_toa, init_logScale, init_log_t_part)
	} else if (datTmb$pingType == 'pbi'){
		inits <- c(inits, init_logSigma_bi)#,  init_logD_v)#, init_logSigma_toa, init_logScale, init_log_t_part)
	}
	
	inits <- stats::rnorm(length(inits), mean=inits, sd=sdInits)
	return(inits)
}
