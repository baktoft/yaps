#' Get bounds restricting the optimizer
#* 
#' Compile a matrix of lower (bounds[,1]) and upper (bounds[,2]) bounds for the parameters to be estimated.
#' @param datTmb Object obtained using getDatTmb()
#' @return Matrix of bounds restricting the optimizer when running runYaps().
#' @noRd
getBounds <- function(datTmb) {
	lu_logD_xy 			<- c(-50,  2)
	lu_logD_z 			<- c(-50,  2)

	lu_logSigma_toa 	<- c(-12, -2)
	if(datTmb$Edist[2] == 1){ # mixture
		lu_logScale 	<- c(-30, 10)
	} else if (datTmb$Edist[3] == 1) { # t
		lu_logScale 	<- c(-10,2)
	}
	lu_log_t_part 		<- c(-100, 100)
	if(datTmb$pingType == 'rbi'){
		lu_logSigma_bi 		<- c(-20, 20)
	} else {
		lu_logSigma_bi 		<- c(-20, -2)
	}
	lu_logD_v 			<- c(-20,  2)

	bounds <- c()
	bounds <- rbind(bounds, lu_logD_xy)

	if(datTmb$how_3d == 'est'){
		bounds <- rbind(bounds, lu_logD_z)
	}

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
		bounds <- rbind(bounds, lu_logSigma_bi)
	} else if (datTmb$pingType == 'pbi'){
		bounds <- rbind(bounds, lu_logSigma_bi)
	}
	
	return(bounds)
}

