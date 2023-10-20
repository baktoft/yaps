#' Get bounds restricting the optimizer
#* 
#' Compile a matrix of lower (bounds[,1]) and upper (bounds[,2]) bounds for the parameters to be estimated.
#' @param datTmb Object obtained using getDatTmb()
#' @return Matrix of bounds restricting the optimizer when running runYaps().
#' @noRd
getBounds <- function(dat_tmb) {
	lu_logD_xy 			<- c(-50,  2)
	lu_logD_z 			<- c(-50,  2)

	lu_logSigma_toa 	<- c(-12, -2)
	if(dat_tmb$E_dist_vec[2] == 1){ # mixture
		lu_logScale 	<- c(-30, 10)
	} else if (dat_tmb$E_dist_vec[3] == 1) { # t
		lu_logScale 	<- c(-10,2)
	}
	lu_log_t_part 		<- c(-100, 100)
	if(dat_tmb$ping_type == 'rbi'){
		lu_logSigma_bi 		<- c(-20, 20)
	} else {
		lu_logSigma_bi 		<- c(-20, -2)
	}
	lu_logD_v 			<- c(-20,  2)

	bounds <- c()
	bounds <- rbind(bounds, lu_logD_xy)

	if(dat_tmb$how_3d == 'est'){
		bounds <- rbind(bounds, lu_logD_z)
	}

	if(dat_tmb$ss_data == 'none'){
		bounds <- rbind(bounds, lu_logD_v)
	}

	if(dat_tmb$E_dist_vec[1] == 1){
		bounds <- rbind(bounds, lu_logSigma_toa)
	} else if(dat_tmb$E_dist_vec[2] == 1){
		bounds <- rbind(bounds, lu_logSigma_toa, lu_logScale, lu_log_t_part)
	} else if(dat_tmb$E_dist_vec[3] == 1){
		bounds <- rbind(bounds, lu_logScale)
	}
	
	if(dat_tmb$ping_type == 'sbi'){
		bounds <- rbind(bounds, lu_logSigma_bi)
	} else if (dat_tmb$ping_type == 'rbi'){
		bounds <- rbind(bounds, lu_logSigma_bi)
	} else if (dat_tmb$ping_type == 'pbi'){
		bounds <- rbind(bounds, lu_logSigma_bi)
	}
	
	return(bounds)
}

