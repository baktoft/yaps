#' Get prepared inp-object for use in TMB-call
#'
#' Wrapper-function to compile a list of input needed to run TMB
#' @param hydros Dataframe from simHydros() or Dataframe with columns hx and hy containing positions of the receivers. Translate the coordinates to get the grid centre close to (0;0).
#' @param toa TOA-matrix: matrix with receivers in rows and detections in columns. Make sure that the receivers are in the same order as in hydros, and that the matrix is very regular: one ping per column (inlude empty columns if a ping is not detected).
#' @param E_dist Which distribution to use in the model - "Gaus" = Gaussian, "Mixture" = mixture of Gaussian and t or "t" = pure t-distribution
#' @param n_ss Number of soundspeed estimates: one estimate per hour is usually enough
#' @param pingType Type of transmitter to simulate - either stable burst interval ('sbi'), random burst interval ('rbi') or random burst interval but where the random sequence is known a priori
#' @param rbi_min,rbi_max Minimum and maximum BI for random burst interval transmitters
#' @param sdInits If >0 initial values will be randomized around the normally fixed value using rnorm(length(inits), mean=inits, sd=sdInits)
#' @param ss_data_what What speed of sound (ss) data to be used. Default ss_data_what='est': ss is estimated by the model. Alternatively, if ss_data_what='data': ss_data must be provided and length(ss_data) == ncol(toa)
#' @param ss_data Vector of ss-data to be used if ss_data_what = 'est'. Otherwise ss_data <- 0 (default)
#' @param biTable Table of known burst intervals. Only used when pingType == "pbi". Default=NULL
#' @param z_vec Vector of known depth values (positive real). Default=NULL is which case no 3D is assumed. If z_vec = "est" depth will be estimated.
#' @param bbox Spatial constraints in the form of a bounding box. See ?getBbox for details.

#' @return List of input data ready for use in `runYaps()`
#' @export
#' @example man/examples/example-yaps_ssu1.R
getInp <- function(hydros, toa, E_dist, n_ss, pingType, sdInits=1, rbi_min=0, rbi_max=0, ss_data_what='est', ss_data=0, biTable=NULL, z_vec=NULL, bbox=NULL){
	stopifnot(pingType %in% c('sbi', 'pbi', 'rbi'))
	inp_params 	<- getInpParams(hydros, toa, pingType)
	datTmb 		<- getDatTmb(hydros, toa, E_dist, n_ss, pingType, rbi_min, rbi_max, ss_data_what, ss_data, biTable, inp_params, z_vec, bbox)
	params 		<- getParams(datTmb)
	inits 		<- getInits(datTmb, sdInits)
	return(list(
		datTmb = datTmb,
		params= params,
		inits = inits,
		inp_params = inp_params
		)
	)
}

