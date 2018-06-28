#' Function to run TMB to estimate track
#'
#' @param inp inp-object obtained from getInp()
#' @param maxIter Sets inner.control(maxit) of the tmb-call. Increase if model is not converging.
#' @param getPlsd,getRep Whether or not to get sd estimates (plsd=TRUE) and reported values (getRep=TRUE).
#' @export
runTmb <- function(inp, maxIter=1000, getPlsd=TRUE, getRep=TRUE){
	obj <- TMB::MakeADFun(
			data = inp$datTmb,
			parameters = inp$params,
			random = c("X", "Y", "ss", "top"),
			DLL = "yaps",
			inner.control = list(maxit = maxIter), 
			silent=FALSE
		)
	opt <- stats::nlminb(inp$inits,obj$fn,obj$gr)
	obj$fn()
	pl <- obj$env$parList()   # List of estimates
	if(getRep){
		rep<- obj$report()         # Report variables, just in case
	} else {rep <- c()}

	if(getPlsd){
		jointrep <- try(TMB::sdreport(obj, getJointPrecision=TRUE), silent=TRUE)
		param_names <- rownames(summary(jointrep))
		sds <- summary(jointrep)[,2]
		summ <- data.frame(param=param_names, sd=sds)
		plsd <- split(summ[,2], f=summ$param)
	} else {plsd <- c()}
	
	return(list(pl=pl, plsd=plsd, rep=rep))
}
