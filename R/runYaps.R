#' Function to run TMB to estimate track
#'
#' @param inp inp-object obtained from getInp()
#' @param maxIter Sets inner.control(maxit) of the tmb-call. Increase if model is not converging.
#' @param getPlsd,getRep Whether or not to get sd estimates (plsd=TRUE) and reported values (getRep=TRUE).
#' @param silent Logical whether to keep the optimization quiet.
#' @export
runYaps <- function(inp, maxIter=1000, getPlsd=TRUE, getRep=TRUE, silent=TRUE){
	print("Running yaps...")
	random <- c("X", "Y", "top")
	if(inp$datTmb$ss_data_what == 'est'){
		random <- c(random, 'ss')
	}

	if(inp$datTmb$pingType == 'pbi'){
		random <- c(random, "tag_drift")
	}

	obj <- TMB::MakeADFun(
			data = inp$datTmb,
			parameters = inp$params,
			random = random,
			DLL = "yaps",
			inner.control = list(maxit = maxIter), 
			silent=silent
		)
	if(!silent){
		opt <- stats::nlminb(inp$inits,obj$fn,obj$gr)
	} else {
		suppressWarnings(opt <- stats::nlminb(inp$inits,obj$fn,obj$gr))
	}
	
	pl <- obj$env$parList()   # List of estimates
	if(getRep){
		rep<- obj$report()         # Report variables, just in case
	} else {rep <- c()}

	if(getPlsd){
		jointrep <- try(TMB::sdreport(obj, getJointPrecision=TRUE), silent=silent)
		if(!silent){
			param_names <- rownames(summary(jointrep))
		} else {
			param_names <- suppressWarnings(rownames(summary(jointrep)))
		}
		if(!silent){
			sds <- summary(jointrep)[,2]
		} else {
			sds <- suppressWarnings(summary(jointrep)[,2])
		}
		summ <- data.frame(param=param_names, sd=sds)
		plsd <- split(summ[,2], f=summ$param)
	} else {plsd <- c()}
	
	obj_out <- obj$fn()
	if(is.na(obj_out) | is.null(opt$convergence)) {
		print("...yaps failed to converge!. Rerun getInp() to get new starting values and try again.")
	} else {
		print(paste0("...yaps converged (obj: ",obj_out,") with message: ",opt$message,""))
	}
	return(list(pl=pl, plsd=plsd, rep=rep, obj=obj_out, inp=inp))
}


#' @rdname runYaps
#' @export
runTmb <- runYaps
