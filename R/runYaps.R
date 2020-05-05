#' Function to run TMB to estimate track
#'
#' @param inp inp-object obtained from getInp()
#' @param maxIter Sets inner.control(maxit) of the tmb-call. Increase if model is not converging.
#' @param getPlsd,getRep Whether or not to get sd estimates (plsd=TRUE) and reported values (getRep=TRUE).
#' @param silent Logical whether to keep the optimization quiet.
#' @param opt_fun Which optimization function to use. Default is 'nlminb' - alternative is 'nloptr' (experimental!). If using 'nloptr', `opt_controls` must be specified. 
#' @param opt_controls List of controls passed to optimization function. For instances, tolerances such as x.tol=1E-8. If opt_fun = 'nloptr', `opt_controls` must be a list formatted appropriately. For instance: opt_controls <- list(algorithm="NLOPT_LD_AUGLAG", xtol_abs=1e-12, maxeval=2E+4, print_level = 1, local_opts= list(algorithm="NLOPT_LD_AUGLAG_EQ", xtol_rel=1e-4) ). See `?nloptr` and the NLopt site https://nlopt.readthedocs.io/en/latest/ for more info. Some algorithms in `nloptr` require bounded parameters - see `bounds`.
#' @param bounds List of two vectors specifying lower and upper bounds of fixed parameters. Length of each vector must be equal to number of fixed parameters. For instance, bounds = list(lb = c(-3, -1, -2), ub = c(2,0,1) ).

#' @export
runYaps <- function(inp, maxIter=1000, getPlsd=TRUE, getRep=TRUE, silent=TRUE, opt_fun='nlminb', opt_controls=list(), bounds=list()){
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

	
	if(opt_fun == 'nloptr'){
		opts <- opt_controls
		if(length(bounds) == 0){
			opt <- nloptr::nloptr(inp$inits,obj$fn,obj$gr,opts=opts,...=NA)
		} else {
			opt <- nloptr::nloptr(inp$inits,obj$fn,obj$gr,opts=opts,...=NA, lb=bounds$lb, ub=bounds$ub)
		}
		
	} else if(opt_fun == 'nlminb'){
		control_list <- opt_controls
		
		if(!silent){
			opt <- stats::nlminb(inp$inits,obj$fn,obj$gr, control = control_list)
		} else {
			suppressWarnings(opt <- stats::nlminb(inp$inits,obj$fn,obj$gr, control = control_list))
		}
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
	if(opt_fun == 'nloptr'){
		conv_status <- opt$status
		conv_message <- opt$message
		
		print(paste0("...yaps converged (obj: ",obj_out,") with status message: status=",conv_status, " - ", conv_message,""))
	} else if(opt_fun == 'nlminb'){
		if(is.na(obj_out) | is.null(opt$convergence)) {
			print("...yaps failed to converge!. Rerun getInp() to get new starting values and try again.")
		} else {
			conv_status <- opt$convergence
			conv_message <- opt$message

			print(paste0("...yaps converged (obj: ",obj_out,") with message: ",conv_message,""))
		}
	}
	return(list(pl=pl, plsd=plsd, rep=rep, obj=obj_out, inp=inp, conv_status=conv_status, conv_message=conv_message))
}


#' @rdname runYaps
#' @export
runTmb <- runYaps
