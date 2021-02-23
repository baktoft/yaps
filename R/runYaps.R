#' Function to run TMB to estimate track
#'
#' @param inp inp-object obtained from `getInp()`
#' @param maxIter Sets `inner.control(maxit)` of the TMB-call. Increase if model is not converging.
#' @param getPlsd,getRep Whether or not to get sd estimates (plsd=TRUE) and reported values (getRep=TRUE).
#' @param silent Logical whether to keep the optimization quiet.
#' @param opt_fun Which optimization function to use. Default is `opt_fun = 'nlminb'` - alternative is `opt_fun = 'nloptr'` (experimental!). If using nloptr, `opt_controls` must be specified. 
#' @param opt_controls List of controls passed to optimization function. For instances, tolerances such as `x.tol=1E-8`. \cr 
#'   If `opt_fun = 'nloptr'`, `opt_controls` must be a list formatted appropriately. For instance: \cr
#'  `opt_controls <- list( algorithm="NLOPT_LD_AUGLAG", xtol_abs=1e-12, maxeval=2E+4, print_level = 1, local_opts= list(algorithm="NLOPT_LD_AUGLAG_EQ", xtol_rel=1e-4) )`. \cr
#'  See `?nloptr` and the NLopt site https://nlopt.readthedocs.io/en/latest/ for more info. Some algorithms in `nloptr` require bounded parameters - this is not currently implemented.
#' @param tmb_smartsearch Logical whether to use the TMB smartsearch in the inner optimizer (see `?TMB::MakeADFun` for info). Default and original implementation is TRUE. However, there seems to be an issue with recent versions of `Matrix` that requires `tmb_smartsearch=FALSE`. 
#'
#' @export
#'
#' @return List containing results of fitting `yaps` to the data.
#'  \describe{
#'    \item{pl}{List containing all parameter estimates.}
#'    \item{plsd}{List containing standard errors of parameter estimates.}
#'    \item{rep}{List containing `mu_toa`.}
#'    \item{obj}{Numeric obj value of the fitted model obtained using `obj$fn()`.}
#'    \item{inp}{List containing the `inp` object used in `runYaps()`. See `?getInp` for further info.}
#'    \item{conv_status}{Integer convergence status.}
#'    \item{conv_message}{Text version of convergence status.}
#'    \item{track}{A data.table containing the estimated track including time-of-ping (top), standard errors and number of hydros detecting each ping (nobs).}
#'  }
#'
#' @example man/examples/example-yaps_ssu1.R
runYaps <- function(inp, maxIter=1000, getPlsd=TRUE, getRep=TRUE, silent=TRUE, opt_fun='nlminb', opt_controls=list(), tmb_smartsearch=TRUE){
	
	# making sure inp is correct...
	checkInp(inp)
	
	nobs <- z <- z_sd <- NULL
	print("Running yaps...")
	random <- c("X", "Y", "top")
	if(inp$datTmb$how_3d == "est"){
		random <- c(random, "Z")
	}	
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
	
	# Attempt to robustify the inner optim problem.
	# Refuse to optimize if gradient is too steep. Default is 1E60
	# TMB::newtonOption(obj, mgcmax=1E8)

	if(!silent){
		obj$env$tracepar = TRUE
		obj$env$tracemgc = TRUE
	}
	
	if(!tmb_smartsearch){
		obj$fn(obj$par) 
		TMB::newtonOption(obj, smartsearch=FALSE)
	}

	# if(	!is.null(opt_controls[['use_bounds']])){
		# lower <- opt_controls[['lower']]
		# upper <- opt_controls[['upper']]
		# opt_controls <- list()
	# } else {
		# lower <- -Inf
		# upper <- Inf
	# }
	
	if(opt_fun == 'nloptr'){
		opts <- opt_controls
		# if(length(bounds) == 0){
			# opt <- nloptr::nloptr(inp$inits,obj$fn,obj$gr,opts=opts,...=NA)
		# } else {
			# opt <- nloptr::nloptr(inp$inits,obj$fn,obj$gr,opts=opts,...=NA, lb=inp$bounds[,1], ub=inp$bounds[,2])
		# }
		opt <- nloptr::nloptr(inp$inits,obj$fn,obj$gr,opts=opts,...=NA)#, lb=inp$bounds[,1], ub=inp$bounds[,2])
		
	} else if(opt_fun == 'nlminb'){
		control_list <- opt_controls
		if(!silent){
				# opt <- stats::nlminb(inp$inits,obj$fn,obj$gr, control = control_list)
				# opt <- stats::nlminb(inp$inits,obj$fn,obj$gr, control = control_list, lower=c(-50,-15, -100, -50, -20), upper= c(2, 2, 100, 2, -2))
				opt <- stats::nlminb(inp$inits,obj$fn,obj$gr, control = control_list, lower=inp$bounds[,1], upper=inp$bounds[,2])
		} else {
			suppressWarnings(opt <- stats::nlminb(inp$inits,obj$fn,obj$gr, control = control_list, lower=inp$bounds[,1], upper=inp$bounds[,2]))
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
	
	# extract track in user friendly format
	track <- data.table::data.table(
		top=as.POSIXct(pl$top + inp$inp_params$T0, origin="1970-01-01", tz="UTC"), top_sd=plsd$top,
		x=pl$X+inp$inp_params$Hx0, y=pl$Y+inp$inp_params$Hy0, 
		x_sd=plsd$X, y_sd=plsd$Y)
	if(inp$datTmb$how_3d == 'est'){
		track[, z := pl$Z]
		track[, z_sd := plsd$Z]
	} else if(inp$datTmb$how_3d == 'data'){
		track[, z := inp$datTmb$z_vec]
		track[, z_sd := NA]
	} else {
		track[, z:=NA]
		track[, z_sd:=NA]
	}
	
	
	track[, nobs := apply(inp$datTmb$toa, 2, function(k) sum(!is.na(k)))]
	
	track <- track[, c('top', 'x', 'y', 'z', 'top_sd', 'x_sd', 'y_sd', 'z_sd', 'nobs')]
	
	return(list(pl=pl, plsd=plsd, rep=rep, obj=obj_out, inp=inp, conv_status=conv_status, conv_message=conv_message, track=track))
}


#' @rdname runYaps
#' @export
runTmb <- runYaps
