#' Get sync model from inp_sync object obtained by `getInpSync()`
#' 
#' @param inp_sync Input data prepared for the sync model using `getInpSync()`
#' @param silent Keep TMB quiet
#' @param fine_tune Logical. Whether to re-run the sync model excluding residual outliers. **Deprecated** use fineTuneSyncModel() instead.
#' @param max_iter Max number of iterations to run TMB. Default=100 seems to work in most cases.
#' @param tmb_smartsearch Logical whether to use the TMB smartsearch in the inner optimizer (see `?TMB::MakeADFun` for info). Default and original implementation is TRUE. However, there seems to be an issue with some versions of `Matrix` that requires `tmb_smartsearch=FALSE`. 
#' 
#' @export
#' @return List containing relevant data constituting the `sync_model` ready for use in `fineTuneSyncModel()` if needed or in `applySync()`
#' @example man/examples/example-yaps_ssu1.R
getSyncModel <- function(inp_sync, silent=TRUE, fine_tune=FALSE, max_iter=100, tmb_smartsearch=TRUE){
	inp_sync$inp_params$tmb_smartsearch <- tmb_smartsearch
	inp_sync$inp_params$max_iter <- max_iter
	
	dat_tmb <- inp_sync$dat_tmb_sync
	params <- inp_sync$params_tmb_sync
	random <- inp_sync$random_tmb_sync
	inits <- inp_sync$inits_tmb_sync
	inp_params <- inp_sync$inp_params

	cat(paste0(Sys.time(), " \n"))
	cat(". Running optimization of the sync model. Please be patient - this can take a long time. \n")
	if(fine_tune){cat(".... fine tuning is enabled, but is getting deprecated in future version. Consider to use the function fineTuneSyncModel() instead. See ?fineTuneSyncModel for info. \n")}

	tictoc::tic("Fitting sync model: ")
	opt <- c()
	pl <- c()
	plsd <- c()
	obj <- c()

	tictoc::tic()
	obj <- c()
	opt <- c()
	report <- c()
	gc()
	
	# config(DLL="yaps_sync")
	# ## Reduce memory peak of a parallel model by creating tapes in serial
	# config(tape.parallel=0, DLL="yaps_sync")
	obj <- TMB::MakeADFun(data = dat_tmb, parameters = params, random = random, DLL = "yaps", inner.control = list(maxit = max_iter), silent=silent)
	obj$fn(obj$par) 
	
	if(!silent){
		obj$env$tracepar = TRUE
		obj$env$tracemgc = TRUE
	}

	
	if(!tmb_smartsearch){
		TMB::newtonOption(obj, smartsearch=FALSE)
	}

	lower <- c(-10)
	upper <- c(-2)
	
	if(silent){
		# opt <- suppressWarnings(stats::nlminb(inits,obj$fn,obj$gr))
		opt <- suppressWarnings(stats::nlminb(inits,obj$fn,obj$gr, lower=lower, upper=upper))
	} else {
		opt <- stats::nlminb(inits,obj$fn,obj$gr, lower=lower, upper=upper)
		# opt <- stats::nlminb(inits,obj$fn,obj$gr)
	}

	pl <- obj$env$parList()   # List of estimates
	obj_val <- opt$objective
	cat(paste0(".. ", Sys.time()), " \n")
	cat(".... obj = ", obj_val, " \n")
	report <- obj$report()

	crazy_outliers <- which(abs(report$eps)*1450 > 10000)
	fine_outliers  <- which(abs(report$eps)*1450 > 1000)
	if(length(crazy_outliers > 0)){
		cat(".... some extreme outliers potentially affecting the model where identified \n Consider to run fineTuneSyncModel(sync_model, eps_threshold=10000). See ?fineTuneSyncModel for more info. \n")
		# dat_tmb$toa_offset[crazy_outliers] <- NA
	} else if(fine_tune){
		cat(".... fine tuning is enabled, but is deprecated. Use the function fineTuneSyncModel() instead. See ?fineTuneSyncModel for info. \n")
		# dat_tmb$toa_offset[fine_outliers] <- NA
	} 

	tictoc::toc()

	jointrep <- try(TMB::sdreport(obj, getJointPrecision=TRUE), silent=silent)
	param_names <- rownames(summary(jointrep))
	sds <- summary(jointrep)[,2]
	summ <- data.frame(param=param_names, sd=sds)
	plsd <- split(summ[,2], f=summ$param)
	
	pl$TRUE_H[,1] <- pl$TRUE_H[,1] + inp_params$Hx0
	pl$TRUE_H[,2] <- pl$TRUE_H[,2] + inp_params$Hy0
	eps_long <- getEpsLong(report, pl, inp_sync)
	
	offset_nas <- which(pl$OFFSET == 0)
	pl$OFFSET[offset_nas] <- NA
	pl$SLOPE1[offset_nas] <- NA
	pl$SLOPE2[offset_nas] <- NA
	
	cat("Sync model done \n")
	cat("Consider saving the sync model for later use - e.g. save(sync_model, file='path_to_sync_save'). \n")

	return(list(pl=pl, plsd=plsd, report=report, obj_val=obj_val, eps_long=eps_long, inp_synced=inp_sync))
}

