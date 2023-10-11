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
getSyncModel <- function(inp_sync, silent=TRUE, max_iter=1000, plot=TRUE){
	inp_sync$inp_params$max_iter <- max_iter
	
	dat_tmb <- inp_sync$dat_tmb_sync
	params <- inp_sync$params_tmb_sync
	random <- inp_sync$random_tmb_sync
	inits <- inp_sync$inits_tmb_sync
	inp_params <- inp_sync$inp_params
	sync_params <- inp_sync$sync_params

	cat(paste0(Sys.time(), " \n"))
	cat(". Running optimization of the sync model. Please be patient - this can take a long time. \n")
	
	cat("..sync_params used:\n")
	cat("...time_keeper			= ", sync_params$time_keeper, "\n")
	cat("...trial_days			= ", sync_params$trial_days, "\n")
	cat("...E_dist			= ", sync_params$E_dist, "\n")
	cat("...keep_rate			= ", sync_params$keep_rate, "\n")
	cat("...n_per_day 			= ", sync_params$n_per_day, "\n")
	cat("...smooth_offsets 		= ", sync_params$smooth_offsets, "\n")
	cat("...fixed_hydros 		= ", sync_params$fixed_hydros, "\n")
	cat("...excl_self_detect		= ", sync_params$excl_self_detect, "\n")
	cat("...silent_check			= ", sync_params$silent_check, "\n")
	cat("...min_hydros			= ", sync_params$min_hydros, "\n")
	cat("...max_epo_diff			= ", sync_params$max_epo_diff, "\n")
	cat("...lin_corr			= ", ifelse(is.na(sync_params$lin_corr[1]), 'NA \n', 'linear correction coefs specified - run sync_params$lin_corr to print these\n'))
	cat("\n")
	
	opt <- c()
	pl <- c()
	plsd <- c()
	obj <- c()
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

	lower <- c(-10, -10)
	upper <- c(-2, 5)
	
	if(dat_tmb$ss_vec[1] == 0){
		lower <- c(lower, -5)
		upper <- c(upper, 10)
	}
	
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
		cat(".... some extreme outliers potentially affecting the model where identified \n Consider running fineTuneSyncModel(sync_model, eps_threshold=10000). See ?fineTuneSyncModel for more info. \n")
		# dat_tmb$toa_offset[crazy_outliers] <- NA
	}

	jointrep <- try(TMB::sdreport(obj, getJointPrecision=TRUE), silent=silent)
	param_names <- rownames(summary(jointrep))
	sds <- summary(jointrep)[,2]
	summ <- data.frame(param=param_names, sd=sds)
	plsd <- split(summ[,2], f=summ$param)
	
	pl$TRUE_H[,1] <- pl$TRUE_H[,1] + attr(inp_params$hydros, 'Hx0')
	pl$TRUE_H[,2] <- pl$TRUE_H[,2] + attr(inp_params$hydros, 'Hy0')
	eps_long <- getEpsLong(report, pl, inp_sync)
	
	dat_sync_cov <- getDatSyncCov(inp_sync)
	nas <- dat_sync_cov[N == 0]
	
	if(nrow(nas) > 0){
		for(i in 1:nrow(nas)){
			pl$OFFSET[inp_sync$inp_params$hydros[h_sn == nas[i, h_sn], h_idx], 	nas[i, offset_idx]] <- NA
		}
	}
	
	sync_model <- list(pl=pl, plsd=plsd, report=report, obj_val=obj_val, eps_long=eps_long, inp_synced=inp_sync)
	
	if(sync_params$smooth_offsets){
		tic <- Sys.time()
		cat(".... Getting smoothed offsets...\n")
		sync_model[['gams']] <- getSmoothOffsets(sync_model)
		Sys.time() - tic
		
		tic <- Sys.time()
		cat(".... Getting synced data for checking the sync model...\n")
		sync_model[['sync_check_dat']] <- getSyncCheckDat(sync_model)
		Sys.time() - tic
	} else {
		sync_model[['gams']] <- NA
	}
	
	
	
	
	cat("Sync model done \n")
	cat("Consider saving the sync model for later use - e.g. save(sync_model, file='path_to_sync_save'). \n")
	
	if(is.na(sync_model[['gams']][1])){
		cat("NOTE: Smoothed offsets not extracted. This can be done post hoc using sync_model[['gams']] <- getSmoothOffsets(sync_model) \n")
		cat("... You might also want to get synced data for checking he sync model. Run sync_model[['sync_check_dat']] <- getSyncCheckDat(sync_model) \n")
	}
	
	if(diff(range(pl$OFFSET[1:nrow(inp_sync$inp_params$hydros),], na.rm=TRUE)) > 50){
		cat("NOTE: Estimated offsets are seemingly rather large. \n ...Consider running:  sync_params$lin_corr <- getLinCorr(sync_model)  to obtain linear correction factors. \n ...Then rerun getInpSync() and getSyncModel() \n")
	}

	if(plot){ plotSyncModel(sync_model)	}

	return(sync_model)
}

