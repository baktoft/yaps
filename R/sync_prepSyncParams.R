#' Internal function to replaces sync_params NAs with defaults and calculated values to sync_params.
#' @noRd
#' @export
prepSyncParams <- function(hydros, dat_sync, sync_params){
	
	cat("Setting defaults and estimated values for non-specified sync_params...\n")
	cat("...override these by specifying them in sync_params before calling getInpSync()...\n")
	
	if(is.null(sync_params$smooth_offsets)){
		sync_params$smooth_offsets <- TRUE
		cat("...smooth_offsets = TRUE \n")
	}
	
	if(is.null(sync_params$trial_days)){
		sync_params$trial_days <- NA
		cat("...trial_days = NA \n")
	}
	
	if(is.null(sync_params$lin_corr)){
		sync_params$lin_corr <- NA
		cat("...lin_corr = NA \n")
	}
	
	if(is.null(sync_params$fixed_hydros)){
		sync_params$fixed_hydros <- 'all'
		cat("...fixed_hydros = 'all' \n")
	}
	
	if(is.null(sync_params$n_per_day)){
		sync_params$n_per_day <- 1
		cat("...n_per_day = 1 \n")
	}
	
	if(is.null(sync_params$keep_rate)){
		sync_params$keep_rate <- 50
		cat("...keep_rate = 50 \n")
	}
	
	if(is.null(sync_params$excl_self_detect)){
		sync_params$excl_self_detect <- TRUE
		cat("...excl_self_detect = TRUE \n")
	}
	
	if(is.null(sync_params$silent_check)){
		sync_params$silent_check <- FALSE
		cat("...silent_check = FALSE \n")
	}
	
	if(is.null(sync_params$min_hydros)){
		sync_params$min_hydros <- 3
		cat("...min_hydros = 3 \n")
	}
	
	if(is.null(sync_params$E_dist)){
		sync_params$E_dist <- 't'
		cat("...E_dist = 't' \n")
	}
	
	# estimate max_epo_diff if not specified
	if(is.null(sync_params$max_epo_diff)){
		epodiffs <- dat_sync[, .(epodiff = diff(epofrac)), by=.(h_sn, tag)]$epodiff
		sync_params$max_epo_diff <- round(median(epodiffs) * 0.4, 1)
		cat("...max_epo_diff = ",sync_params$max_epo_diff," \n")
	}
	
	return(sync_params)

}
