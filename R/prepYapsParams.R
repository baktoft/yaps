#' Internal function to replace yaps_params NAs with defaults and calculated values.
#' @noRd
#' @export
prepYapsParams <- function(yaps_params, ss_data, z_data){
	cat("Setting defaults and estimated values for non-specified yaps_params...\n")
	cat("...override these by specifying them in yaps_params before calling getInp()...\n")


	if(yaps_params$ping_type != 'rbi'){
		cat("ERROR: Only ping_type = 'rbi' is supported at the moment.\n")
		stopSilent()
	}

	if(is.null(yaps_params$ping_type)){
		cat("ERROR: ping_type must be specified in yaps_params!\n")
		stopSilent()
	}
	
	if(yaps_params$ping_type == 'rbi' & length(yaps_params$bi) != 2){
		cat("ERROR: When yaps_params$ping_type = 'rbi' yaps_params$bi must be a vector of min and max bi. \n")
		cat("...For instance yaps_params$bi <- c(60, 120) \n")
		stopSilent()
	}

	if(is.null(yaps_params$E_dist)){
		yaps_params$E_dist <- 'Mixture'
		cat("...E_dist = 'Mixture' \n")
	}

	valid_E_dist <- c('Mixture', 't', 'Gaus')
	if(!yaps_params$E_dist %in% valid_E_dist){
		cat("ERROR: Invalid error distribution selected. Can only be one of ",paste0(valid_E_dist, collapse =", "),"\n")
		cat("...If unsure, use yaps_params$E_dist <- 'Mixture'\n")
		stopSilent()
	}
	
	if(ss_data == 'none' & is.null(yaps_params$n_ss)){
		cat("...Setting n_ss = 1 \n")
		yaps_params$n_ss <- 1
	}
	
	if(is.null(yaps_params$bbox)){
		cat("...Setting bbox = TRUE to use standard bounding box \n")
		yaps_params$bbox <- TRUE
	}
	
	if(is.null(yaps_params$sd_inits)){
		cat("...Setting sd_inits = 1 \n")
		yaps_params$sd_inits <- 1
	}
	
	
	
	return(yaps_params)
}