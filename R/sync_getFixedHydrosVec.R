
#' Internal function to get vector of which hydros are fixed
#' @inheritParams getInpSync
#' @noRd
#' @export
getFixedHydrosVec <- function(hydros, sync_params){
	
	if(sync_params$fixed_hydros[1] == "all"){
		fixed_hydros_vec <- rep(1, times=nrow(hydros))
	} else {
		fixed_hydros_vec <- rep(0, times=nrow(hydros))
		fixed_hydros_vec[hydros$h_sn %in% sync_params$fixed_hydros] <- 1
	}
	
	return(fixed_hydros_vec)
}

