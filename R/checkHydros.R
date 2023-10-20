#' Internal function to check hydros data.tables
#' @export
checkHydros <- function(hydros){
	if(is.null(attr(hydros, 'yapsified'))){
		cat("ERROR: The hydros data.table is not yapsified. Run e.g. hydros <- yapsify(hydros) to do so. \n")
		stopSilent()
	}
	
	if(!is.null(attr(hydros, 'yapsified')) & attr(hydros, 'yapsified') == FALSE){
		cat("ERROR: The hydros data.table is not yapsified. Run e.g. hydros <- yapsify(hydros) to do so. \n")
		stopSilent()
	}
	
	if(length(unique(hydros$h_sn)) != nrow(hydros)){
		cat("ERROR: At least one hydrophone serial number is used more than once in sync_dat$hydros!\n")
		stopSilent()
	}


	
}