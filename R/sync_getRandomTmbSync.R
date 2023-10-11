#' Internal function to get random params for TMB sync
#' @inheritParams getInpSync
#' @noRd
#' @export
getRandomTmbSync <- function(dat_tmb_sync, ss_vec){
	random_tmb_sync <- c("TOP")
	
	# random_tmb_sync <- c(random_tmb_sync, "OFFSET", "SLOPE1", "SLOPE2", "TRUE_H")
	random_tmb_sync <- c(random_tmb_sync, "OFFSET", "TRUE_H")
	
	if(ss_vec[1] == 0){
		random_tmb_sync <- c(random_tmb_sync, "SS")
	}
	return(random_tmb_sync)
}
