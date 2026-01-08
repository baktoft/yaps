#' Wrapper function to doRunYaps()
#'
#' @param inp input
#' @param silent input
#'
#' @export

runYaps <- function(inp, silent, best_of=1, max_iter=1000){

	if(best_of == 1){
		yaps_out <- doRunYaps(inp, silent, max_iter) 
	} else {
		yaps_out_list <- list()
		cat(paste0("Running yaps to get best of ", best_of, "\n"))
		for(i in 1:best_of){
			# resample inits and params to get different starting point
			inp$inits <- yaps:::getInits(inp$dat_tmb, inp$yaps_params)
			inp$params <- yaps:::getParams(inp$dat_tmb)
			
			
			yaps_out_list[[i]] <- doRunYaps(inp, silent, max_iter)
		}
		
		objs <- sapply(yaps_out_list, `[`, 'obj')
		best_i <- which.min(objs)
		
		yaps_out <- yaps_out_list[[best_i]]
		
	}
	
	return(yaps_out)
}


