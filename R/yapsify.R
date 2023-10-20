#' Internal function. yapsify / standarize hydros and detections for sync and yaps
#' @export
yapsify <- function(yapsify_me){
	
	if(!is.null(attr(yapsify_me, 'yapsified'))){
		if(attr(yapsify_me, 'yapsified') == TRUE){
			cat("ERROR: Data already yapsified!\n")
			stopSilent()
		}
	}
	
	if(sum(c('h_sn', 'h_x','h_y','h_z','sync_tag') %in% colnames(yapsify_me)) != 5 & sum(c('ts','h_sn','tag','frac') %in% colnames(yapsify_me)) != 4){
		cat("ERROR: Data must contain the following one of the following combinations of colnames:
			...hydros: c('h_sn', 'h_x', 'h_y', 'h_z', 'sync_tag') 
			...detections: c('ts', 'h_sn', 'tag', 'frac')
			...You have colnames(yapsify_me) = ",paste0(colnames(yapsify_me), collapse=", ")	,"
			")
		stopSilent()
	}
	
	if(sum(c("h_x", "h_y", "h_z") %in% colnames(yapsify_me)) >= 1){ # seems to be a hydro data.table

		std_h <- yapsify_me[, c('h_sn', 'h_x','h_y','h_z','sync_tag')]
		setorder(std_h, h_sn)
		std_h[, h_idx := 1:.N]

		Hx0 <- std_h[1,h_x]
		Hy0 <- std_h[1,h_y]
		
		std_h[, h_x:=h_x-Hx0]
		std_h[, h_y:=h_y-Hy0]
		std_h[]
		
		attr(std_h, 'yapsified') <- TRUE
		attr(std_h, 'Hx0') <- Hx0
		attr(std_h, 'Hy0') <- Hy0
	
		return(std_h)
	} else if(sum(c("ts", "tag", "epo", "frac") %in% colnames(yapsify_me)) >= 1){ # should be a detection table
		std_dat <- yapsify_me[, c('ts','h_sn','tag','frac')]
		std_dat[, epo := floor(as.numeric(ts))]
		std_dat[, epofrac := epo + frac]
		
		attr(std_dat, 'yapsified') <- TRUE
		
		return(std_dat)
	}
}


