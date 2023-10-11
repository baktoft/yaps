#' Internal function. yapsify / standarize hydros and detections for sync and yaps
#' @noRd
#' @export
yapsify <- function(yapsify_me){
	
	if(!is.null(attr(yapsify_me, 'yapsified'))){
		if(attr(yapsify_me, 'yapsified') == TRUE){
			cat("ERROR: Data already yapsified!\n")
			stopSilent()
		}
	}
	
	if(sum(c("x", "y", "z") %in% colnames(yapsify_me)) >= 1){ # seems to be a hydro data.table

		std_h <- yapsify_me[, c('h_sn', 'x','y','z','sync_tag')]
		setorder(std_h, h_sn)
		std_h[, h_idx := 1:.N]

		Hx0 <- std_h[1,x]
		Hy0 <- std_h[1,y]
		
		std_h[, x:=x-Hx0]
		std_h[, y:=y-Hy0]
		std_h[]
		
		attr(std_h, 'yapsified') <- TRUE
		attr(std_h, 'Hx0') <- Hx0
		attr(std_h, 'Hy0') <- Hy0
	
		return(std_h=std_h)
	} else if(sum(c("ts", "tag", "epo", "frac") %in% colnames(yapsify_me)) >= 1){ # should be a detection table
		std_dat <- yapsify_me[, c('ts','h_sn','tag','frac')]
		std_dat[, epo := floor(as.numeric(ts))]
		std_dat[, epofrac := epo + frac]
		
		attr(std_dat, 'yapsified') <- TRUE
		
		return(std_dat)
	}
}


