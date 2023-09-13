#' Internal function. yapsify / standarize hydros for sync and yaps
#' @noRd
#' @export
yapsifyHydros <- function(hydros){
	Hx0 <- hydros[1,x]
	Hy0 <- hydros[1,y]

	std_h <- hydros[, c('h_sn', 'x','y','z','sync_tag')]
	setorder(std_h, h_sn)
	std_h[, h_idx := 1:.N]
	
	std_h[, x:=x-Hx0]
	std_h[, y:=y-Hy0]
	std_h[]
	
	attr(std_h, 'yapsified') <- TRUE
	attr(std_h, 'Hx0') <- Hx0
	attr(std_h, 'Hy0') <- Hy0
	
	return(std_h=std_h)
}


