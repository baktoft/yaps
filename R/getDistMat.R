#' Get matrix of hydro-to-hydro distances
#' @export
getDistMat <- function(hydros){
	dist_mat <- matrix(ncol=nrow(hydros), nrow=nrow(hydros))
	rownames(dist_mat) <- hydros$h_sn
	colnames(dist_mat) <- hydros$h_sn
	for(hx in 1:nrow(hydros)){
		for(hy in 1:nrow(hydros)){
			dist_mat[hx, hy] <- sqrt((hydros[hx, h_x] - hydros[hy, h_x] )^2 + (hydros[hx, h_y] - hydros[hy, h_y] )^2 + (hydros[hx, h_z] - hydros[hy, h_z] )^2)
		}
	}
	return(dist_mat)
}
