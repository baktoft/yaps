#' Get matrix of hydro-to-hydro distances
#' @export
getDistMat <- function(hydros){
	dist_mat <- matrix(ncol=nrow(hydros), nrow=nrow(hydros))
	rownames(dist_mat) <- hydros$h_sn
	colnames(dist_mat) <- hydros$h_sn
	for(hx in 1:nrow(hydros)){
		for(hy in 1:nrow(hydros)){
			dist_mat[hx, hy] <- sqrt((hydros[hx, x] - hydros[hy, x] )^2 + (hydros[hx, y] - hydros[hy, y] )^2 + (hydros[hx, z] - hydros[hy, z] )^2)
		}
	}
	return(dist_mat)
}
