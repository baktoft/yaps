#' Get a standard bounding box to impose spatial constraints
#'
#' Standard is a rectangle based on coordinates of outer hydros +- the buffer in meters
#' Returns a vector of lenght 6: c(x_min, x_max, y_min, y_max, eps, pen). Limits are given in UTM coordinates.
#' @param buffer Number of meters the spatial domain extends beyound the outer hydros.
#' @param eps Specifies how well-defined the borders are (eps=1E-2 is very sharp, eps=100 is very soft). 
#' @param pen Specifies the penalty multiplier.
#' @inheritParams getInp
#' @export
getBbox <- function(hydros, buffer=5, eps=1E-3, pen=1){
	x_min <- hydros[which.min(hydros$hx), hx] - buffer
	x_max <- hydros[which.max(hydros$hx), hx] + buffer
	y_min <- hydros[which.min(hydros$hy), hy] - buffer
	y_max <- hydros[which.max(hydros$hy), hy] + buffer
	bbox <- c(x_min, x_max, y_min, y_max, eps, pen)
	return(bbox)
}
