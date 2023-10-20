#' Get a standard bounding box to impose spatial constraints
#'
#' Standard is a rectangle based on coordinates of outer hydros +- the buffer in meters
#' @param buffer Number of meters the spatial domain extends beyound the outer hydros.
#' @param eps Specifies how well-defined the borders are (eps=1E-2 is very sharp, eps=100 is very soft). 
#' @param pen Specifies the penalty multiplier.
#' @inheritParams getInp
#' @export
#' @return Vector of lenght 6: c(x_min, x_max, y_min, y_max, eps, pen). Limits are given in UTM coordinates.
#' @example man/examples/example-bbox.R
getBbox <- function(hydros, buffer=100, eps=1E-3, pen=1E6){
	x_min <- hydros[which.min(hydros$h_x), h_x] - buffer
	x_max <- hydros[which.max(hydros$h_x), h_x] + buffer
	y_min <- hydros[which.min(hydros$h_y), h_y] - buffer
	y_max <- hydros[which.max(hydros$h_y), h_y] + buffer
	bbox <- c(x_min, x_max, y_min, y_max, eps, pen)
	return(bbox)
}
