#' Graphical representation of spatial constraints
#' @inheritParams getInp
#' @export
#' @return No return value, called to plot graphic.
#' @example man/examples/example-bbox.R
plotBbox <- function(hydros, bbox){
	Var1 <- Var2 <- NULL
	softplus <- function(x, eps){return (0.5*(x+sqrt(x*x+eps*eps)))	}

	x_space_min <- floor(hydros[which.min(hydros$hx), hx] - 100)
	x_space_max <- ceiling(hydros[which.max(hydros$hx), hx] + 100)
	y_space_min <- floor(hydros[which.min(hydros$hy), hy] - 100)
	y_space_max <- ceiling(hydros[which.max(hydros$hy), hy] + 100)
	
	xs <- x_space_min:x_space_max
	ys <- y_space_min:y_space_max

	x_min <- bbox[1]
	x_max <- bbox[2]
	y_min <- bbox[3]
	y_max <- bbox[4]
	eps   <- bbox[5]
	pen   <- bbox[6]

	pen_mat <- (outer(xs,ys, FUN=function(i,j) {
		pen * (softplus(i - x_max, eps=eps) + softplus(x_min - i, eps=eps) +
			softplus(j - y_max, eps=eps) + softplus(y_min - j, eps=eps))
	}))


	pen_long <- data.table::data.table(reshape2::melt(pen_mat))
	pen_long[, x := xs[Var1]]
	pen_long[, y := ys[Var2]]
	pen_long[, pen := value]

	p <- ggplot2::ggplot(pen_long) + geom_tile(aes(x, y, fill=log(pen))) + 
		coord_fixed(ratio=1, xlim=range(c(xs, x_min, x_max)), ylim=range(c(ys, y_min, y_max))) + 
		viridis::scale_fill_viridis() + 
		geom_point(data=hydros, aes(x=hx, y=hy), col="red", size=2)
	print(p)
}

