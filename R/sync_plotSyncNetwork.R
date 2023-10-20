#' Plot sync network
#' @export
plotSyncNetwork <- function(hydros, dat_sync){
	
	if(is.null(attr(hydros, 'yapsified'))){
		hydros <- yapsify(hydros)
	}
	
	sync_at_hydros <- dat_sync[, .N, by=.(h_sn, tag)]
	sync_at_hydros <- merge(sync_at_hydros, hydros[, .(sync_tag, x_from=h_x, y_from=h_y)], by.x="tag", by.y="sync_tag")
	sync_at_hydros <- merge(sync_at_hydros, hydros[, .(h_sn, x_to=h_x, y_to=h_y)], by="h_sn")

	p1 <- ggplot2::ggplot() 
	p1 <- p1 + geom_point(data=hydros, aes(x=h_x, y=h_y)) + coord_fixed(ratio=1) 
	p1 <- p1 + geom_segment(data=sync_at_hydros, aes(x=x_from, xend=x_to, y=y_from, yend=y_to, col=N), lwd=2) + viridis::scale_color_viridis(limits=c(0,max(sync_at_hydros$N)))
	p1 <- p1 + ggrepel::geom_label_repel(data=hydros, aes(x=h_x, y=h_y, label=paste0(h_sn,'\n',sync_tag)))
	p1
}