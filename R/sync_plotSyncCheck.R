#' Makes various plots to check sync_model
#' @export
plotSyncCheck <- function(sync_model, type=1){
	sync_check_dat <- sync_model$sync_check_dat
	hydros <- sync_model$inp_synced$inp_params$hydros
	
	tk <- sync_model$inp_synced$sync_params$time_keeper
	dat_sync_cov <- getDatSyncCov(sync_model$inp_synced)
	dat_sync_cov[, focal_hsn :=  as.numeric(levels(h_sn))[h_sn]]
	
	if(is.null(sync_check_dat[1])){
		cat("ERROR: Data for checking the sync is not available! Run sync_model[['sync_check_dat']] <- getSyncCheckDat(sync_model) to obtain these \n")
		stopSilent()
	}


	if(type==1){
		p1 <- ggplot2::ggplot(sync_check_dat) + geom_histogram(aes(x=delta), bins=50) + facet_wrap(~focal_hsn) + geom_vline(xintercept=0, col="red")
		return(p1)
	}

	if(type == 2){
		dat_plot2 <- sync_check_dat[, .(mean=mean(abs(delta), na.rm=TRUE), median=median(abs(delta), na.rm=TRUE), 
			q05=quantile(abs(delta), probs=c(0.05), na.rm=TRUE), q95=quantile(abs(delta), probs=c(0.95), na.rm=TRUE), 
			q25=quantile(abs(delta), probs=c(0.25), na.rm=TRUE), q75=quantile(abs(delta), probs=c(0.75), na.rm=TRUE)), by=.(focal_hsn, offset_idx)]
			
		p2 <- ggplot2::ggplot(dat_plot2, aes(x=offset_idx)) + 
			geom_line(aes(y=q05		), lty=3) + 
			geom_line(aes(y=q95		), lty=3) + 
			geom_line(aes(y=q25		), lty=2) + 
			geom_line(aes(y=q75		), lty=2) + 
			geom_line(aes(y=median 	), lty=1) + 
			facet_wrap(~focal_hsn) + labs(title="Sync deviance (m) per h_sn", y="Deviance (m)")
		p2 <- p2 + geom_point(data=dat_sync_cov[N==0], aes(y=0), col="red", size=4)
		p2 <- p2 + geom_point(data=dat_sync_cov[h_sn == tk & N==0, .(offset_idx)], aes(y=0), col="red", size=4)
		
		return(p2)
	}

	if(type == 3){
		dat_plot3 <- sync_check_dat[, .(mean=mean(abs(delta), na.rm=TRUE), median=median(abs(delta), na.rm=TRUE), 
			q05=quantile(abs(delta), probs=c(0.05), na.rm=TRUE), q95=quantile(abs(delta), probs=c(0.95), na.rm=TRUE), 
			q25=quantile(abs(delta), probs=c(0.25), na.rm=TRUE), q75=quantile(abs(delta), probs=c(0.75), na.rm=TRUE)), 	by=.(sync_tag, offset_idx)]
			
		p3 <- ggplot2::ggplot(dat_plot3) + 
				geom_line(aes(x=offset_idx, y=q25), lty=2) + 
				geom_line(aes(x=offset_idx, y=q75), lty=2) + 
				geom_line(aes(x=offset_idx, y=q05), lty=3) + 
				geom_line(aes(x=offset_idx, y=q95), lty=3) + 
				geom_line(aes(x=offset_idx, y=median), lty=1) + 
				facet_wrap(~sync_tag) + labs(title="Sync deviance (m) per sync_tag", y="Deviance (m)")
		p3 <- p3 + geom_point(data=dat_sync_cov[h_sn == tk & N==0, .(offset_idx)], aes(x=offset_idx, y=0), col="red", size=4)
		return(p3)
	}
	
	if(type == 4){
		dat_plot4 <- sync_check_dat[, .(mean=mean(abs(delta), na.rm=TRUE), median=median(abs(delta), na.rm=TRUE)), by=.(focal_hsn, sync_tag)]

		dat_plot4 <- merge(dat_plot4, hydros[, .(sync_tag, x_from=h_x, y_from=h_y)], by.x="sync_tag", by.y="sync_tag")
		dat_plot4 <- merge(dat_plot4, hydros[, .(h_sn, x_to=h_x, y_to=h_y)], by.x="focal_hsn", by.y="h_sn")

		p4 <- ggplot2::ggplot() 
		p4 <- p4 + geom_point(data=hydros, aes(x=h_x, y=h_y)) + coord_fixed(ratio=1) 
		p4 <- p4 + geom_segment(data=dat_plot4, aes(x=x_from, xend=x_to, y=y_from, yend=y_to, col=mean), lwd=1) 
		p4 <- p4 + viridis::scale_color_viridis(limits=c(0,max(dat_plot4$mean)), name="Mean dev\n(m)")
		p4 <- p4 + ggrepel::geom_label_repel(data=hydros, aes(x=h_x, y=h_y, label=paste0(h_sn,'\n',sync_tag)))
		return(p4)
	}
	
	if(type == 5){
		dat_plot5 <- sync_check_dat[, .(mean=mean(abs(delta), na.rm=TRUE), median=median(abs(delta), na.rm=TRUE)), by=.(focal_hsn, sync_tag, offset_idx)]
		
		p5 <- ggplot2::ggplot(sync_check_dat) + geom_boxplot(aes(x=factor(focal_hsn), y=abs(delta)+0.1), varwidth=TRUE) + 
			geom_hline(data=dat_plot5[, .(median=median(median)), by=.(sync_tag)], aes(yintercept=median), col="red", lty=2) + 
			facet_grid(sync_tag~.) + scale_y_continuous(trans='log10')
		
		return(p5)
	}
	
	if(type == 6){
		dat_plot6 <- sync_check_dat[, .(mean=mean(abs(delta), na.rm=TRUE), median=median(abs(delta), na.rm=TRUE)), by=.(focal_hsn, sync_tag, offset_idx)]
		p6 <- ggplot2::ggplot(dat_plot6) + geom_line(aes(x=offset_idx, y=median)) + 
			geom_hline(data=dat_plot6[, .(median=median(median)), by=.(focal_hsn, sync_tag)], aes(yintercept=median), col="red", lty=2) + 
			geom_hline(data=dat_plot6[, .(median=median(median))], aes(yintercept=median), col="blue", lty=2) + 
			facet_grid(focal_hsn ~ sync_tag)
		p6 <- p6 + geom_point(data=dat_sync_cov[N==0], aes(x=offset_idx, y=0), col="red", size=4)
		p6 <- p6 + geom_point(data=dat_sync_cov[h_sn == tk & N==0, .(offset_idx)], aes(x=offset_idx, y=0), col="red", size=4)

		return(p6)
	}

}