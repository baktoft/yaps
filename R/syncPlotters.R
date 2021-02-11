#' Plot residuals of sync_model to enable check of model
#' 
#' @param sync_model Synchronization model obtained using `getSyncModel()`
#' @param by What to facet/group the plot by? Currently supports one of 'overall', 'sync_tag', 'hydro', 'quantiles', 'temporal', 'temporal_hydro', 'temporal_sync_tag'
#' @export
#' @return No return value, called to plot graphics.
#' @example man/examples/example-syncModelPlots.R
plotSyncModelResids <- function(sync_model, by='overall'){
	eps_long <- sync_model$eps_long
	if(by == 'overall'){
		# print(quantile(eps_long$E_m, probs=c(0.01, 0.025, 0.05, 0.25, 0.5, 0.57, 0.95, 0.975, 0.99)))
		p <- ggplot2::ggplot(data=eps_long) + geom_histogram(aes(E_m), bins=50)
		p <- p + geom_vline(xintercept=c(quantile(eps_long$E_m, probs=c(0.01, 0.05, 0.95, 0.99))), col="red", lty=c(3,2,2,3))
		p <- p + xlab("Residual (m)")
	} else if(by == 'sync_tag'){
		p <- ggplot2::ggplot(data=eps_long) 
		p <- p + geom_violin(aes(x=factor(hydro_idx), y=E_m), scale="count") 
		p <- p + geom_vline(xintercept=seq(0, length(unique(eps_long$hydro_idx)), by=5), lty=3, col="red")
		p <- p + geom_hline(yintercept=0, col="red") 
		p <- p + facet_wrap(~sync_tag_idx)
		# p <- p + labs(title="Sync model residuals - facet by sync_tag_idx") + xlab("hydro_idx")
		p <- p + ylab("Residual (m)")
		p <- p + xlab("hydro_idx")
		p <- p + ggplot2::scale_x_discrete(breaks = pretty(unique(eps_long$hydro_idx)))
	} else if(by == 'hydro'){
		p <- ggplot2::ggplot(data=eps_long) + geom_violin(aes(x=factor(sync_tag_idx), y=E_m), scale="count") 
		p <- p + geom_hline(yintercept=0, col="red") + facet_wrap(~hydro_idx) 
		# p <- p + labs(title="Sync model residuals - facet by hydro_idx") + xlab("sync_tag_idx")
		p <- p + xlab("sync_tag_idx")
		p <- p + ylab("Residual (m)")  + ggplot2::scale_x_discrete(breaks = pretty(unique(eps_long$sync_tag_idx)))
	} else if(by == 'quantiles'){
		quants <- eps_long[, .(N=.N, q01=quantile(E_m, probs=c(0.01)), q05=quantile(E_m, probs=c(0.05)), q10=quantile(E_m, probs=c(0.10)), q25=quantile(E_m, probs=c(0.25)), q50=quantile(E_m, probs=c(0.50)), q75=quantile(E_m, probs=c(0.75)), q90=quantile(E_m, probs=c(0.90)), q95=quantile(E_m, probs=c(0.95)), q99=quantile(E_m, probs=c(0.99))), by=c('hydro_idx','sync_tag_idx')]
		thres <- 00
		out_quants <- quants[ abs(q10) > thres | abs(q90) > thres]
		p <- ggplot2::ggplot(data=out_quants) + geom_point(aes(x=hydro_idx, y=factor(sync_tag_idx), col=abs(q50), size=N))
		p <- p + viridis::scale_color_viridis(option="magma") + labs(y = "sync tag idx")
	} else if(by %in% c('temporal', 'temporal_hydro', 'temporal_sync_tag')){
		T0 <- sync_model$inp_synced$inp_params$T0
		offset_idx <- sync_model$inp_synced$dat_tmb_sync$offset_idx
		offset_levels <- sync_model$inp_synced$inp_params$offset_levels
		# tops <- sync_model$pl$TOP + sync_model$inp_synced$inp_params$T0
		tops <- as.POSIXct(sync_model$pl$TOP + offset_levels[offset_idx, 1], origin="1970-01-01", tz="UTC")
		eps_long[, top := tops[ping]]
		if(by == 'temporal_hydro'){
			p <- ggplot2::ggplot(data=eps_long) + ggtitle("by hydro") + geom_point(aes(x=top, y=E_m), pch=".") + geom_hline(data=eps_long[, .(mean_E_m=mean(E_m)), by=hydro_idx], aes(yintercept=mean_E_m), col="red") + facet_wrap(~hydro_idx)
		} else if(by == 'temporal_sync_tag'){
			p <- ggplot2::ggplot(data=eps_long) + ggtitle("by sync tag") + geom_point(aes(x=top, y=E_m), pch=".") + geom_hline(data=eps_long[, .(mean_E_m=mean(E_m)), by=sync_tag_idx], aes(yintercept=mean_E_m), col="red") + facet_wrap(~sync_tag_idx)
		} else if(by == 'temporal'){
			p <- ggplot2::ggplot(data=eps_long) + ggtitle("by hydro (col) X sync tag (row)") + geom_point(aes(x=top, y=E_m), pch=".") + geom_hline(data=eps_long[, .(mean_E_m=mean(E_m)), by=c('hydro_idx', 'sync_tag_idx')], aes(yintercept=mean_E_m), col="red") + facet_grid(sync_tag_idx~hydro_idx)
		}
	}
	
	suppressWarnings(print(p))
}

#' Plot hydrophone positions. Especially useful if some hydro re-positioned as part of the sync model.
#' @param sync_model Synchronization model obtained using `getSyncModel()`
#' @export
#' @return No return value, called to plot graphics.
#' @example man/examples/example-syncModelPlots.R
plotSyncModelHydros <- function(sync_model){
	z_synced <- NULL
	h_pos <- data.table::data.table(sync_model$inp_synced$dat_tmb_sync$H)
	colnames(h_pos) <- c('x','y','z')
	h_pos[, idx := 1:.N]
	h_pos[, x := x + sync_model$inp_synced$inp_params$Hx0]
	h_pos[, y := y + sync_model$inp_synced$inp_params$Hy0]
	
	h_pos[, x_synced := sync_model$pl$TRUE_H[,1]]
	h_pos[, y_synced := sync_model$pl$TRUE_H[,2]]
	h_pos[, z_synced := sync_model$pl$TRUE_H[,3]]
	
	cols <- ifelse(sync_model$inp_synced$dat_tmb_sync$fixed_hydros_vec == 1, "steelblue", "tomato")
	
	p1 <- ggplot2::ggplot(h_pos) + geom_point(aes(x=x,y=y), size=3) + geom_point(aes(x=x_synced, y=y_synced), col=cols) + ggrepel::geom_text_repel(aes(x=x_synced, y=y_synced, label=idx)) + coord_fixed(ratio=1)
	p2 <- ggplot2::ggplot(h_pos) + geom_point(aes(x=idx, y=x-x), size=3) + geom_point(aes(x=idx, y=x-x_synced), col=cols) + ylab("x-x_synced")
	p3 <- ggplot2::ggplot(h_pos) + geom_point(aes(x=idx, y=y-y), size=3) + geom_point(aes(x=idx, y=y-y_synced), col=cols) + ylab("y-y_synced") 
	
	return(cowplot::plot_grid(p1, p2, p3, ncol=1, rel_heights=c(5,1,1)))

}




#' Plot to check how well the sync model is working
#' 
#' Delta values indicate absolute difference between true and estimated distances based on pairwise relative distances to sync_tag. 
#' For instance, a ping from sync_tag t colocated with hydro Ht is detected by hydros H1 and H2. The pairwise relative distance to sync tag is then
#' delta = abs((true_dist(Ht, H1) - true_dist(Ht, H2)) - (est_dist(Ht, H1) - est_dist(Ht, H2)))
#'
#' @param sync_model Synchronization model obtained using `getSyncModel()`
#' @param by What to facet/group the plot by? Currently supports one of 'sync_bin_sync', 'sync_bin_hydro', 'sync_bin_sync_smooth', 'sync_bin_hydro_smooth', 'hydro', 'sync_tag'
#' @export
#' @return No return value, called to plot graphics.
#' @example man/examples/example-syncModelPlots.R
plotSyncModelCheck <- function(sync_model, by=""){
	sync_check_dat <- getSyncCheckDat(sync_model)
	
	# if(by == "ping"){
		# p <- ggplot2::ggplot(data=sync_check_dat) + geom_point(aes(x=ping_idx, y=delta), alpha=0.1, pch=20)
		# p <- p + geom_smooth(aes(x=ping_idx, y=delta, group=factor(focal_hydro_idx)), se=FALSE)
		# p <- p + xlab("ping")

	if(by == "sync_bin_sync"){
		plot_dat <- sync_check_dat[, .(med_delta=median((delta))), by=c('sync_tag_idx', 'focal_hydro_idx', 'hydro_idx', 'offset_idx')]
		p <- ggplot2::ggplot(data=plot_dat) + geom_violin(aes(x=factor(offset_idx), y=(med_delta)), scale="count", draw_quantiles=c(.25,.5, .75)) + facet_wrap(~sync_tag_idx)
		# p <- ggplot2::ggplot(data=plot_dat) + geom_boxplot(aes(x=factor(offset_idx), y=(med_delta))) + facet_wrap(~sync_tag_idx)
		p <- p + xlab("Sync period")
	} else if(by == "sync_bin_hydro"){
		plot_dat <- sync_check_dat[, .(med_delta=median((delta))), by=c('sync_tag_idx', 'focal_hydro_idx', 'hydro_idx', 'offset_idx')]
		p <- ggplot2::ggplot(data=plot_dat) + geom_violin(aes(x=factor(offset_idx), y=(med_delta)), scale="count")  + facet_wrap(~focal_hydro_idx)
		p <- p + xlab("Sync period")
	} else if(by == "sync_bin_sync_smooth"){
		plot_dat <- sync_check_dat[, .(med_delta=median((delta))), by=c('sync_tag_idx', 'focal_hydro_idx', 'hydro_idx', 'offset_idx')]
		p <- ggplot2::ggplot(data=plot_dat) + geom_smooth(aes(x=offset_idx, y=med_delta), method = "gam", formula = y ~ s(x, bs = "cs")) + facet_wrap(~sync_tag_idx)
		p <- p + xlab("Sync period")
	} else if(by == "sync_bin_hydro_smooth"){
		plot_dat <- sync_check_dat[, .(med_delta=median((delta))), by=c('sync_tag_idx', 'focal_hydro_idx', 'hydro_idx', 'offset_idx')]
		p <- ggplot2::ggplot(data=plot_dat) + geom_smooth(aes(x=offset_idx, y=med_delta), method = "gam", formula = y ~ s(x, bs = "cs")) + facet_wrap(~focal_hydro_idx)
		p <- p + xlab("Sync period")
	} else if(by == "sync_tag"){
		plot_dat <- sync_check_dat[, .(med_delta=median((delta))), by=c('sync_tag_idx', 'focal_hydro_idx', 'hydro_idx')]
		p <- ggplot2::ggplot(data=plot_dat) + geom_violin(aes(x=factor(focal_hydro_idx), y=(med_delta)), scale="count", draw_quantiles=c(.25,.5, .75)) + facet_wrap(~sync_tag_idx)
		p <- p + xlab('hydro_idx') + ggplot2::scale_x_discrete(breaks = pretty(unique(plot_dat$focal_hydro_idx)))
	} else if(by == "hydro"){
		plot_dat <- sync_check_dat[, .(med_delta=median((delta))), by=c('sync_tag_idx', 'focal_hydro_idx', 'hydro_idx')]
		p <- ggplot2::ggplot(data=plot_dat) + geom_violin(aes(x=factor(sync_tag_idx), y=med_delta), scale="count", draw_quantiles=c(.25,.5, .75)) + facet_wrap(~focal_hydro_idx)
		p <- p + xlab('sync_tag_idx') + ggplot2::scale_x_discrete(breaks = pretty(unique(plot_dat$sync_tag_idx)))
	}
	p <- p + geom_hline(yintercept=c(1,10,100), lty=3, col="red")
	p <- p + scale_y_log10()
	p <- p + ylab("log(delta) (log(m))")
	suppressWarnings(print(p))
}

