#' Plot residuals of sync_model to enable check of model
#' 
#' @param sync_model Synchronization model obtained using `getSyncModel()`
#' @param by What to facet/group the plot by? Currently supports one of 'overall', 'sync_tag', 'hydro', 'quantiles'
#' @export
plotSyncModelResids <- function(sync_model, by='overall'){
	eps_long <- sync_model$eps_long
	if(by == 'overall'){
		# print(quantile(eps_long$E_m, probs=c(0.01, 0.025, 0.05, 0.25, 0.5, 0.57, 0.95, 0.975, 0.99)))
		p <- ggplot2::ggplot(data=eps_long) + geom_histogram(aes(E_m), bins=50)
		p <- p + geom_vline(xintercept=c(quantile(eps_long$E_m, probs=c(0.01, 0.05, 0.95, 0.99))), col="red", lty=c(3,2,2,3))
		p <- p + xlab("Residual (m)")
	} else if(by == 'sync_tag'){
		p <- ggplot2::ggplot(data=eps_long) 
		p <- p + geom_violin(aes(x=factor(hydro_idx), y=E_m), scale="count", draw_quantiles=c(.25,.5, .75)) 
		p <- p + geom_vline(xintercept=seq(0, length(unique(eps_long$hydro_idx)), by=5), lty=3, col="red")
		p <- p + geom_hline(yintercept=0, col="red") + facet_wrap(.~sync_tag_idx) 
		# p <- p + labs(title="Sync model residuals - facet by sync_tag_idx") + xlab("hydro_idx")
		p <- p + ylab("Residual (m)")
		p <- p + xlab("hydro_idx") + ggplot2::scale_x_discrete(breaks = pretty(unique(eps_long$hydro_idx)))
	} else if(by == 'hydro'){
		p <- ggplot2::ggplot(data=eps_long) + geom_violin(aes(x=factor(sync_tag_idx), y=E_m), scale="count", draw_quantiles=c(.25,.5, .75)) 
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
	
	}
	
	suppressWarnings(print(p))
}

checkSyncModelResidQuantiles <- function(sync_model){
	eps_long <- sync_model$eps_long
	
	
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
		p <- ggplot2::ggplot(data=plot_dat) + geom_violin(aes(x=factor(offset_idx), y=(med_delta)), scale="count", draw_quantiles=c(.25,.5, .75))  + facet_wrap(~focal_hydro_idx)
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
	print(p)
}

