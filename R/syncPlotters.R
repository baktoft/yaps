#' Plot residuals of sync_model to enable check of model
plotSyncModelResids <- function(sync_model, by='overall'){
	eps_long <- sync_model$eps_long
	if(by == 'overall'){
		print(quantile(eps_long$E_m, probs=c(0.01, 0.025, 0.05, 0.25, 0.5, 0.57, 0.95, 0.975, 0.99)))
		p <- ggplot2::ggplot(data=eps_long) + geom_histogram(aes(E_m), bins=50)
		p <- p + geom_vline(xintercept=c(0, quantile(eps_long$E_m, probs=c(0.01, 0.05, 0.95, 0.99))), col="red", lty=c(1,3,2,2,4))
	} else if(by == 'sync_tag'){
		p <- ggplot2::ggplot(data=eps_long) + geom_boxplot(aes(x=factor(hydro_idx), y=E_m)) 
		p <- p + geom_hline(yintercept=0, col="red") + facet_grid(.~sync_tag_idx) 
		p <- p + labs(title="Sync model residuals - facet by sync_tag_idx") + xlab("hydro_idx")
	} else if(by == 'hydro'){
		p <- ggplot2::ggplot(data=eps_long) + geom_boxplot(aes(x=factor(sync_tag_idx), y=E_m)) 
		p <- p + geom_hline(yintercept=0, col="red") + facet_wrap(~hydro_idx) 
		p <- p + labs(title="Sync model residuals - facet by hydro_idx") + xlab("sync_tag_idx")
	}
	
	print(p)
}


#' Plot to check how well the sync model is working. 
#' Delta values indicate absolute difference between true and estimated distances based on pairwise relative distances to sync_tag. 
#' For instance, a ping from sync_tag t colocated with hydro Ht is detected by hydros H1 and H2. The pairwise relative distance to sync tag is then
#' delta = abs((true_dist(Ht, H1) - true_dist(Ht, H2)) - (est_dist(Ht, H1) - est_dist(Ht, H2)))
plotSyncModelCheck <- function(sync_model, by="ping"){
	sync_check_dat <- getSyncCheckDat(sync_model)
	
	if(by == "ping"){
		p <- ggplot(data=sync_check_dat) + geom_smooth(aes(x=ping_idx, y=delta, col=factor(focal_hydro_idx)), se=FALSE)
		p <- p + xlab(ping)
	} else if(by == "sync_tag"){
		plot_dat <- sync_check_dat[, .(med_delta=median((delta))), by=c('sync_tag_idx', 'focal_hydro_idx', 'hydro_idx')]
		p <- ggplot(data=plot_dat) + geom_boxplot(aes(x=factor(focal_hydro_idx), y=med_delta)) + facet_grid(.~sync_tag_idx)
		p <- p + xlab('sync_tag_idx')
	} else if(by == "hydro"){
		plot_dat <- sync_check_dat[, .(med_delta=median((delta))), by=c('sync_tag_idx', 'focal_hydro_idx', 'hydro_idx')]
		p <- ggplot(data=plot_dat) + geom_boxplot(aes(x=factor(sync_tag_idx), y=med_delta)) + facet_wrap(~focal_hydro_idx)
		p <- p + xlab('hydro_idx')
	}
	p <- p + ylab("Delta (m)")
	print(p)
}

