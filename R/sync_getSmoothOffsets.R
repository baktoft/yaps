#' Get smoothed sync offsets from sync_model
#' @export
getSmoothOffsets <- function(sync_model, E_smooth = "gaus", k_div=2){
	cat("Smoothing estimated sync offsets...\n")
	# gams <- list(length = sync_model$inp_synced$dat_tmb$nh)
	# for(h in 1:sync_model$inp_synced$dat_tmb$nh){
		# cat(".",h,".")
		# offsets_h <- data.table(offset = sync_model$pl$OFFSET[h,], epofrac = rowMeans(sync_model$inp_synced$inp_params$offset_levels))
		# if(E_smooth == "t"){
			# m <- mgcv::gam(offset ~ s(epofrac, k=nrow(offsets_h[!is.na(offset)])), data=offsets_h, family="scat")
		# } else {
			# m <- mgcv::gam(offset ~ s(epofrac, k=nrow(offsets_h[!is.na(offset)])), data=offsets_h)
		# }
		# gams[[h]] <- m
		
		# if(h %% 10 == 0){ cat("\n")}
	# }

	offset_mat <- sync_model$pl$OFFSET
	epofrac <- rowMeans(sync_model$inp_synced$inp_params$offset_levels)
	# gnu <- apply(offset_mat, 1, FUN=function(k) {
		# # dat_k <- k
		# # m_k <- mgcv::gam(offset ~ s(epofrac_vec, k=floor(sum(!is.na(dat_k))/10)), data=data.frame(offset=dat_k))
		# m_k <- mgcv::gam(k ~ s(epofrac_vec, k=floor(sum(!is.na(k))/10)))
	# })
	# tic <- Sys.time()
	n_cores <- parallel::detectCores()-1
	cat("...running parallel using ",n_cores," cores\n")
	cl <- parallel::makeCluster(n_cores)
	parallel::clusterExport(cl, c("epofrac", "k_div"), envir=environment())
	gams <- parallel::parApply(cl, offset_mat, 1, FUN=function(f) {
		m_k <- mgcv::gam(f ~ s(epofrac, k=floor(sum(!is.na(f))/k_div)))
		# floor(sum(!is.na(f))/k_div)
	})
	parallel::stopCluster(cl)
	
	# Sys.time() - tic

	
	# resids <- lapply(seq_along(gams), function(i) data.table(E=resid(gams[[i]]), hsn = i))
	# resids <- do.call(rbind, resids)
	
	# # # # this is not entirely correct - doesn't account for NA offset_idxs
	# resids[, offset_idx := 1:.N, by=hsn]
	
	# ggplot(resids) + geom_line(aes(x=offset_idx, y=E*1450, group=hsn)) + facet_wrap(~hsn)
	
	# hist(resids$E * 1450)
	
	cat("\n")
	return(gams)
}