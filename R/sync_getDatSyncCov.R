#' Get data for evaluating sync coverage...
#' @export
getDatSyncCov <- function(inp_sync){
	dat_sync_cov <- data.table(reshape2::melt(inp_sync$dat_tmb_sync$toa_offset), offset_idx = inp_sync$dat_tmb_sync$offset_idx)
	colnames(dat_sync_cov) <- c('ping_idx', 'h_sn', 'value', 'offset_idx')
	dat_sync_cov <- dat_sync_cov[!is.na(value)]
	dat_sync_cov[, h_sn := factor(h_sn)]
	dat_sync_cov[, offset_idx := factor(offset_idx)]
	setkey(dat_sync_cov, h_sn, offset_idx)
	dat_sync_cov <- dat_sync_cov[CJ(h_sn, offset_idx, unique = TRUE), .N, by = .EACHI]
	dat_sync_cov[, offset_idx := as.numeric(levels(offset_idx))[offset_idx]]
	
	return(dat_sync_cov)
}